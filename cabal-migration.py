#!/usr/bin/env python3
"""
GHC 9.8.4 Rollback Script - Cabal File Migration
Transforms .cabal files from STAGING back to GHC 9.8.4 patterns
"""

import re
import sys
import argparse
from pathlib import Path
from typing import List, Tuple


class CabalRollback:
    """Handles rollback of cabal files from staging to ghc9.8.4"""

    def __init__(self, content: str):
        self.content = content
        self.changes = []

    def rollback_crypton_to_cryptos(self) -> 'CabalRollback':
        """Replace crypton with cryptonite in build-depends (ROLLBACK)"""
        original = self.content

        # Replace crypton packages with cryptonite equivalents (ROLLBACK DIRECTION)
        replacements = [
            (r'(?<![-\w])crypton-x509(?![-\w])', 'x509'),
            (r'(?<![-\w])crypton-x509-store(?![-\w])', 'x509-store'),
            (r'(?<![-\w])crypton-x509-validation(?![-\w])', 'x509-validation'),
            (r'(?<![-\w])crypton(?![-\w])', 'cryptonite'),  # Must be last
        ]

        for pattern, replacement in replacements:
            self.content = re.sub(pattern, replacement, self.content)

        if self.content != original:
            self.changes.append("Replaced crypton with cryptonite packages")

        return self

    def add_record_dot_preprocessor(self) -> 'CabalRollback':
        """ADD RecordDotPreprocessor plugin to ghc-options (ROLLBACK)"""
        original = self.content

        # Check if already has RecordDotPreprocessor
        if 'RecordDotPreprocessor' in self.content:
            return self

        # Add -fplugin=RecordDotPreprocessor to ghc-options
        # Look for ghc-options in library or executable sections
        self.content = re.sub(
            r'(library\s*\n|executable\s+\w+\s*\n|common\s+\w+\s*\n)',
            r'\1  ghc-options: -fplugin=RecordDotProcessor\n',
            self.content
        )

        if self.content != original:
            self.changes.append("Added RecordDotPreprocessor plugin")

        return self

    def change_optimization_level_down(self) -> 'CabalRollback':
        """Change -O1 to -O0 for Local builds (ROLLBACK)"""
        original = self.content

        # Find Local build and change -O1 to -O0
        local_pattern = r'(flag\s+Local.*?\n)(.*?)(ghc-options:.*?-O)1'
        self.content = re.sub(local_pattern, r'\1\2\30', self.content,
                             flags=re.DOTALL | re.MULTILINE | re.IGNORECASE)

        if self.content != original:
            self.changes.append("Changed optimization level from -O1 to -O0")

        return self

    def add_type_applications(self) -> 'CabalRollback':
        """ADD TypeApplications to default-extensions (ROLLBACK)"""
        original = self.content

        # Check if already has TypeApplications
        if 'TypeApplications' in self.content:
            return self

        # Add TypeApplications to default-extensions or extensions
        self.content = re.sub(
            r'(default-extensions:\s*\n?)',
            r'\1    TypeApplications\n',
            self.content
        )

        if self.content != original:
            self.changes.append("Added TypeApplications extension")

        return self

    def disable_compiler_plugins(self) -> 'CabalRollback':
        """DISABLE compiler plugins: sheriff, fdep, fieldInspector, warner, paymentFlow, endpoints, FunctionInstrumentation.Plugin"""
        original = self.content

        plugins_to_disable = [
            'sheriff', 'fdep', 'fieldInspector', 'warner',
            'paymentFlow', 'endpoints', 'FunctionInstrumentation.Plugin'
        ]

        for plugin in plugins_to_disable:
            # Comment out plugin in ghc-options
            pattern = rf'(-fplugin={plugin}\b)'
            self.content = re.sub(pattern, r'-- \1', self.content)

        if self.content != original:
            self.changes.append("Disabled compiler plugins")

        return self

    def get_result(self) -> Tuple[str, List[str]]:
        """Return transformed content and list of changes"""
        return self.content, self.changes


def migrate_cabal_file(filepath: Path, dry_run: bool = False) -> None:
    """Migrate a single cabal file"""
    print(f"Processing: {filepath}")

    with open(filepath, 'r') as f:
        content = f.read()

    migrator = CabalRollback(content)
    new_content, changes = (migrator
        .rollback_crypton_to_cryptos()
        .add_record_dot_preprocessor()
        .change_optimization_level_down()
        .add_type_applications()
        .disable_compiler_plugins()
        .get_result())

    if not changes:
        print("  No changes needed")
        return

    print(f"  Changes: {', '.join(changes)}")

    if dry_run:
        print("  (Dry run - not writing changes)")
        return

    # Create backup
    backup_path = str(filepath) + '.bak'
    with open(backup_path, 'w') as f:
        f.write(content)

    with open(filepath, 'w') as f:
        f.write(new_content)
    print(f"  Written: {filepath}")
    print(f"  Backup: {backup_path}")


def find_cabal_files(directory: Path) -> List[Path]:
    """Find all .cabal files in directory recursively"""
    return list(directory.rglob('*.cabal'))


def main():
    parser = argparse.ArgumentParser(
        description='Rollback cabal files from staging to ghc9.8.4 patterns'
    )
    parser.add_argument(
        'path',
        nargs='?',
        default='.',
        help='Path to cabal file or directory containing cabal files'
    )
    parser.add_argument(
        '--dry-run',
        '-n',
        action='store_true',
        help='Show changes without writing them'
    )
    parser.add_argument(
        '--recursive',
        '-r',
        action='store_true',
        default=True,
        help='Process directories recursively (default: True)'
    )

    args = parser.parse_args()
    path = Path(args.path)

    if path.is_file() and path.suffix == '.cabal':
        migrate_cabal_file(path, args.dry_run)
    elif path.is_dir():
        files = find_cabal_files(path)

        if not files:
            print(f"No .cabal files found in {path}")
            sys.exit(1)

        print(f"Found {len(files)} cabal files\n")
        for cabal_file in files:
            migrate_cabal_file(cabal_file, args.dry_run)
    else:
        print(f"Invalid path: {path}")
        sys.exit(1)


if __name__ == '__main__':
    main()
