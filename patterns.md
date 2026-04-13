# GHC 9.8.4 Rollback Patterns

Patterns for migrating from **staging** back to **ghc9.8.4** branch.

**Direction**: `staging` → `ghc9.8.4` (rollback)

## Usage

### Text-based Migration (Recommended)
```bash
# Dry run to preview changes
./text-migration.sh --dry-run

# Apply changes to all packages
./text-migration.sh

# Apply to specific package only
./text-migration.sh ecPrelude

# Set custom path
EULER_API_TXNS=/path/to/repo ./text-migration.sh
```

### Cabal File Migration
```bash
python3 cabal-migration.py /path/to/euler-api-txns --dry-run
python3 cabal-migration.py /path/to/euler-api-txns
```

## Package Structure

The migration handles 6 packages with multiple source directories:

| Package | Source Directories |
|---------|-------------------|
| ecPrelude | src, src-generated, src-extras, src-javascript |
| dbTypes | src, src-extras, src-generated |
| euler-api-decider | src |
| euler-x | src, src-extras, src-generated, test |
| oltp | src, src-generated, src-extras |
| euler-api-txns (src/) | src, app, test, benchmark |

## Migration Patterns

### 1. Cryptonite → Crypton
**staging** → **ghc9.8.4**

```haskell
-- staging:
import "cryptonite" Crypto.Cipher.AES (AES128)

-- ghc9.8.4:
import "crypton" Crypto.Cipher.AES (AES128)
```

### 2. RecordDotPreprocessor Addition
**staging** → **ghc9.8.4**

```haskell
-- staging: (no pragma)

-- ghc9.8.4:
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
```

Cabal files:
```cabal
-- staging: (no plugin)

-- ghc9.8.4:
ghc-options: -fplugin=RecordDotPreprocessor
```

### 3. Nau.Runtime deriveGenerics Addition
**staging** → **ghc9.8.4**

```haskell
-- staging:
import qualified Nau.Runtime as Nau (HasCallStack, largeRecord)

-- ghc9.8.4:
import qualified Nau.Runtime as Nau (HasCallStack, largeRecord, deriveGenerics)
```

### 4. Generic Deriving for Beam Types
**staging** → **ghc9.8.4**

```haskell
-- staging:
data TableT (f :: Type -> Type) = TableT
  { col :: B.C f Text
  }

-- ghc9.8.4:
data TableT (f :: Type -> Type) = TableT
  { col :: B.C f Text
  } deriving stock (Generic)
```

### 5. Encoding Utils Consolidation
**staging** → **ghc9.8.4**

```haskell
-- staging:
import Data.Aeson (defaultOptions, genericParseJSON, genericToJSON)
import qualified Data.Aeson as Aeson

-- ghc9.8.4:
import Utils.Encoding (defaultDecode, defaultEncode)
-- Or:
import Euler.Utils.Commons (defaultDecode, defaultEncode)
```

### 6. DB.Types._getCurrentDate Removal
**staging** → **ghc9.8.4**

```haskell
-- staging:
import DB.Types (Date, _getCurrentDate)

-- ghc9.8.4:
import DB.Types (Date)
```

### 7. Compiler Plugin Disabling
**staging** → **ghc9.8.4**

Comment out or remove these plugins:
- `sheriff`
- `fdep`
- `fieldInspector`
- `warner`
- `paymentFlow`
- `endpoints`
- `FunctionInstrumentation.Plugin`

### 8. Cereal TH Addition
**staging** → **ghc9.8.4**

```haskell
-- staging: (nothing)

-- ghc9.8.4:
import Data.Cereal.TH (makeCereal)
makeCereal ''TypeName
```

### 9. Optimization Level Change
**staging** → **ghc9.8.4**

```cabal
-- staging:
ghc-options: -O1

-- ghc9.8.4:
ghc-options: -O0
```

### 10. Package Renames
**staging** → **ghc9.8.4**

```cabal
-- staging:
, crypton
, crypton-x509
, crypton-x509-store
, crypton-x509-validation

-- ghc9.8.4:
, cryptonite
, x509
, x509-store
, x509-validation
```

### 11. TypeApplications Extension
**staging** → **ghc9.8.4**

```cabal
-- staging: TypeApplications not listed (relies on default)

-- ghc9.8.4:
default-extensions:
    TypeApplications
```

### 12. Qualified Prelude.error Removal
**staging** → **ghc9.8.4**

```haskell
-- staging:
import qualified Prelude (error)

-- ghc9.8.4:
-- (removed - uses standard error from Prelude)
```

## Backup Strategy

All modified files are backed up with `.bak` suffix. To restore:

```bash
# Restore a single file
mv file.hs.bak file.hs

# Restore all backups in a package
find ecPrelude -name "*.bak" -exec sh -c 'mv "$1" "${1%.bak}"' _ {} \;

# Restore all packages
for pkg in ecPrelude dbTypes euler-api-decider euler-x oltp src; do
    find "$pkg" -name "*.bak" -exec sh -c 'mv "$1" "${1%.bak}"' _ {} \; 2>/dev/null || true
done
```

## Manual Review Checklist

After running automated migration, manually review:

1. **Beam table types** - Ensure `deriving stock (Generic)` is added correctly
2. **makeCereal splices** - May need manual insertion after data type declarations
3. **Encoding utils** - Verify imports match actual function usage
4. **Plugin interactions** - Some disabled plugins may affect runtime behavior
5. **Type applications** - Verify explicit type application sites compile

## Known Limitations

1. **AST-based approach**: Not usable due to GHC version mismatch (haskell-tools uses GHC 8.10, euler-api-txns uses GHC 9.6+)
2. **makeCereal insertion**: Text-based migration identifies files that may need it but cannot reliably determine exact insertion points
3. **Qualified imports**: Complex qualified import transformations may need manual adjustment
4. **CPP conditionals**: Files with heavy use of `#ifdef` may need manual review
