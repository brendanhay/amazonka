-- |
-- Module      : Amazonka.Prelude
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- An intentionally limited set of prelude exports to control backward
-- compatibility and simplify code generation.
--
-- Please consider long and hard before adding any addtional types exports to
-- this module - they should either be in pervasive use throughout the project
-- or have zero ambiguity. If you ever are forced to disambiguate at any point,
-- it's a bad export.
--
-- Try and avoid any value, operator, or symbol exports, if possible. Most of
-- the ones here exist to ease legacy code-migration.
module Amazonka.Prelude
  ( module Export,
    TextLazy,
    TextBuilder,
    ByteStringLazy,
    ByteStringBuilder,
  )
where

import Control.Applicative as Export (Alternative ((<|>)))
import Control.DeepSeq as Export (NFData (rnf))
import Control.Exception as Export (Exception, SomeException)
-- import Control.Lens as Export
--   ( Iso',
--     Lens',
--     Prism',
--     Setter',
--     Traversal',
--   )

import Control.Monad as Export
import Control.Monad.IO.Class as Export (MonadIO (liftIO))
import Control.Monad.Trans.Class as Export (MonadTrans (lift))
import Control.Monad.Trans.Resource as Export (MonadResource)
import Data.Bifoldable as Export
import Data.Bifunctor as Export
import Data.Bitraversable as Export
import Data.ByteString as Export (ByteString)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.CaseInsensitive as Export (CI)
import Data.Coerce as Export (Coercible, coerce)
import Data.Function as Export ((&))
import Data.Functor as Export ((<&>))
import Data.Functor.Identity as Export (Identity (..))
import Data.HashMap.Strict as Export (HashMap)
import Data.HashSet as Export (HashSet)
import Data.Hashable as Export (Hashable (hash, hashWithSalt))
import Data.Int as Export (Int16, Int32, Int64, Int8)
import Data.Kind as Export (Type)
import Data.List.NonEmpty as Export (NonEmpty ((:|)))
import Data.Maybe as Export
import Data.Monoid as Export (First)
import Data.Proxy as Export (Proxy (Proxy))
import Data.Scientific as Export (Scientific)
import Data.String as Export (IsString (fromString))
import Data.Text as Export (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Time as Export (Day, DiffTime, NominalDiffTime, UTCTime)
import Data.Void as Export (Void)
import Data.Word as Export (Word16, Word32, Word64, Word8)
import GHC.Exts as Export (IsList (..))
import GHC.Generics as Export (Generic)
import GHC.TypeLits as Export (KnownNat, KnownSymbol, Nat, Symbol)
import Lens.Micro as Export
  ( ASetter',
    Lens',
  )
import Lens.Micro.Pro as Export
  ( Iso',
    Prism',
    Traversal',
  )
import Numeric.Natural as Export (Natural)
import Prelude as Export

type TextLazy = Text.Lazy.Text

type TextBuilder = Text.Lazy.Builder.Builder

type ByteStringLazy = ByteString.Lazy.ByteString

type ByteStringBuilder = ByteString.Builder.Builder
