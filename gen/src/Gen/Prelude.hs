-- | An intentionally limited set of prelude exports.
--
-- Please consider long and hard before adding any addtional types exports to
-- this module - they should either be in pervasive use throughout the project
-- or have zero ambiguity. If you ever are forced to disambiguate at any point,
-- it's a bad export.
--
-- Attempt to avoid non-Prelude base exports if possible, unless pervasive
-- and unambiguous.
--
-- Avoid non-base function, operator, or value-level exports.
module Gen.Prelude
  ( -- * Text
    Text,
    TextLazy,
    TextBuilder,

    -- * Bytes
    ByteString,
    ByteStringLazy,
    ByteBuilder,

    -- * Errors
    hush,
    note,

    -- * Re-exports
    module Export,
  )
where

import Control.Applicative as Export (Alternative ((<|>)))
import Control.Comonad as Export (Comonad)
import Control.Comonad.Cofree as Export (Cofree (..))
import Control.Exception as Export (Exception, SomeException)
import Control.Lens as Export (Getter, Lens', (%=), (%~), (.~), (<>~), (?~), (^.), (^..), (^?))
import Control.Monad as Export
import Control.Monad.Except as Export
  ( Except,
    ExceptT (ExceptT),
    MonadError,
  )
import Control.Monad.IO.Class as Export (MonadIO (liftIO))
import Control.Monad.Reader as Export
  ( MonadReader,
    Reader,
    ReaderT (ReaderT),
  )
import Control.Monad.State.Strict as Export (MonadState, State, StateT (StateT))
import Control.Monad.Trans as Export (MonadTrans (lift))
import Control.Monad.Trans.Identity as Export (IdentityT (IdentityT))
import Control.Monad.Trans.Maybe as Export (MaybeT (MaybeT))
import Data.Aeson as Export
  ( FromJSON,
    FromJSONKey,
    ToJSON,
    ToJSONKey,
  )
import Data.Bifoldable as Export (Bifoldable (bifoldMap), bifor_, bitraverse_)
import Data.Bifunctor as Export (Bifunctor (bimap, first, second))
import Data.Bitraversable as Export (Bitraversable (bitraverse), bifor)
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import Data.ByteString.Short as Export (ShortByteString)
import Data.CaseInsensitive as Export (CI)
import Data.Coerce as Export (Coercible)
import Data.Either as Export
import Data.Foldable as Export (foldl', for_, traverse_)
import Data.Function as Export ((&))
import Data.Functor as Export (($>), (<&>))
import Data.Functor.Const as Export (Const (Const), getConst)
import Data.Functor.Contravariant as Export (Contravariant (contramap))
import Data.Functor.Identity as Export (Identity (Identity), runIdentity)
import Data.HashMap.Strict as Export (HashMap)
import Data.HashSet as Export (HashSet)
import Data.Hashable as Export (Hashable (hashWithSalt))
import Data.IORef as Export (IORef)
import Data.Int as Export (Int16, Int32, Int64, Int8)
import Data.IntSet as Export (IntSet)
import Data.List.NonEmpty as Export (NonEmpty (..))
import Data.Map.Strict as Export (Map)
import Data.Maybe as Export
import Data.Proxy as Export (Proxy (Proxy))
import Data.Scientific as Export (Scientific)
import Data.Set as Export (Set)
import Data.String as Export (IsString (fromString))
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import Data.Time as Export (Day, DiffTime, NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX as Export (POSIXTime)
import Data.Traversable as Export (for)
import Data.Typeable as Export (Typeable)
import Data.Void as Export (Void)
import Data.Word as Export (Word16, Word32, Word64, Word8)
import GHC.Exts as Export (Constraint, IsList (fromList, toList))
import GHC.Generics as Export (Generic)
import GHC.Stack as Export (HasCallStack)
import GHC.TypeLits as Export (KnownNat, KnownSymbol, Nat, Symbol)
import Numeric.Natural as Export (Natural)
import System.FilePath as Export ((<.>), (</>))
import Prelude as Export hiding
  ( foldl,
    head,
    init,
    last,
    lines,
    log,
    lookup,
    product,
    sum,
    tail,
  )

type Text = Data.Text.Text

type TextLazy = Data.Text.Lazy.Text

type TextBuilder = Data.Text.Lazy.Builder.Builder

type ByteString = Data.ByteString.ByteString

type ByteStringLazy = Data.ByteString.Lazy.ByteString

type ByteBuilder = Data.ByteString.Builder.Builder

-- | Suppress the 'Left' value of an 'Either'.
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Tag the 'Nothing' value of a 'Maybe'.
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
