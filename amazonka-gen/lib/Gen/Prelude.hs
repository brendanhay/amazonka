-- | An intentionally limited set of prelude exports.
--
-- Please consider long and hard before adding any addtional types exports to
-- this module - they should either be in pervasive use throughout the project
-- or have zero ambiguity. If you ever are forced Lens.to disambiguate at any point,
-- it's a bad export.
--
-- Try and avoid any value, operator, or symbol exports. Most of the ones here
-- (such as Lens) exist Lens.to ease legacy code-migration.
module Gen.Prelude
  ( module Export,
    module Gen.Prelude,
  )
where

import Control.Applicative as Export (Alternative ((<|>)))
import Control.Comonad.Cofree as Export (Cofree ((:<)))
import Control.Exception as Export (Exception, SomeException)
import Control.Lens as Export
  ( Lens,
    Lens',
    Prism,
    Prism',
    Traversal,
    Traversal',
    (%~),
    (.~),
    (<>~),
    (^.),
    (?~),
    (^..),
    (%=),
    (^?),
    Getter,
  )
import Control.Monad as Export (join, void, (<=<), (>=>), when, unless)
import Control.Monad.Except as Export ( Except, ExceptT (ExceptT), MonadError)
import Control.Monad.IO.Class as Export (MonadIO (liftIO))
import Control.Monad.Reader as Export ( MonadReader, Reader, ReaderT (ReaderT))
import Control.Monad.State.Strict as Export (MonadState, State, StateT (StateT))
import Control.Monad.Trans as Export (MonadTrans (lift))
import Data.Aeson as Export ( FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifoldable as Export (Bifoldable)
import Data.Bifunctor as Export (Bifunctor (bimap, first, second))
import Data.Bitraversable as Export (Bitraversable)
import Data.ByteString as Export (ByteString)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.CaseInsensitive as Export (CI)
import Data.Either as Export
import Data.Function as Export ((&))
import Data.Functor as Export ((<&>))
import Data.Functor.Contravariant as Export (Contravariant)
import Data.Functor.Identity as Export (Identity (..))
import Data.Graph as Export (Graph, Vertex)
import Data.HashMap.Strict as Export (HashMap)
import Data.HashSet as Export (HashSet)
import Data.Hashable as Export (Hashable)
import Data.Int as Export (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty as Export (NonEmpty ((:|)))
import Data.Maybe as Export
import Data.Proxy as Export (Proxy (Proxy))
import Data.Scientific as Export (Scientific)
import Data.String as Export (IsString (fromString))
import Data.Text as Export (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Time as Export (Day, DiffTime, UTCTime)
import Data.Tree as Export (Forest, Tree)
import Data.Typeable as Export (Typeable)
import Data.Void as Export (Void)
import Data.Word as Export (Word16, Word32, Word64, Word8)
import GHC.Generics as Export (Generic)
import GHC.TypeLits as Export (KnownNat, KnownSymbol, Nat, Symbol)
import Numeric.Natural as Export (Natural)
import Prelude as Export

type LazyText = Text.Lazy.Text

type LazyByteString = ByteString.Lazy.ByteString

type ByteStringBuilder = ByteString.Builder.Builder

type TextBuilder = Text.Lazy.Builder.Builder
