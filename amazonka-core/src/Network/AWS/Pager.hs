-- |
-- Module      : Network.AWS.Pager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pager
  ( AWSPager (..),
    AWSTruncated (..),
    stop,
    choice,
  )
where

import Control.Applicative
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Network.AWS.Data.Text (ToText (..))
import Network.AWS.Lens (Getter, to)
import Network.AWS.Prelude
import Network.AWS.Types

-- | Specify how an 'AWSRequest' and it's associated 'Rs' response can
-- generate a subsequent request, if available.
class AWSRequest a => AWSPager a where
  page :: a -> AWSResponse a -> Maybe a

-- | Generalise IsTruncated and other optional/required
-- response pagination fields.
class AWSTruncated a where
  truncated :: a -> Bool

instance AWSTruncated Bool where
  truncated = id
  {-# INLINEABLE truncated #-}

instance AWSTruncated [a] where
  truncated = not . null
  {-# INLINEABLE truncated #-}

instance AWSTruncated (HashMap k v) where
  truncated = not . HashMap.null
  {-# INLINEABLE truncated #-}

instance {-# OVERLAPPABLE #-} AWSTruncated (Maybe a) where
  truncated = Maybe.isJust
  {-# INLINEABLE truncated #-}

instance {-# OVERLAPS #-} AWSTruncated (Maybe Bool) where
  truncated = Maybe.fromMaybe False
  {-# INLINEABLE truncated #-}

stop :: AWSTruncated a => a -> Bool
stop = not . truncated
{-# INLINEABLE stop #-}

choice ::
  (Alternative f, ToText a, ToText b) =>
  (s -> f a) ->
  (s -> f b) ->
  Getter s (f Text)
choice f g = to $ \x -> (toText <$> f x) <|> (toText <$> g x)
{-# INLINEABLE choice #-}
