{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Network.AWS.Pager
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pager
    ( AWSPager     (..)
    , AWSTruncated (..)
    , stop
    , choice
    ) where

import           Control.Applicative
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.Maybe            (isJust, fromMaybe)
import           Data.Text             (Text)
import           Network.AWS.Data.Text (ToText (..))
import           Network.AWS.Lens      (Getter, to)
import           Network.AWS.Types

-- | Specify how an 'AWSRequest' and it's associated 'Rs' response can
-- generate a subsequent request, if available.
class AWSRequest a => AWSPager a where
    page :: a -> Rs a -> Maybe a

-- | Generalise IsTruncated and other optional/required
-- response pagination fields.
class AWSTruncated a where
    truncated :: a -> Bool

instance AWSTruncated Bool where
    truncated = id

instance AWSTruncated [a] where
    truncated = not . null

instance AWSTruncated (HashMap k v) where
    truncated = not . Map.null

instance {-# OVERLAPPABLE #-} AWSTruncated (Maybe a) where
    truncated = isJust

instance {-# OVERLAPS #-} AWSTruncated (Maybe Bool) where
    truncated = fromMaybe False

stop :: AWSTruncated a => a -> Bool
stop = not . truncated

choice :: (Alternative f, ToText a, ToText b)
       => (s -> f a)
       -> (s -> f b)
       -> Getter s (f Text)
choice f g = to $ \x -> (toText <$> f x) <|> (toText <$> g x)
