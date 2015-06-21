{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

-- Module      : Network.AWS.Pagers
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Pagers
    ( AWSPager     (..)
    , AWSTruncated (..)
    , stop
    , select
    , choice
    ) where

import           Control.Applicative
import           Control.Lens          hiding (index)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.Text             (Text)
import           Network.AWS.Data.Text (ToText (..))
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

instance AWSTruncated (Maybe Bool) where
    truncated (Just x) = x
    truncated Nothing  = False

instance AWSTruncated (Maybe Text) where
    truncated (Just _) = True
    truncated Nothing  = False

instance AWSTruncated [a] where
    truncated = not . null

instance AWSTruncated (HashMap k v) where
    truncated = not . Map.null

stop :: AWSTruncated a => a -> Bool
stop = not . truncated

select :: Getter [a] (Maybe a)
select = to lastMay

choice :: (Alternative f, ToText a, ToText b)
       => (s -> f a)
       -> (s -> f b)
       -> Getter s (f Text)
choice f g = to $ \x -> (toText <$> f x) <|> (toText <$> g x)

-- choice :: Alternative f => (a -> f b) -> (a -> f b) -> a -> f b
-- choice f g x = f x <|> g x

lastMay :: [a] -> Maybe a
lastMay []     = Nothing
lastMay (x:xs) = Just (go x xs)
  where
    go y []     = y
    go _ (y:ys) = go y ys
