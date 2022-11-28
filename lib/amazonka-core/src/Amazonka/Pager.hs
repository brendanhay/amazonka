-- |
-- Module      : Amazonka.Pager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Pager
  ( AWSPager (..),
    AWSTruncated (..),
    stop,
    choice,
  )
where

import Amazonka.Core.Lens.Internal (Getter, to)
import Amazonka.Data (ToText (..))
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.HashMap.Strict as HashMap

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

instance AWSTruncated [a] where
  truncated = not . null

instance AWSTruncated (HashMap k v) where
  truncated = not . HashMap.null

instance {-# OVERLAPPABLE #-} AWSTruncated (Maybe a) where
  truncated = isJust

instance {-# OVERLAPS #-} AWSTruncated (Maybe Bool) where
  truncated = fromMaybe False

stop :: AWSTruncated a => a -> Bool
stop = not . truncated

choice ::
  (Alternative f, ToText a, ToText b) =>
  (s -> f a) ->
  (s -> f b) ->
  Getter s (f Text)
choice f g = to $ \x -> (toText <$> f x) <|> (toText <$> g x)
