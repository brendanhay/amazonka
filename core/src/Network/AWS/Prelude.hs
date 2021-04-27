-- |
-- Module      : Network.AWS.Prelude
-- Copyright   : (c) 2013-2018 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Prelude
    ( module Network.AWS.Prelude
    , module Export
    ) where

import Control.Applicative       as Export (Applicative, pure)
import Control.Applicative       as Export ((<$>), (<*>), (<|>))
import Control.DeepSeq           as Export (NFData)
import Data.Data                 as Export (Data, Typeable)
import Data.Foldable             as Export (Foldable)
import Data.Hashable             as Export (Hashable)
import Data.HashMap.Strict       as Export (HashMap)
import Data.List.NonEmpty        as Export (NonEmpty (..))
import Data.Maybe                as Export
import Data.Monoid               as Export (First, mconcat, mempty)
import Data.Monoid               as Export ((<>))
import GHC.Exts                  as Export (toList)
import GHC.Generics              as Export (Generic)
import Network.HTTP.Types.Status as Export (Status (..))
import Network.HTTP.Types.URI    as Export (urlDecode, urlEncode)
import Numeric.Natural           as Export (Natural)

import Network.AWS.Data.Base64     as Export
import Network.AWS.Data.Body       as Export
import Network.AWS.Data.ByteString as Export
import Network.AWS.Data.Crypto     as Export hiding (Base (..))
import Network.AWS.Data.Headers    as Export
import Network.AWS.Data.JSON       as Export
import Network.AWS.Data.List1      as Export
import Network.AWS.Data.Map        as Export
import Network.AWS.Data.Numeric    as Export
import Network.AWS.Data.Path       as Export
import Network.AWS.Data.Query      as Export
import Network.AWS.Data.Sensitive  as Export
import Network.AWS.Data.Text       as Export
import Network.AWS.Data.Time       as Export (AWSTime, BasicTime, ISO8601,
                                              POSIX, RFC822, Time (..), UTCTime,
                                              _Time)
import Network.AWS.Data.XML        as Export
import Network.AWS.Endpoint        as Export
import Network.AWS.Error           as Export
import Network.AWS.Types           as Export hiding (Algorithm, Endpoint,
                                              LogLevel (..), Seconds, Signer,
                                              serviceEndpoint)
       
import Prelude                     as Export hiding
  (log, min, max, maximum, minimum, sum, compare, even, odd, flip, until,
   error, head, tail, init, last, id)

infixl 7 .!@

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may _ [] = pure Nothing
may f xs = Just <$> f xs
