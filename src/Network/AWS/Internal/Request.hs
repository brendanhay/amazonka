{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal.Request
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Request where

import Control.Arrow
import Data.ByteString                 (ByteString)
import Network.AWS.Internal.String
import Network.AWS.Internal.Types
import Network.HTTP.Conduit
import Network.HTTP.QueryString.Pickle
import Network.HTTP.Types              hiding (toQuery)
import Text.XML.Expat.Pickle.Generic

query :: IsQuery a => Service -> StdMethod -> ByteString -> a -> Raw
query s@Service{..} m p x = Raw s m p q [] (RequestBodyBS "")
  where
    q = map (second Just) (toQuery x)

query4 :: IsQuery a => Service -> StdMethod -> ByteString -> a -> Raw
query4 s m a q = query s m "/" q .?. [("Action", Just a)]

xml :: IsXML a => Service -> StdMethod -> ByteString -> a -> Raw
xml s@Service{..} m p = Raw s m ("/" `addPrefix` p) [] [] . RequestBodyBS . toXML

(.?.) :: Raw -> [(ByteString, Maybe ByteString)] -> Raw
(.?.) r q = r { rqQuery = rqQuery r ++ q }

(.:.) :: Raw -> [Header] -> Raw
(.:.) r hs = r { rqHeaders = rqHeaders r ++ hs }
