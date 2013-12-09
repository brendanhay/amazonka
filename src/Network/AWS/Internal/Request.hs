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

import Data.ByteString                 (ByteString)
import Network.AWS.Headers
import Network.AWS.Internal.Signing
import Network.AWS.Internal.String
import Network.AWS.Internal.Types
import Network.HTTP.Conduit
import Network.HTTP.QueryString.Pickle
import Network.HTTP.Types.Method
import Text.XML.Expat.Pickle.Generic

query :: IsQuery a => Service -> StdMethod -> ByteString -> a -> Raw
query s@Service{..} m p x = Raw s m p (toQuery x) [] (RequestBodyBS "")

query4 :: IsQuery a => Service -> StdMethod -> ByteString -> a -> Raw
query4 s m a q = query s m "/" q .?. [("Action",  a)]

xml :: IsXML a => Service -> StdMethod -> ByteString -> a -> Raw
xml s@Service{..} m p = Raw s m p [] [] . RequestBodyBS . toXML
--     , rqHeaders = [hdr (Content :: XML)]

(.?.) :: Raw -> [(ByteString, ByteString)] -> Raw
(.?.) r q = r { rqQuery = rqQuery r ++ q }

-- (.:.) :: Request -> [AnyHeader] -> Request
-- (.:.) rq hs = rq { rqHeaders = rqHeaders rq ++ hs }
