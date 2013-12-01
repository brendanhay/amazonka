{-# LANGUAGE OverloadedStrings #-}

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
import Network.HTTP.QueryString.Pickle
import Network.Http.Client             (Method)
import Text.XML.Expat.Pickle.Generic

(.?.) :: Request -> [(ByteString, ByteString)] -> Request
(.?.) rq qry = rq { rqQuery = rqQuery rq ++ qry }

(.:.) :: Request -> [AnyHeader] -> Request
(.:.) rq hs = rq { rqHeaders = rqHeaders rq ++ hs }

version4Query :: IsQuery a => Service -> Method -> ByteString -> a -> AWS Signed
version4Query svc meth act q = sign version4 $
    requestQuery svc meth "/" q .?.
        [ ("Action",  act)
        , ("Version", svcVersion svc)
        ]

requestQuery :: IsQuery a
             => Service
             -> Method
             -> ByteString
             -> a
             -> Request
requestQuery svc meth path q = Request
    { rqService = svc
    , rqMethod  = meth
    , rqPath    = addPrefix "/" path
    , rqHeaders = [] -- [hdr (Content :: FormURLEncoded)]
    , rqQuery   = toQuery q
    , rqBody    = Empty
    }

requestXML :: IsXML a
           => Service
           -> Method
           -> ByteString
           -> a
           -> Request
requestXML svc meth path x = Request
    { rqService = svc
    , rqMethod  = meth
    , rqPath    = addPrefix "/" path
    , rqHeaders = [hdr (Content :: XML)]
    , rqQuery   = []
    , rqBody    = Strict $ toXML x
    }
