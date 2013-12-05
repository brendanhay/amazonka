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

mkQuery :: IsQuery a => Service -> StdMethod -> ByteString -> a -> Raw
mkQuery s@Service{..} m p x = Raw s m p (toQuery x) [] (RequestBodyBS "")

mkXML :: IsXML a => Service -> StdMethod -> ByteString -> a -> Raw
mkXML s@Service{..} m p = Raw s m p [] [] . RequestBodyBS . toXML
--     , rqHeaders = [hdr (Content :: XML)]

-- requestXML :: IsXML a
--            => Service
--            -> Method
--            -> ByteString
--            -> a
--            -> Request
-- requestXML svc meth path x = Request
--     { rqService = svc
--     , rqMethod  = meth
--     , rqPath    = addPrefix "/" path
--     , rqQuery   = []
--     , rqBody    = Strict $ toXML x
--     }

(.?.) :: Raw -> [(ByteString, ByteString)] -> Raw
(.?.) r q = r { rqQuery = rqQuery r ++ q }

-- (.:.) :: Request -> [AnyHeader] -> Request
-- (.:.) rq hs = rq { rqHeaders = rqHeaders rq ++ hs }

-- version4Query :: IsQuery a => Service -> Method -> ByteString -> a -> AWS Signed
-- version4Query svc meth act q = sign version4 $
--     requestQuery svc meth "/" q .?.
--         [ ("Action",  act)
--         , ("Version", svcVersion svc)
--         ]
