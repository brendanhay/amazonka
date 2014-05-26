{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- Module      : Network.AWS.Signing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Types where

import Data.Time
import Network.AWS.Types

data Signed v = Signed (Context ())

type Signer v = Auth -> Region -> UTCTime -> Signed v

class SigningAlgorithm v where
    finalise :: Service a v -> Context a -> Signer v

sign :: (AWSRequest a, AWSService (Sv a), SigningAlgorithm (Sg (Sv a)))
     => a
     -> Signer (Sg (Sv a))
sign = finalise service . request

-- sign :: Raw -> AWS Request
-- sign raw@Raw{..} = do
--     auth <- getAuth
--     reg  <- region rqService
--     time <- liftIO getCurrentTime

--     let sig = svcSigner rqService
--         hs  = hHost (endpoint rqService reg) : rqHeaders

--     return $! sig (raw { rqHeaders = hs }) auth reg time

-- common :: Raw -> Region -> Common
-- common Raw{..} reg = Common
--     { _service = svcName rqService
--     , _version = svcVersion rqService
--     , _host    = endpoint rqService reg
--     , _query   = sort rqQuery
--     }

-- data Common = Common
--     { _service :: !ByteString
--     , _version :: !ByteString
--     , _host    :: !ByteString
--     , _query   :: [(ByteString, Maybe ByteString)]
--     }


--     return . svcSigner $ Signee
--         { sigAccess  = Text.encodeUtf8 authAccessKeyId
--         , sigSecret  = Text.encodeUtf8 authSecretAccessKey
--         , sigToken   = Text.encodeUtf8 <$> authSecurityToken
--         , sigTime    = time
--         , sigRegion  = reg
--         , sigService = svcName
--         , sigVersion = svcVersion
--         , sigMethod  = BS.pack $ show rawMethod
--         , sigHost    = host
--         , sigPath    = Text.encodeUtf8 rawPath
--         , sigQuery   = HTTP.queryTextToQuery $ sort rawQuery
--         , sigHeaders = hHost host : rawHeaders
--         , sigBody    = rawBody
--         }
--   where
--     Service{..} = rawService

