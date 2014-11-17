{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.StartGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation starts a gateway that you previously shut down (see
-- ShutdownGateway). After the gateway starts, you can then make other API
-- calls, your applications can read from or write to the gateway's storage
-- volumes and you will be able to take snapshot backups. To specify which
-- gateway to start, use the Amazon Resource Name (ARN) of the gateway in your
-- request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_StartGateway.html>
module Network.AWS.StorageGateway.StartGateway
    (
    -- * Request
      StartGateway
    -- ** Request constructor
    , startGateway
    -- ** Request lenses
    , sgGatewayARN

    -- * Response
    , StartGatewayResponse
    -- ** Response constructor
    , startGatewayResponse
    -- ** Response lenses
    , sgrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype StartGateway = StartGateway
    { _sgGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StartGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgGatewayARN' @::@ 'Text'
--
startGateway :: Text -- ^ 'sgGatewayARN'
             -> StartGateway
startGateway p1 = StartGateway
    { _sgGatewayARN = p1
    }

sgGatewayARN :: Lens' StartGateway Text
sgGatewayARN = lens _sgGatewayARN (\s a -> s { _sgGatewayARN = a })

newtype StartGatewayResponse = StartGatewayResponse
    { _sgrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'StartGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgrGatewayARN' @::@ 'Maybe' 'Text'
--
startGatewayResponse :: StartGatewayResponse
startGatewayResponse = StartGatewayResponse
    { _sgrGatewayARN = Nothing
    }

sgrGatewayARN :: Lens' StartGatewayResponse (Maybe Text)
sgrGatewayARN = lens _sgrGatewayARN (\s a -> s { _sgrGatewayARN = a })

instance ToPath StartGateway where
    toPath = const "/"

instance ToQuery StartGateway where
    toQuery = const mempty

instance ToHeaders StartGateway

instance ToJSON StartGateway where
    toJSON StartGateway{..} = object
        [ "GatewayARN" .= _sgGatewayARN
        ]

instance AWSRequest StartGateway where
    type Sv StartGateway = StorageGateway
    type Rs StartGateway = StartGatewayResponse

    request  = post "StartGateway"
    response = jsonResponse

instance FromJSON StartGatewayResponse where
    parseJSON = withObject "StartGatewayResponse" $ \o -> StartGatewayResponse
        <$> o .:? "GatewayARN"
