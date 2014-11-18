{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.ShutdownGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation shuts down a gateway. To specify which gateway to shut down,
-- use the Amazon Resource Name (ARN) of the gateway in the body of your
-- request. The operation shuts down the gateway service component running in
-- the storage gateway's virtual machine (VM) and not the VM. After the
-- gateway is shutdown, you cannot call any other API except StartGateway,
-- DescribeGatewayInformation, and ListGateways. For more information, see
-- ActivateGateway. Your applications cannot read from or write to the
-- gateway's storage volumes, and there are no snapshots taken. If do not
-- intend to use the gateway again, you must delete the gateway (using
-- DeleteGateway) to no longer pay software charges associated with the
-- gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ShutdownGateway.html>
module Network.AWS.StorageGateway.ShutdownGateway
    (
    -- * Request
      ShutdownGateway
    -- ** Request constructor
    , shutdownGateway
    -- ** Request lenses
    , sg1GatewayARN

    -- * Response
    , ShutdownGatewayResponse
    -- ** Response constructor
    , shutdownGatewayResponse
    -- ** Response lenses
    , sgr1GatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype ShutdownGateway = ShutdownGateway
    { _sg1GatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ShutdownGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sg1GatewayARN' @::@ 'Text'
--
shutdownGateway :: Text -- ^ 'sg1GatewayARN'
                -> ShutdownGateway
shutdownGateway p1 = ShutdownGateway
    { _sg1GatewayARN = p1
    }

sg1GatewayARN :: Lens' ShutdownGateway Text
sg1GatewayARN = lens _sg1GatewayARN (\s a -> s { _sg1GatewayARN = a })

newtype ShutdownGatewayResponse = ShutdownGatewayResponse
    { _sgr1GatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ShutdownGatewayResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgr1GatewayARN' @::@ 'Maybe' 'Text'
--
shutdownGatewayResponse :: ShutdownGatewayResponse
shutdownGatewayResponse = ShutdownGatewayResponse
    { _sgr1GatewayARN = Nothing
    }

sgr1GatewayARN :: Lens' ShutdownGatewayResponse (Maybe Text)
sgr1GatewayARN = lens _sgr1GatewayARN (\s a -> s { _sgr1GatewayARN = a })

instance ToPath ShutdownGateway where
    toPath = const "/"

instance ToQuery ShutdownGateway where
    toQuery = const mempty

instance ToHeaders ShutdownGateway

instance ToJSON ShutdownGateway where
    toJSON ShutdownGateway{..} = object
        [ "GatewayARN" .= _sg1GatewayARN
        ]

instance AWSRequest ShutdownGateway where
    type Sv ShutdownGateway = StorageGateway
    type Rs ShutdownGateway = ShutdownGatewayResponse

    request  = post "ShutdownGateway"
    response = jsonResponse

instance FromJSON ShutdownGatewayResponse where
    parseJSON = withObject "ShutdownGatewayResponse" $ \o -> ShutdownGatewayResponse
        <$> o .:? "GatewayARN"
