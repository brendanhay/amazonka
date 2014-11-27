{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation updates the gateway virtual machine (VM) software. The request
-- immediately triggers the software update.
--
-- A software update forces a system restart of your gateway. You can minimize
-- the chance of any disruption to your applications by increasing your iSCSI
-- Initiators' timeouts. For more information about increasing iSCSI Initiator
-- timeouts for Windows and Linux, see <http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorWindowsClient.html#CustomizeWindowsiSCSISettings Customizing Your Windows iSCSI Settings>
-- and <http://docs.aws.amazon.com/storagegateway/latest/userguide/ConfiguringiSCSIClientInitiatorRedHatClient.html#CustomizeLinuxiSCSISettings Customizing Your Linux iSCSI Settings>, respectively.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewaySoftwareNow.html>
module Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
    (
    -- * Request
      UpdateGatewaySoftwareNow
    -- ** Request constructor
    , updateGatewaySoftwareNow
    -- ** Request lenses
    , ugsnGatewayARN

    -- * Response
    , UpdateGatewaySoftwareNowResponse
    -- ** Response constructor
    , updateGatewaySoftwareNowResponse
    -- ** Response lenses
    , ugsnrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow
    { _ugsnGatewayARN :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'UpdateGatewaySoftwareNow' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsnGatewayARN' @::@ 'Text'
--
updateGatewaySoftwareNow :: Text -- ^ 'ugsnGatewayARN'
                         -> UpdateGatewaySoftwareNow
updateGatewaySoftwareNow p1 = UpdateGatewaySoftwareNow
    { _ugsnGatewayARN = p1
    }

ugsnGatewayARN :: Lens' UpdateGatewaySoftwareNow Text
ugsnGatewayARN = lens _ugsnGatewayARN (\s a -> s { _ugsnGatewayARN = a })

newtype UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse
    { _ugsnrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'UpdateGatewaySoftwareNowResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugsnrGatewayARN' @::@ 'Maybe' 'Text'
--
updateGatewaySoftwareNowResponse :: UpdateGatewaySoftwareNowResponse
updateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse
    { _ugsnrGatewayARN = Nothing
    }

ugsnrGatewayARN :: Lens' UpdateGatewaySoftwareNowResponse (Maybe Text)
ugsnrGatewayARN = lens _ugsnrGatewayARN (\s a -> s { _ugsnrGatewayARN = a })

instance ToPath UpdateGatewaySoftwareNow where
    toPath = const "/"

instance ToQuery UpdateGatewaySoftwareNow where
    toQuery = const mempty

instance ToHeaders UpdateGatewaySoftwareNow

instance ToJSON UpdateGatewaySoftwareNow where
    toJSON UpdateGatewaySoftwareNow{..} = object
        [ "GatewayARN" .= _ugsnGatewayARN
        ]

instance AWSRequest UpdateGatewaySoftwareNow where
    type Sv UpdateGatewaySoftwareNow = StorageGateway
    type Rs UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNowResponse

    request  = post "UpdateGatewaySoftwareNow"
    response = jsonResponse

instance FromJSON UpdateGatewaySoftwareNowResponse where
    parseJSON = withObject "UpdateGatewaySoftwareNowResponse" $ \o -> UpdateGatewaySoftwareNowResponse
        <$> o .:? "GatewayARN"
