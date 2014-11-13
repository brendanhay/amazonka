{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.StorageGateway.UpdateGatewaySoftwareNow
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the gateway virtual machine (VM) software. The
-- request immediately triggers the software update. A software update forces
-- a system restart of your gateway. You can minimize the chance of any
-- disruption to your applications by increasing your iSCSI Initiators'
-- timeouts. For more information about increasing iSCSI Initiator timeouts
-- for Windows and Linux, see Customizing Your Windows iSCSI Settings and
-- Customizing Your Linux iSCSI Settings, respectively.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

newtype UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNow
    { _ugsnGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

instance ToPath UpdateGatewaySoftwareNow where
    toPath = const "/"

instance ToQuery UpdateGatewaySoftwareNow where
    toQuery = const mempty

instance ToHeaders UpdateGatewaySoftwareNow

instance ToBody UpdateGatewaySoftwareNow where
    toBody = toBody . encode . _ugsnGatewayARN

newtype UpdateGatewaySoftwareNowResponse = UpdateGatewaySoftwareNowResponse
    { _ugsnrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

-- FromJSON

instance AWSRequest UpdateGatewaySoftwareNow where
    type Sv UpdateGatewaySoftwareNow = StorageGateway
    type Rs UpdateGatewaySoftwareNow = UpdateGatewaySoftwareNowResponse

    request  = post'
    response = jsonResponse $ \h o -> UpdateGatewaySoftwareNowResponse
        <$> o .: "GatewayARN"
