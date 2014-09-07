{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DisableGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DisableGateway
    (
    -- * Request
      DisableGateway
    -- ** Request constructor
    , mkDisableGateway
    -- ** Request lenses
    , dg1GatewayARN

    -- * Response
    , DisableGatewayResponse
    -- ** Response lenses
    , dgrsrsGatewayARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DisableGateway = DisableGateway
    { _dg1GatewayARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableGateway' request.
mkDisableGateway :: Text -- ^ 'dg1GatewayARN'
                 -> DisableGateway
mkDisableGateway p1 = DisableGateway
    { _dg1GatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dg1GatewayARN :: Lens' DisableGateway Text
dg1GatewayARN = lens _dg1GatewayARN (\s a -> s { _dg1GatewayARN = a })

instance ToPath DisableGateway

instance ToQuery DisableGateway

instance ToHeaders DisableGateway

instance ToJSON DisableGateway

newtype DisableGatewayResponse = DisableGatewayResponse
    { _dgrsrsGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dgrsrsGatewayARN :: Lens' DisableGatewayResponse (Maybe Text)
dgrsrsGatewayARN =
    lens _dgrsrsGatewayARN (\s a -> s { _dgrsrsGatewayARN = a })

instance FromJSON DisableGatewayResponse

instance AWSRequest DisableGateway where
    type Sv DisableGateway = StorageGateway
    type Rs DisableGateway = DisableGatewayResponse

    request = get
    response _ = jsonResponse
