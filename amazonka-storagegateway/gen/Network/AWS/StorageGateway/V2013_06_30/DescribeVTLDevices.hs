{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices
    (
    -- * Request
      DescribeVTLDevices
    -- ** Request constructor
    , mkDescribeVTLDevices
    -- ** Request lenses
    , dvtldGatewayARN
    , dvtldVTLDeviceARNs
    , dvtldMarker
    , dvtldLimit

    -- * Response
    , DescribeVTLDevicesResponse
    -- ** Response lenses
    , dvtldrsGatewayARN
    , dvtldrsVTLDevices
    , dvtldrsMarker
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeVTLDevices = DescribeVTLDevices
    { _dvtldGatewayARN :: Text
    , _dvtldVTLDeviceARNs :: [Text]
    , _dvtldMarker :: Maybe Text
    , _dvtldLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVTLDevices' request.
mkDescribeVTLDevices :: Text -- ^ 'dvtldGatewayARN'
                     -> DescribeVTLDevices
mkDescribeVTLDevices p1 = DescribeVTLDevices
    { _dvtldGatewayARN = p1
    , _dvtldVTLDeviceARNs = mempty
    , _dvtldMarker = Nothing
    , _dvtldLimit = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dvtldGatewayARN :: Lens' DescribeVTLDevices Text
dvtldGatewayARN = lens _dvtldGatewayARN (\s a -> s { _dvtldGatewayARN = a })

dvtldVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldVTLDeviceARNs =
    lens _dvtldVTLDeviceARNs (\s a -> s { _dvtldVTLDeviceARNs = a })

dvtldMarker :: Lens' DescribeVTLDevices (Maybe Text)
dvtldMarker = lens _dvtldMarker (\s a -> s { _dvtldMarker = a })

dvtldLimit :: Lens' DescribeVTLDevices (Maybe Integer)
dvtldLimit = lens _dvtldLimit (\s a -> s { _dvtldLimit = a })

instance ToPath DescribeVTLDevices

instance ToQuery DescribeVTLDevices

instance ToHeaders DescribeVTLDevices

instance ToJSON DescribeVTLDevices

data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse
    { _dvtldrsGatewayARN :: Maybe Text
    , _dvtldrsVTLDevices :: [VTLDevice]
    , _dvtldrsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dvtldrsGatewayARN :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsGatewayARN =
    lens _dvtldrsGatewayARN (\s a -> s { _dvtldrsGatewayARN = a })

dvtldrsVTLDevices :: Lens' DescribeVTLDevicesResponse [VTLDevice]
dvtldrsVTLDevices =
    lens _dvtldrsVTLDevices (\s a -> s { _dvtldrsVTLDevices = a })

dvtldrsMarker :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsMarker = lens _dvtldrsMarker (\s a -> s { _dvtldrsMarker = a })

instance FromJSON DescribeVTLDevicesResponse

instance AWSRequest DescribeVTLDevices where
    type Sv DescribeVTLDevices = StorageGateway
    type Rs DescribeVTLDevices = DescribeVTLDevicesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeVTLDevices where
    next rq rs = (\x -> rq & dvtldMarker ?~ x)
        <$> (rs ^. dvtldrsMarker)
