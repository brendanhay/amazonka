{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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

module Network.AWS.StorageGateway.V2013_06_30.DescribeVTLDevices where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeVTLDevices' request.
describeVTLDevices :: Text -- ^ '_dvtldiGatewayARN'
                   -> DescribeVTLDevices
describeVTLDevices p1 = DescribeVTLDevices
    { _dvtldiGatewayARN = p1
    , _dvtldiMarker = Nothing
    , _dvtldiLimit = Nothing
    , _dvtldiVTLDeviceARNs = mempty
    }

data DescribeVTLDevices = DescribeVTLDevices
    { _dvtldiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dvtldiMarker :: Maybe Text
    , _dvtldiLimit :: Maybe Integer
    , _dvtldiVTLDeviceARNs :: [Text]
    } deriving (Show, Generic)

makeLenses ''DescribeVTLDevices

instance ToPath DescribeVTLDevices

instance ToQuery DescribeVTLDevices

instance ToHeaders DescribeVTLDevices

instance ToJSON DescribeVTLDevices

data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse
    { _dvtldoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _dvtldoMarker :: Maybe Text
    , _dvtldoVTLDevices :: [VTLDevice]
    } deriving (Show, Generic)

makeLenses ''DescribeVTLDevicesResponse

instance FromJSON DescribeVTLDevicesResponse

instance AWSRequest DescribeVTLDevices where
    type Sv DescribeVTLDevices = StorageGateway
    type Rs DescribeVTLDevices = DescribeVTLDevicesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeVTLDevices where
    next rq rs = (\x -> rq { _dvtldiMarker = Just x })
        <$> (_dvtldoMarker rs)
