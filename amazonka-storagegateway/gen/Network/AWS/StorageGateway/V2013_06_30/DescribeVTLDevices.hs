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
    , describeVTLDevices
    -- ** Request lenses
    , dvtldiGatewayARN
    , dvtldiMarker
    , dvtldiLimit
    , dvtldiVTLDeviceARNs

    -- * Response
    , DescribeVTLDevicesResponse
    -- ** Response lenses
    , dvtldoGatewayARN
    , dvtldoMarker
    , dvtldoVTLDevices
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeVTLDevices' request.
describeVTLDevices :: Text -- ^ 'dvtldiGatewayARN'
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

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dvtldiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeVTLDevices
    -> f DescribeVTLDevices
dvtldiGatewayARN f x =
    (\y -> x { _dvtldiGatewayARN = y })
       <$> f (_dvtldiGatewayARN x)
{-# INLINE dvtldiGatewayARN #-}

dvtldiMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVTLDevices
    -> f DescribeVTLDevices
dvtldiMarker f x =
    (\y -> x { _dvtldiMarker = y })
       <$> f (_dvtldiMarker x)
{-# INLINE dvtldiMarker #-}

dvtldiLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeVTLDevices
    -> f DescribeVTLDevices
dvtldiLimit f x =
    (\y -> x { _dvtldiLimit = y })
       <$> f (_dvtldiLimit x)
{-# INLINE dvtldiLimit #-}

dvtldiVTLDeviceARNs
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeVTLDevices
    -> f DescribeVTLDevices
dvtldiVTLDeviceARNs f x =
    (\y -> x { _dvtldiVTLDeviceARNs = y })
       <$> f (_dvtldiVTLDeviceARNs x)
{-# INLINE dvtldiVTLDeviceARNs #-}

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

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dvtldoGatewayARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVTLDevicesResponse
    -> f DescribeVTLDevicesResponse
dvtldoGatewayARN f x =
    (\y -> x { _dvtldoGatewayARN = y })
       <$> f (_dvtldoGatewayARN x)
{-# INLINE dvtldoGatewayARN #-}

dvtldoMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeVTLDevicesResponse
    -> f DescribeVTLDevicesResponse
dvtldoMarker f x =
    (\y -> x { _dvtldoMarker = y })
       <$> f (_dvtldoMarker x)
{-# INLINE dvtldoMarker #-}

dvtldoVTLDevices
    :: Functor f
    => ([VTLDevice]
    -> f ([VTLDevice]))
    -> DescribeVTLDevicesResponse
    -> f DescribeVTLDevicesResponse
dvtldoVTLDevices f x =
    (\y -> x { _dvtldoVTLDevices = y })
       <$> f (_dvtldoVTLDevices x)
{-# INLINE dvtldoVTLDevices #-}

instance FromJSON DescribeVTLDevicesResponse

instance AWSRequest DescribeVTLDevices where
    type Sv DescribeVTLDevices = StorageGateway
    type Rs DescribeVTLDevices = DescribeVTLDevicesResponse

    request = get
    response _ = jsonResponse

instance AWSPager DescribeVTLDevices where
    next rq rs = (\x -> rq { _dvtldiMarker = Just x })
        <$> (_dvtldoMarker rs)
