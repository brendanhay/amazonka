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

-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of virtual tape library (VTL) devices for the
-- specified gateway. In the response, AWS Storage Gateway returns VTL device
-- information. The list of VTL devices must be from one gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeVTLDevices.html>
module Network.AWS.StorageGateway.DescribeVTLDevices
    (
    -- * Request
      DescribeVTLDevices
    -- ** Request constructor
    , describeVTLDevices
    -- ** Request lenses
    , dvtldGatewayARN
    , dvtldLimit
    , dvtldMarker
    , dvtldVTLDeviceARNs

    -- * Response
    , DescribeVTLDevicesResponse
    -- ** Response constructor
    , describeVTLDevicesResponse
    -- ** Response lenses
    , dvtldrGatewayARN
    , dvtldrMarker
    , dvtldrVTLDevices
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data DescribeVTLDevices = DescribeVTLDevices
    { _dvtldGatewayARN    :: Text
    , _dvtldLimit         :: Maybe Nat
    , _dvtldMarker        :: Maybe Text
    , _dvtldVTLDeviceARNs :: List "VTLDeviceARNs" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeVTLDevices' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldGatewayARN' @::@ 'Text'
--
-- * 'dvtldLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dvtldMarker' @::@ 'Maybe' 'Text'
--
-- * 'dvtldVTLDeviceARNs' @::@ ['Text']
--
describeVTLDevices :: Text -- ^ 'dvtldGatewayARN'
                   -> DescribeVTLDevices
describeVTLDevices p1 = DescribeVTLDevices
    { _dvtldGatewayARN    = p1
    , _dvtldVTLDeviceARNs = mempty
    , _dvtldMarker        = Nothing
    , _dvtldLimit         = Nothing
    }

dvtldGatewayARN :: Lens' DescribeVTLDevices Text
dvtldGatewayARN = lens _dvtldGatewayARN (\s a -> s { _dvtldGatewayARN = a })

-- | Specifies that the number of VTL devices described be limited to the
-- specified number.
dvtldLimit :: Lens' DescribeVTLDevices (Maybe Natural)
dvtldLimit = lens _dvtldLimit (\s a -> s { _dvtldLimit = a }) . mapping _Nat

-- | An opaque string that indicates the position at which to begin describing
-- the VTL devices.
dvtldMarker :: Lens' DescribeVTLDevices (Maybe Text)
dvtldMarker = lens _dvtldMarker (\s a -> s { _dvtldMarker = a })

-- | An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
dvtldVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldVTLDeviceARNs =
    lens _dvtldVTLDeviceARNs (\s a -> s { _dvtldVTLDeviceARNs = a })
        . _List

data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse
    { _dvtldrGatewayARN :: Maybe Text
    , _dvtldrMarker     :: Maybe Text
    , _dvtldrVTLDevices :: List "VTLDevices" VTLDevice
    } deriving (Eq, Show)

-- | 'DescribeVTLDevicesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldrGatewayARN' @::@ 'Maybe' 'Text'
--
-- * 'dvtldrMarker' @::@ 'Maybe' 'Text'
--
-- * 'dvtldrVTLDevices' @::@ ['VTLDevice']
--
describeVTLDevicesResponse :: DescribeVTLDevicesResponse
describeVTLDevicesResponse = DescribeVTLDevicesResponse
    { _dvtldrGatewayARN = Nothing
    , _dvtldrVTLDevices = mempty
    , _dvtldrMarker     = Nothing
    }

dvtldrGatewayARN :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrGatewayARN = lens _dvtldrGatewayARN (\s a -> s { _dvtldrGatewayARN = a })

-- | An opaque string that indicates the position at which the VTL devices
-- that were fetched for description ended. Use the marker in your next
-- request to fetch the next set of VTL devices in the list. If there are no
-- more VTL devices to describe, this field does not appear in the response.
dvtldrMarker :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrMarker = lens _dvtldrMarker (\s a -> s { _dvtldrMarker = a })

-- | An array of VTL device objects composed of the Amazon Resource Name(ARN)
-- of the VTL devices.
dvtldrVTLDevices :: Lens' DescribeVTLDevicesResponse [VTLDevice]
dvtldrVTLDevices = lens _dvtldrVTLDevices (\s a -> s { _dvtldrVTLDevices = a }) . _List

instance ToPath DescribeVTLDevices where
    toPath = const "/"

instance ToQuery DescribeVTLDevices where
    toQuery = const mempty

instance ToHeaders DescribeVTLDevices

instance ToJSON DescribeVTLDevices where
    toJSON DescribeVTLDevices{..} = object
        [ "GatewayARN"    .= _dvtldGatewayARN
        , "VTLDeviceARNs" .= _dvtldVTLDeviceARNs
        , "Marker"        .= _dvtldMarker
        , "Limit"         .= _dvtldLimit
        ]

json

instance AWSRequest DescribeVTLDevices where
    type Sv DescribeVTLDevices = StorageGateway
    type Rs DescribeVTLDevices = DescribeVTLDevicesResponse

    request  = post "DescribeVTLDevices"
    response = jsonResponse

instance FromJSON DescribeVTLDevicesResponse where
    parseJSON = withObject "DescribeVTLDevicesResponse" $ \o -> DescribeVTLDevicesResponse
        <$> o .:? "GatewayARN"
        <*> o .:? "Marker"
        <*> o .:  "VTLDevices"

instance AWSPager DescribeVTLDevices where
    page rq rs
        | stop (rq ^. dvtldMarker) = Nothing
        | otherwise = (\x -> rq & dvtldMarker ?~ x)
            <$> (rs ^. dvtldrMarker)
