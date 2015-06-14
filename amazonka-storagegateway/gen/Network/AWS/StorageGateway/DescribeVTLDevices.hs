{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a description of virtual tape library (VTL) devices for the
-- specified gateway. In the response, AWS Storage Gateway returns VTL
-- device information.
--
-- The list of VTL devices must be from one gateway.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeVTLDevices.html>
module Network.AWS.StorageGateway.DescribeVTLDevices
    (
    -- * Request
      DescribeVTLDevices
    -- ** Request constructor
    , describeVTLDevices
    -- ** Request lenses
    , dvtldVTLDeviceARNs
    , dvtldGatewayARN
    , dvtldMarker
    , dvtldLimit

    -- * Response
    , DescribeVTLDevicesResponse
    -- ** Response constructor
    , describeVTLDevicesResponse
    -- ** Response lenses
    , dvtldrVTLDevices
    , dvtldrGatewayARN
    , dvtldrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'describeVTLDevices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldVTLDeviceARNs'
--
-- * 'dvtldGatewayARN'
--
-- * 'dvtldMarker'
--
-- * 'dvtldLimit'
data DescribeVTLDevices = DescribeVTLDevices'{_dvtldVTLDeviceARNs :: [Text], _dvtldGatewayARN :: Text, _dvtldMarker :: Text, _dvtldLimit :: Nat} deriving (Eq, Read, Show)

-- | 'DescribeVTLDevices' smart constructor.
describeVTLDevices :: Text -> Text -> Natural -> DescribeVTLDevices
describeVTLDevices pGatewayARN pMarker pLimit = DescribeVTLDevices'{_dvtldVTLDeviceARNs = mempty, _dvtldGatewayARN = pGatewayARN, _dvtldMarker = pMarker, _dvtldLimit = _Nat # pLimit};

-- | An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
--
-- All of the specified VTL devices must be from the same gateway. If no
-- VTL devices are specified, the result will contain all devices on the
-- specified gateway.
dvtldVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldVTLDeviceARNs = lens _dvtldVTLDeviceARNs (\ s a -> s{_dvtldVTLDeviceARNs = a});

-- | FIXME: Undocumented member.
dvtldGatewayARN :: Lens' DescribeVTLDevices Text
dvtldGatewayARN = lens _dvtldGatewayARN (\ s a -> s{_dvtldGatewayARN = a});

-- | An opaque string that indicates the position at which to begin
-- describing the VTL devices.
dvtldMarker :: Lens' DescribeVTLDevices Text
dvtldMarker = lens _dvtldMarker (\ s a -> s{_dvtldMarker = a});

-- | Specifies that the number of VTL devices described be limited to the
-- specified number.
dvtldLimit :: Lens' DescribeVTLDevices Natural
dvtldLimit = lens _dvtldLimit (\ s a -> s{_dvtldLimit = a}) . _Nat;

instance AWSRequest DescribeVTLDevices where
        type Sv DescribeVTLDevices = StorageGateway
        type Rs DescribeVTLDevices =
             DescribeVTLDevicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVTLDevicesResponse' <$>
                   x .?> "VTLDevices" .!@ mempty <*> x .:> "GatewayARN"
                     <*> x .:> "Marker")

instance ToHeaders DescribeVTLDevices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeVTLDevices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVTLDevices where
        toJSON DescribeVTLDevices'{..}
          = object
              ["VTLDeviceARNs" .= _dvtldVTLDeviceARNs,
               "GatewayARN" .= _dvtldGatewayARN,
               "Marker" .= _dvtldMarker, "Limit" .= _dvtldLimit]

instance ToPath DescribeVTLDevices where
        toPath = const "/"

instance ToQuery DescribeVTLDevices where
        toQuery = const mempty

-- | /See:/ 'describeVTLDevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldrVTLDevices'
--
-- * 'dvtldrGatewayARN'
--
-- * 'dvtldrMarker'
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'{_dvtldrVTLDevices :: [VTLDevice], _dvtldrGatewayARN :: Text, _dvtldrMarker :: Text} deriving (Eq, Read, Show)

-- | 'DescribeVTLDevicesResponse' smart constructor.
describeVTLDevicesResponse :: Text -> Text -> DescribeVTLDevicesResponse
describeVTLDevicesResponse pGatewayARN pMarker = DescribeVTLDevicesResponse'{_dvtldrVTLDevices = mempty, _dvtldrGatewayARN = pGatewayARN, _dvtldrMarker = pMarker};

-- | An array of VTL device objects composed of the Amazon Resource Name(ARN)
-- of the VTL devices.
dvtldrVTLDevices :: Lens' DescribeVTLDevicesResponse [VTLDevice]
dvtldrVTLDevices = lens _dvtldrVTLDevices (\ s a -> s{_dvtldrVTLDevices = a});

-- | FIXME: Undocumented member.
dvtldrGatewayARN :: Lens' DescribeVTLDevicesResponse Text
dvtldrGatewayARN = lens _dvtldrGatewayARN (\ s a -> s{_dvtldrGatewayARN = a});

-- | An opaque string that indicates the position at which the VTL devices
-- that were fetched for description ended. Use the marker in your next
-- request to fetch the next set of VTL devices in the list. If there are
-- no more VTL devices to describe, this field does not appear in the
-- response.
dvtldrMarker :: Lens' DescribeVTLDevicesResponse Text
dvtldrMarker = lens _dvtldrMarker (\ s a -> s{_dvtldrMarker = a});
