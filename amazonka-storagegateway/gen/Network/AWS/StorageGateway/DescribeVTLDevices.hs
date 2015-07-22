{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of virtual tape library (VTL) devices for the
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
    , dvtldrqMarker
    , dvtldrqLimit
    , dvtldrqVTLDeviceARNs
    , dvtldrqGatewayARN

    -- * Response
    , DescribeVTLDevicesResponse
    -- ** Response constructor
    , describeVTLDevicesResponse
    -- ** Response lenses
    , dvtldrsGatewayARN
    , dvtldrsVTLDevices
    , dvtldrsMarker
    , dvtldrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DescribeVTLDevicesInput
--
-- /See:/ 'describeVTLDevices' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldrqMarker'
--
-- * 'dvtldrqLimit'
--
-- * 'dvtldrqVTLDeviceARNs'
--
-- * 'dvtldrqGatewayARN'
data DescribeVTLDevices = DescribeVTLDevices'
    { _dvtldrqMarker        :: !(Maybe Text)
    , _dvtldrqLimit         :: !(Maybe Nat)
    , _dvtldrqVTLDeviceARNs :: !(Maybe [Text])
    , _dvtldrqGatewayARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVTLDevices' smart constructor.
describeVTLDevices :: Text -> DescribeVTLDevices
describeVTLDevices pGatewayARN =
    DescribeVTLDevices'
    { _dvtldrqMarker = Nothing
    , _dvtldrqLimit = Nothing
    , _dvtldrqVTLDeviceARNs = Nothing
    , _dvtldrqGatewayARN = pGatewayARN
    }

-- | An opaque string that indicates the position at which to begin
-- describing the VTL devices.
dvtldrqMarker :: Lens' DescribeVTLDevices (Maybe Text)
dvtldrqMarker = lens _dvtldrqMarker (\ s a -> s{_dvtldrqMarker = a});

-- | Specifies that the number of VTL devices described be limited to the
-- specified number.
dvtldrqLimit :: Lens' DescribeVTLDevices (Maybe Natural)
dvtldrqLimit = lens _dvtldrqLimit (\ s a -> s{_dvtldrqLimit = a}) . mapping _Nat;

-- | An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
--
-- All of the specified VTL devices must be from the same gateway. If no
-- VTL devices are specified, the result will contain all devices on the
-- specified gateway.
dvtldrqVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldrqVTLDeviceARNs = lens _dvtldrqVTLDeviceARNs (\ s a -> s{_dvtldrqVTLDeviceARNs = a}) . _Default;

-- | FIXME: Undocumented member.
dvtldrqGatewayARN :: Lens' DescribeVTLDevices Text
dvtldrqGatewayARN = lens _dvtldrqGatewayARN (\ s a -> s{_dvtldrqGatewayARN = a});

instance AWSPager DescribeVTLDevices where
        page rq rs
          | stop (rs ^. dvtldrsMarker) = Nothing
          | stop (rs ^. dvtldrsVTLDevices) = Nothing
          | otherwise =
            Just $ rq & dvtldrqMarker .~ rs ^. dvtldrsMarker

instance AWSRequest DescribeVTLDevices where
        type Sv DescribeVTLDevices = StorageGateway
        type Rs DescribeVTLDevices =
             DescribeVTLDevicesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVTLDevicesResponse' <$>
                   (x .?> "GatewayARN") <*>
                     (x .?> "VTLDevices" .!@ mempty)
                     <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

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
              ["Marker" .= _dvtldrqMarker,
               "Limit" .= _dvtldrqLimit,
               "VTLDeviceARNs" .= _dvtldrqVTLDeviceARNs,
               "GatewayARN" .= _dvtldrqGatewayARN]

instance ToPath DescribeVTLDevices where
        toPath = const "/"

instance ToQuery DescribeVTLDevices where
        toQuery = const mempty

-- | DescribeVTLDevicesOutput
--
-- /See:/ 'describeVTLDevicesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvtldrsGatewayARN'
--
-- * 'dvtldrsVTLDevices'
--
-- * 'dvtldrsMarker'
--
-- * 'dvtldrsStatus'
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'
    { _dvtldrsGatewayARN :: !(Maybe Text)
    , _dvtldrsVTLDevices :: !(Maybe [VTLDevice])
    , _dvtldrsMarker     :: !(Maybe Text)
    , _dvtldrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVTLDevicesResponse' smart constructor.
describeVTLDevicesResponse :: Int -> DescribeVTLDevicesResponse
describeVTLDevicesResponse pStatus =
    DescribeVTLDevicesResponse'
    { _dvtldrsGatewayARN = Nothing
    , _dvtldrsVTLDevices = Nothing
    , _dvtldrsMarker = Nothing
    , _dvtldrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dvtldrsGatewayARN :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsGatewayARN = lens _dvtldrsGatewayARN (\ s a -> s{_dvtldrsGatewayARN = a});

-- | An array of VTL device objects composed of the Amazon Resource Name(ARN)
-- of the VTL devices.
dvtldrsVTLDevices :: Lens' DescribeVTLDevicesResponse [VTLDevice]
dvtldrsVTLDevices = lens _dvtldrsVTLDevices (\ s a -> s{_dvtldrsVTLDevices = a}) . _Default;

-- | An opaque string that indicates the position at which the VTL devices
-- that were fetched for description ended. Use the marker in your next
-- request to fetch the next set of VTL devices in the list. If there are
-- no more VTL devices to describe, this field does not appear in the
-- response.
dvtldrsMarker :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsMarker = lens _dvtldrsMarker (\ s a -> s{_dvtldrsMarker = a});

-- | FIXME: Undocumented member.
dvtldrsStatus :: Lens' DescribeVTLDevicesResponse Int
dvtldrsStatus = lens _dvtldrsStatus (\ s a -> s{_dvtldrsStatus = a});
