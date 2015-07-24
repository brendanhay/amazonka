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
    , dvtldMarker
    , dvtldLimit
    , dvtldVTLDeviceARNs
    , dvtldGatewayARN

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
-- * 'dvtldMarker'
--
-- * 'dvtldLimit'
--
-- * 'dvtldVTLDeviceARNs'
--
-- * 'dvtldGatewayARN'
data DescribeVTLDevices = DescribeVTLDevices'
    { _dvtldMarker        :: !(Maybe Text)
    , _dvtldLimit         :: !(Maybe Nat)
    , _dvtldVTLDeviceARNs :: !(Maybe [Text])
    , _dvtldGatewayARN    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVTLDevices' smart constructor.
describeVTLDevices :: Text -> DescribeVTLDevices
describeVTLDevices pGatewayARN_ =
    DescribeVTLDevices'
    { _dvtldMarker = Nothing
    , _dvtldLimit = Nothing
    , _dvtldVTLDeviceARNs = Nothing
    , _dvtldGatewayARN = pGatewayARN_
    }

-- | An opaque string that indicates the position at which to begin
-- describing the VTL devices.
dvtldMarker :: Lens' DescribeVTLDevices (Maybe Text)
dvtldMarker = lens _dvtldMarker (\ s a -> s{_dvtldMarker = a});

-- | Specifies that the number of VTL devices described be limited to the
-- specified number.
dvtldLimit :: Lens' DescribeVTLDevices (Maybe Natural)
dvtldLimit = lens _dvtldLimit (\ s a -> s{_dvtldLimit = a}) . mapping _Nat;

-- | An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
--
-- All of the specified VTL devices must be from the same gateway. If no
-- VTL devices are specified, the result will contain all devices on the
-- specified gateway.
dvtldVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldVTLDeviceARNs = lens _dvtldVTLDeviceARNs (\ s a -> s{_dvtldVTLDeviceARNs = a}) . _Default;

-- | FIXME: Undocumented member.
dvtldGatewayARN :: Lens' DescribeVTLDevices Text
dvtldGatewayARN = lens _dvtldGatewayARN (\ s a -> s{_dvtldGatewayARN = a});

instance AWSPager DescribeVTLDevices where
        page rq rs
          | stop (rs ^. dvtldrsMarker) = Nothing
          | stop (rs ^. dvtldrsVTLDevices) = Nothing
          | otherwise =
            Just $ rq & dvtldMarker .~ rs ^. dvtldrsMarker

instance AWSRequest DescribeVTLDevices where
        type Sv DescribeVTLDevices = StorageGateway
        type Rs DescribeVTLDevices =
             DescribeVTLDevicesResponse
        request = postJSON "DescribeVTLDevices"
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
              ["Marker" .= _dvtldMarker, "Limit" .= _dvtldLimit,
               "VTLDeviceARNs" .= _dvtldVTLDeviceARNs,
               "GatewayARN" .= _dvtldGatewayARN]

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
describeVTLDevicesResponse pStatus_ =
    DescribeVTLDevicesResponse'
    { _dvtldrsGatewayARN = Nothing
    , _dvtldrsVTLDevices = Nothing
    , _dvtldrsMarker = Nothing
    , _dvtldrsStatus = pStatus_
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
