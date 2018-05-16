{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.
--
--
-- This operation is only supported in the tape gateway type.
--
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeVTLDevices
    (
    -- * Creating a Request
      describeVTLDevices
    , DescribeVTLDevices
    -- * Request Lenses
    , dvtldMarker
    , dvtldLimit
    , dvtldVTLDeviceARNs
    , dvtldGatewayARN

    -- * Destructuring the Response
    , describeVTLDevicesResponse
    , DescribeVTLDevicesResponse
    -- * Response Lenses
    , dvtldrsVTLDevices
    , dvtldrsGatewayARN
    , dvtldrsMarker
    , dvtldrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | DescribeVTLDevicesInput
--
--
--
-- /See:/ 'describeVTLDevices' smart constructor.
data DescribeVTLDevices = DescribeVTLDevices'
  { _dvtldMarker        :: !(Maybe Text)
  , _dvtldLimit         :: !(Maybe Nat)
  , _dvtldVTLDeviceARNs :: !(Maybe [Text])
  , _dvtldGatewayARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVTLDevices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvtldMarker' - An opaque string that indicates the position at which to begin describing the VTL devices.
--
-- * 'dvtldLimit' - Specifies that the number of VTL devices described be limited to the specified number.
--
-- * 'dvtldVTLDeviceARNs' - An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
--
-- * 'dvtldGatewayARN' - Undocumented member.
describeVTLDevices
    :: Text -- ^ 'dvtldGatewayARN'
    -> DescribeVTLDevices
describeVTLDevices pGatewayARN_ =
  DescribeVTLDevices'
    { _dvtldMarker = Nothing
    , _dvtldLimit = Nothing
    , _dvtldVTLDeviceARNs = Nothing
    , _dvtldGatewayARN = pGatewayARN_
    }


-- | An opaque string that indicates the position at which to begin describing the VTL devices.
dvtldMarker :: Lens' DescribeVTLDevices (Maybe Text)
dvtldMarker = lens _dvtldMarker (\ s a -> s{_dvtldMarker = a})

-- | Specifies that the number of VTL devices described be limited to the specified number.
dvtldLimit :: Lens' DescribeVTLDevices (Maybe Natural)
dvtldLimit = lens _dvtldLimit (\ s a -> s{_dvtldLimit = a}) . mapping _Nat

-- | An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
dvtldVTLDeviceARNs :: Lens' DescribeVTLDevices [Text]
dvtldVTLDeviceARNs = lens _dvtldVTLDeviceARNs (\ s a -> s{_dvtldVTLDeviceARNs = a}) . _Default . _Coerce

-- | Undocumented member.
dvtldGatewayARN :: Lens' DescribeVTLDevices Text
dvtldGatewayARN = lens _dvtldGatewayARN (\ s a -> s{_dvtldGatewayARN = a})

instance AWSPager DescribeVTLDevices where
        page rq rs
          | stop (rs ^. dvtldrsMarker) = Nothing
          | stop (rs ^. dvtldrsVTLDevices) = Nothing
          | otherwise =
            Just $ rq & dvtldMarker .~ rs ^. dvtldrsMarker

instance AWSRequest DescribeVTLDevices where
        type Rs DescribeVTLDevices =
             DescribeVTLDevicesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVTLDevicesResponse' <$>
                   (x .?> "VTLDevices" .!@ mempty) <*>
                     (x .?> "GatewayARN")
                     <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeVTLDevices where

instance NFData DescribeVTLDevices where

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
              (catMaybes
                 [("Marker" .=) <$> _dvtldMarker,
                  ("Limit" .=) <$> _dvtldLimit,
                  ("VTLDeviceARNs" .=) <$> _dvtldVTLDeviceARNs,
                  Just ("GatewayARN" .= _dvtldGatewayARN)])

instance ToPath DescribeVTLDevices where
        toPath = const "/"

instance ToQuery DescribeVTLDevices where
        toQuery = const mempty

-- | DescribeVTLDevicesOutput
--
--
--
-- /See:/ 'describeVTLDevicesResponse' smart constructor.
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'
  { _dvtldrsVTLDevices     :: !(Maybe [VTLDevice])
  , _dvtldrsGatewayARN     :: !(Maybe Text)
  , _dvtldrsMarker         :: !(Maybe Text)
  , _dvtldrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVTLDevicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvtldrsVTLDevices' - An array of VTL device objects composed of the Amazon Resource Name(ARN) of the VTL devices.
--
-- * 'dvtldrsGatewayARN' - Undocumented member.
--
-- * 'dvtldrsMarker' - An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
--
-- * 'dvtldrsResponseStatus' - -- | The response status code.
describeVTLDevicesResponse
    :: Int -- ^ 'dvtldrsResponseStatus'
    -> DescribeVTLDevicesResponse
describeVTLDevicesResponse pResponseStatus_ =
  DescribeVTLDevicesResponse'
    { _dvtldrsVTLDevices = Nothing
    , _dvtldrsGatewayARN = Nothing
    , _dvtldrsMarker = Nothing
    , _dvtldrsResponseStatus = pResponseStatus_
    }


-- | An array of VTL device objects composed of the Amazon Resource Name(ARN) of the VTL devices.
dvtldrsVTLDevices :: Lens' DescribeVTLDevicesResponse [VTLDevice]
dvtldrsVTLDevices = lens _dvtldrsVTLDevices (\ s a -> s{_dvtldrsVTLDevices = a}) . _Default . _Coerce

-- | Undocumented member.
dvtldrsGatewayARN :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsGatewayARN = lens _dvtldrsGatewayARN (\ s a -> s{_dvtldrsGatewayARN = a})

-- | An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
dvtldrsMarker :: Lens' DescribeVTLDevicesResponse (Maybe Text)
dvtldrsMarker = lens _dvtldrsMarker (\ s a -> s{_dvtldrsMarker = a})

-- | -- | The response status code.
dvtldrsResponseStatus :: Lens' DescribeVTLDevicesResponse Int
dvtldrsResponseStatus = lens _dvtldrsResponseStatus (\ s a -> s{_dvtldrsResponseStatus = a})

instance NFData DescribeVTLDevicesResponse where
