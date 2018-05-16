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
-- Module      : Network.AWS.DeviceFarm.UpdateNetworkProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the network profile with specific settings.
--
--
module Network.AWS.DeviceFarm.UpdateNetworkProfile
    (
    -- * Creating a Request
      updateNetworkProfile
    , UpdateNetworkProfile
    -- * Request Lenses
    , unpUplinkJitterMs
    , unpUplinkLossPercent
    , unpDownlinkJitterMs
    , unpName
    , unpDownlinkLossPercent
    , unpType
    , unpUplinkDelayMs
    , unpUplinkBandwidthBits
    , unpDescription
    , unpDownlinkDelayMs
    , unpDownlinkBandwidthBits
    , unpArn

    -- * Destructuring the Response
    , updateNetworkProfileResponse
    , UpdateNetworkProfileResponse
    -- * Response Lenses
    , unprsNetworkProfile
    , unprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { _unpUplinkJitterMs        :: !(Maybe Integer)
  , _unpUplinkLossPercent     :: !(Maybe Nat)
  , _unpDownlinkJitterMs      :: !(Maybe Integer)
  , _unpName                  :: !(Maybe Text)
  , _unpDownlinkLossPercent   :: !(Maybe Nat)
  , _unpType                  :: !(Maybe NetworkProfileType)
  , _unpUplinkDelayMs         :: !(Maybe Integer)
  , _unpUplinkBandwidthBits   :: !(Maybe Integer)
  , _unpDescription           :: !(Maybe Text)
  , _unpDownlinkDelayMs       :: !(Maybe Integer)
  , _unpDownlinkBandwidthBits :: !(Maybe Integer)
  , _unpArn                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unpUplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'unpUplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- * 'unpDownlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'unpName' - The name of the network profile about which you are returning information.
--
-- * 'unpDownlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- * 'unpType' - The type of network profile you wish to return information about. Valid values are listed below.
--
-- * 'unpUplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'unpUplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- * 'unpDescription' - The descriptoin of the network profile about which you are returning information.
--
-- * 'unpDownlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'unpDownlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- * 'unpArn' - The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
updateNetworkProfile
    :: Text -- ^ 'unpArn'
    -> UpdateNetworkProfile
updateNetworkProfile pArn_ =
  UpdateNetworkProfile'
    { _unpUplinkJitterMs = Nothing
    , _unpUplinkLossPercent = Nothing
    , _unpDownlinkJitterMs = Nothing
    , _unpName = Nothing
    , _unpDownlinkLossPercent = Nothing
    , _unpType = Nothing
    , _unpUplinkDelayMs = Nothing
    , _unpUplinkBandwidthBits = Nothing
    , _unpDescription = Nothing
    , _unpDownlinkDelayMs = Nothing
    , _unpDownlinkBandwidthBits = Nothing
    , _unpArn = pArn_
    }


-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
unpUplinkJitterMs :: Lens' UpdateNetworkProfile (Maybe Integer)
unpUplinkJitterMs = lens _unpUplinkJitterMs (\ s a -> s{_unpUplinkJitterMs = a})

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
unpUplinkLossPercent :: Lens' UpdateNetworkProfile (Maybe Natural)
unpUplinkLossPercent = lens _unpUplinkLossPercent (\ s a -> s{_unpUplinkLossPercent = a}) . mapping _Nat

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
unpDownlinkJitterMs :: Lens' UpdateNetworkProfile (Maybe Integer)
unpDownlinkJitterMs = lens _unpDownlinkJitterMs (\ s a -> s{_unpDownlinkJitterMs = a})

-- | The name of the network profile about which you are returning information.
unpName :: Lens' UpdateNetworkProfile (Maybe Text)
unpName = lens _unpName (\ s a -> s{_unpName = a})

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
unpDownlinkLossPercent :: Lens' UpdateNetworkProfile (Maybe Natural)
unpDownlinkLossPercent = lens _unpDownlinkLossPercent (\ s a -> s{_unpDownlinkLossPercent = a}) . mapping _Nat

-- | The type of network profile you wish to return information about. Valid values are listed below.
unpType :: Lens' UpdateNetworkProfile (Maybe NetworkProfileType)
unpType = lens _unpType (\ s a -> s{_unpType = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
unpUplinkDelayMs :: Lens' UpdateNetworkProfile (Maybe Integer)
unpUplinkDelayMs = lens _unpUplinkDelayMs (\ s a -> s{_unpUplinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
unpUplinkBandwidthBits :: Lens' UpdateNetworkProfile (Maybe Integer)
unpUplinkBandwidthBits = lens _unpUplinkBandwidthBits (\ s a -> s{_unpUplinkBandwidthBits = a})

-- | The descriptoin of the network profile about which you are returning information.
unpDescription :: Lens' UpdateNetworkProfile (Maybe Text)
unpDescription = lens _unpDescription (\ s a -> s{_unpDescription = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
unpDownlinkDelayMs :: Lens' UpdateNetworkProfile (Maybe Integer)
unpDownlinkDelayMs = lens _unpDownlinkDelayMs (\ s a -> s{_unpDownlinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
unpDownlinkBandwidthBits :: Lens' UpdateNetworkProfile (Maybe Integer)
unpDownlinkBandwidthBits = lens _unpDownlinkBandwidthBits (\ s a -> s{_unpDownlinkBandwidthBits = a})

-- | The Amazon Resource Name (ARN) of the project for which you want to update network profile settings.
unpArn :: Lens' UpdateNetworkProfile Text
unpArn = lens _unpArn (\ s a -> s{_unpArn = a})

instance AWSRequest UpdateNetworkProfile where
        type Rs UpdateNetworkProfile =
             UpdateNetworkProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateNetworkProfileResponse' <$>
                   (x .?> "networkProfile") <*> (pure (fromEnum s)))

instance Hashable UpdateNetworkProfile where

instance NFData UpdateNetworkProfile where

instance ToHeaders UpdateNetworkProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateNetworkProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNetworkProfile where
        toJSON UpdateNetworkProfile'{..}
          = object
              (catMaybes
                 [("uplinkJitterMs" .=) <$> _unpUplinkJitterMs,
                  ("uplinkLossPercent" .=) <$> _unpUplinkLossPercent,
                  ("downlinkJitterMs" .=) <$> _unpDownlinkJitterMs,
                  ("name" .=) <$> _unpName,
                  ("downlinkLossPercent" .=) <$>
                    _unpDownlinkLossPercent,
                  ("type" .=) <$> _unpType,
                  ("uplinkDelayMs" .=) <$> _unpUplinkDelayMs,
                  ("uplinkBandwidthBits" .=) <$>
                    _unpUplinkBandwidthBits,
                  ("description" .=) <$> _unpDescription,
                  ("downlinkDelayMs" .=) <$> _unpDownlinkDelayMs,
                  ("downlinkBandwidthBits" .=) <$>
                    _unpDownlinkBandwidthBits,
                  Just ("arn" .= _unpArn)])

instance ToPath UpdateNetworkProfile where
        toPath = const "/"

instance ToQuery UpdateNetworkProfile where
        toQuery = const mempty

-- | /See:/ 'updateNetworkProfileResponse' smart constructor.
data UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { _unprsNetworkProfile :: !(Maybe NetworkProfile)
  , _unprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unprsNetworkProfile' - A list of the available network profiles.
--
-- * 'unprsResponseStatus' - -- | The response status code.
updateNetworkProfileResponse
    :: Int -- ^ 'unprsResponseStatus'
    -> UpdateNetworkProfileResponse
updateNetworkProfileResponse pResponseStatus_ =
  UpdateNetworkProfileResponse'
    {_unprsNetworkProfile = Nothing, _unprsResponseStatus = pResponseStatus_}


-- | A list of the available network profiles.
unprsNetworkProfile :: Lens' UpdateNetworkProfileResponse (Maybe NetworkProfile)
unprsNetworkProfile = lens _unprsNetworkProfile (\ s a -> s{_unprsNetworkProfile = a})

-- | -- | The response status code.
unprsResponseStatus :: Lens' UpdateNetworkProfileResponse Int
unprsResponseStatus = lens _unprsResponseStatus (\ s a -> s{_unprsResponseStatus = a})

instance NFData UpdateNetworkProfileResponse where
