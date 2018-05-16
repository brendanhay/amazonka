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
-- Module      : Network.AWS.DeviceFarm.CreateNetworkProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network profile.
--
--
module Network.AWS.DeviceFarm.CreateNetworkProfile
    (
    -- * Creating a Request
      createNetworkProfile
    , CreateNetworkProfile
    -- * Request Lenses
    , cnpUplinkJitterMs
    , cnpUplinkLossPercent
    , cnpDownlinkJitterMs
    , cnpDownlinkLossPercent
    , cnpType
    , cnpUplinkDelayMs
    , cnpUplinkBandwidthBits
    , cnpDescription
    , cnpDownlinkDelayMs
    , cnpDownlinkBandwidthBits
    , cnpProjectARN
    , cnpName

    -- * Destructuring the Response
    , createNetworkProfileResponse
    , CreateNetworkProfileResponse
    -- * Response Lenses
    , cnprsNetworkProfile
    , cnprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createNetworkProfile' smart constructor.
data CreateNetworkProfile = CreateNetworkProfile'
  { _cnpUplinkJitterMs        :: !(Maybe Integer)
  , _cnpUplinkLossPercent     :: !(Maybe Nat)
  , _cnpDownlinkJitterMs      :: !(Maybe Integer)
  , _cnpDownlinkLossPercent   :: !(Maybe Nat)
  , _cnpType                  :: !(Maybe NetworkProfileType)
  , _cnpUplinkDelayMs         :: !(Maybe Integer)
  , _cnpUplinkBandwidthBits   :: !(Maybe Integer)
  , _cnpDescription           :: !(Maybe Text)
  , _cnpDownlinkDelayMs       :: !(Maybe Integer)
  , _cnpDownlinkBandwidthBits :: !(Maybe Integer)
  , _cnpProjectARN            :: !Text
  , _cnpName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnpUplinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'cnpUplinkLossPercent' - Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
--
-- * 'cnpDownlinkJitterMs' - Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
--
-- * 'cnpDownlinkLossPercent' - Proportion of received packets that fail to arrive from 0 to 100 percent.
--
-- * 'cnpType' - The type of network profile you wish to create. Valid values are listed below.
--
-- * 'cnpUplinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'cnpUplinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- * 'cnpDescription' - The description of the network profile.
--
-- * 'cnpDownlinkDelayMs' - Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
--
-- * 'cnpDownlinkBandwidthBits' - The data throughput rate in bits per second, as an integer from 0 to 104857600.
--
-- * 'cnpProjectARN' - The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
--
-- * 'cnpName' - The name you wish to specify for the new network profile.
createNetworkProfile
    :: Text -- ^ 'cnpProjectARN'
    -> Text -- ^ 'cnpName'
    -> CreateNetworkProfile
createNetworkProfile pProjectARN_ pName_ =
  CreateNetworkProfile'
    { _cnpUplinkJitterMs = Nothing
    , _cnpUplinkLossPercent = Nothing
    , _cnpDownlinkJitterMs = Nothing
    , _cnpDownlinkLossPercent = Nothing
    , _cnpType = Nothing
    , _cnpUplinkDelayMs = Nothing
    , _cnpUplinkBandwidthBits = Nothing
    , _cnpDescription = Nothing
    , _cnpDownlinkDelayMs = Nothing
    , _cnpDownlinkBandwidthBits = Nothing
    , _cnpProjectARN = pProjectARN_
    , _cnpName = pName_
    }


-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
cnpUplinkJitterMs :: Lens' CreateNetworkProfile (Maybe Integer)
cnpUplinkJitterMs = lens _cnpUplinkJitterMs (\ s a -> s{_cnpUplinkJitterMs = a})

-- | Proportion of transmitted packets that fail to arrive from 0 to 100 percent.
cnpUplinkLossPercent :: Lens' CreateNetworkProfile (Maybe Natural)
cnpUplinkLossPercent = lens _cnpUplinkLossPercent (\ s a -> s{_cnpUplinkLossPercent = a}) . mapping _Nat

-- | Time variation in the delay of received packets in milliseconds as an integer from 0 to 2000.
cnpDownlinkJitterMs :: Lens' CreateNetworkProfile (Maybe Integer)
cnpDownlinkJitterMs = lens _cnpDownlinkJitterMs (\ s a -> s{_cnpDownlinkJitterMs = a})

-- | Proportion of received packets that fail to arrive from 0 to 100 percent.
cnpDownlinkLossPercent :: Lens' CreateNetworkProfile (Maybe Natural)
cnpDownlinkLossPercent = lens _cnpDownlinkLossPercent (\ s a -> s{_cnpDownlinkLossPercent = a}) . mapping _Nat

-- | The type of network profile you wish to create. Valid values are listed below.
cnpType :: Lens' CreateNetworkProfile (Maybe NetworkProfileType)
cnpType = lens _cnpType (\ s a -> s{_cnpType = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
cnpUplinkDelayMs :: Lens' CreateNetworkProfile (Maybe Integer)
cnpUplinkDelayMs = lens _cnpUplinkDelayMs (\ s a -> s{_cnpUplinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
cnpUplinkBandwidthBits :: Lens' CreateNetworkProfile (Maybe Integer)
cnpUplinkBandwidthBits = lens _cnpUplinkBandwidthBits (\ s a -> s{_cnpUplinkBandwidthBits = a})

-- | The description of the network profile.
cnpDescription :: Lens' CreateNetworkProfile (Maybe Text)
cnpDescription = lens _cnpDescription (\ s a -> s{_cnpDescription = a})

-- | Delay time for all packets to destination in milliseconds as an integer from 0 to 2000.
cnpDownlinkDelayMs :: Lens' CreateNetworkProfile (Maybe Integer)
cnpDownlinkDelayMs = lens _cnpDownlinkDelayMs (\ s a -> s{_cnpDownlinkDelayMs = a})

-- | The data throughput rate in bits per second, as an integer from 0 to 104857600.
cnpDownlinkBandwidthBits :: Lens' CreateNetworkProfile (Maybe Integer)
cnpDownlinkBandwidthBits = lens _cnpDownlinkBandwidthBits (\ s a -> s{_cnpDownlinkBandwidthBits = a})

-- | The Amazon Resource Name (ARN) of the project for which you want to create a network profile.
cnpProjectARN :: Lens' CreateNetworkProfile Text
cnpProjectARN = lens _cnpProjectARN (\ s a -> s{_cnpProjectARN = a})

-- | The name you wish to specify for the new network profile.
cnpName :: Lens' CreateNetworkProfile Text
cnpName = lens _cnpName (\ s a -> s{_cnpName = a})

instance AWSRequest CreateNetworkProfile where
        type Rs CreateNetworkProfile =
             CreateNetworkProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateNetworkProfileResponse' <$>
                   (x .?> "networkProfile") <*> (pure (fromEnum s)))

instance Hashable CreateNetworkProfile where

instance NFData CreateNetworkProfile where

instance ToHeaders CreateNetworkProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateNetworkProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNetworkProfile where
        toJSON CreateNetworkProfile'{..}
          = object
              (catMaybes
                 [("uplinkJitterMs" .=) <$> _cnpUplinkJitterMs,
                  ("uplinkLossPercent" .=) <$> _cnpUplinkLossPercent,
                  ("downlinkJitterMs" .=) <$> _cnpDownlinkJitterMs,
                  ("downlinkLossPercent" .=) <$>
                    _cnpDownlinkLossPercent,
                  ("type" .=) <$> _cnpType,
                  ("uplinkDelayMs" .=) <$> _cnpUplinkDelayMs,
                  ("uplinkBandwidthBits" .=) <$>
                    _cnpUplinkBandwidthBits,
                  ("description" .=) <$> _cnpDescription,
                  ("downlinkDelayMs" .=) <$> _cnpDownlinkDelayMs,
                  ("downlinkBandwidthBits" .=) <$>
                    _cnpDownlinkBandwidthBits,
                  Just ("projectArn" .= _cnpProjectARN),
                  Just ("name" .= _cnpName)])

instance ToPath CreateNetworkProfile where
        toPath = const "/"

instance ToQuery CreateNetworkProfile where
        toQuery = const mempty

-- | /See:/ 'createNetworkProfileResponse' smart constructor.
data CreateNetworkProfileResponse = CreateNetworkProfileResponse'
  { _cnprsNetworkProfile :: !(Maybe NetworkProfile)
  , _cnprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnprsNetworkProfile' - The network profile that is returned by the create network profile request.
--
-- * 'cnprsResponseStatus' - -- | The response status code.
createNetworkProfileResponse
    :: Int -- ^ 'cnprsResponseStatus'
    -> CreateNetworkProfileResponse
createNetworkProfileResponse pResponseStatus_ =
  CreateNetworkProfileResponse'
    {_cnprsNetworkProfile = Nothing, _cnprsResponseStatus = pResponseStatus_}


-- | The network profile that is returned by the create network profile request.
cnprsNetworkProfile :: Lens' CreateNetworkProfileResponse (Maybe NetworkProfile)
cnprsNetworkProfile = lens _cnprsNetworkProfile (\ s a -> s{_cnprsNetworkProfile = a})

-- | -- | The response status code.
cnprsResponseStatus :: Lens' CreateNetworkProfileResponse Int
cnprsResponseStatus = lens _cnprsResponseStatus (\ s a -> s{_cnprsResponseStatus = a})

instance NFData CreateNetworkProfileResponse where
