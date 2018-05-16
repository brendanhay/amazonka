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
-- Module      : Network.AWS.CloudHSMv2.CreateHSM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new hardware security module (HSM) in the specified AWS CloudHSM cluster.
--
--
module Network.AWS.CloudHSMv2.CreateHSM
    (
    -- * Creating a Request
      createHSM
    , CreateHSM
    -- * Request Lenses
    , chIPAddress
    , chClusterId
    , chAvailabilityZone

    -- * Destructuring the Response
    , createHSMResponse
    , CreateHSMResponse
    -- * Response Lenses
    , chrsHSM
    , chrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createHSM' smart constructor.
data CreateHSM = CreateHSM'
  { _chIPAddress        :: !(Maybe Text)
  , _chClusterId        :: !Text
  , _chAvailabilityZone :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chIPAddress' - The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
--
-- * 'chClusterId' - The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
--
-- * 'chAvailabilityZone' - The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
createHSM
    :: Text -- ^ 'chClusterId'
    -> Text -- ^ 'chAvailabilityZone'
    -> CreateHSM
createHSM pClusterId_ pAvailabilityZone_ =
  CreateHSM'
    { _chIPAddress = Nothing
    , _chClusterId = pClusterId_
    , _chAvailabilityZone = pAvailabilityZone_
    }


-- | The HSM's IP address. If you specify an IP address, use an available address from the subnet that maps to the Availability Zone where you are creating the HSM. If you don't specify an IP address, one is chosen for you from that subnet.
chIPAddress :: Lens' CreateHSM (Maybe Text)
chIPAddress = lens _chIPAddress (\ s a -> s{_chIPAddress = a})

-- | The identifier (ID) of the HSM's cluster. To find the cluster ID, use 'DescribeClusters' .
chClusterId :: Lens' CreateHSM Text
chClusterId = lens _chClusterId (\ s a -> s{_chClusterId = a})

-- | The Availability Zone where you are creating the HSM. To find the cluster's Availability Zones, use 'DescribeClusters' .
chAvailabilityZone :: Lens' CreateHSM Text
chAvailabilityZone = lens _chAvailabilityZone (\ s a -> s{_chAvailabilityZone = a})

instance AWSRequest CreateHSM where
        type Rs CreateHSM = CreateHSMResponse
        request = postJSON cloudHSMv2
        response
          = receiveJSON
              (\ s h x ->
                 CreateHSMResponse' <$>
                   (x .?> "Hsm") <*> (pure (fromEnum s)))

instance Hashable CreateHSM where

instance NFData CreateHSM where

instance ToHeaders CreateHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("BaldrApiService.CreateHsm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHSM where
        toJSON CreateHSM'{..}
          = object
              (catMaybes
                 [("IpAddress" .=) <$> _chIPAddress,
                  Just ("ClusterId" .= _chClusterId),
                  Just ("AvailabilityZone" .= _chAvailabilityZone)])

instance ToPath CreateHSM where
        toPath = const "/"

instance ToQuery CreateHSM where
        toQuery = const mempty

-- | /See:/ 'createHSMResponse' smart constructor.
data CreateHSMResponse = CreateHSMResponse'
  { _chrsHSM            :: !(Maybe HSM)
  , _chrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chrsHSM' - Information about the HSM that was created.
--
-- * 'chrsResponseStatus' - -- | The response status code.
createHSMResponse
    :: Int -- ^ 'chrsResponseStatus'
    -> CreateHSMResponse
createHSMResponse pResponseStatus_ =
  CreateHSMResponse'
    {_chrsHSM = Nothing, _chrsResponseStatus = pResponseStatus_}


-- | Information about the HSM that was created.
chrsHSM :: Lens' CreateHSMResponse (Maybe HSM)
chrsHSM = lens _chrsHSM (\ s a -> s{_chrsHSM = a})

-- | -- | The response status code.
chrsResponseStatus :: Lens' CreateHSMResponse Int
chrsResponseStatus = lens _chrsResponseStatus (\ s a -> s{_chrsResponseStatus = a})

instance NFData CreateHSMResponse where
