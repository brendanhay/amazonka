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
-- Module      : Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the discovery of any smart home appliances associated with the room.
--
--
module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
    (
    -- * Creating a Request
      startSmartHomeApplianceDiscovery
    , StartSmartHomeApplianceDiscovery
    -- * Request Lenses
    , sshadRoomARN

    -- * Destructuring the Response
    , startSmartHomeApplianceDiscoveryResponse
    , StartSmartHomeApplianceDiscoveryResponse
    -- * Response Lenses
    , sshadrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startSmartHomeApplianceDiscovery' smart constructor.
newtype StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { _sshadRoomARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSmartHomeApplianceDiscovery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sshadRoomARN' - The room where smart home appliance discovery was initiated.
startSmartHomeApplianceDiscovery
    :: Text -- ^ 'sshadRoomARN'
    -> StartSmartHomeApplianceDiscovery
startSmartHomeApplianceDiscovery pRoomARN_ =
  StartSmartHomeApplianceDiscovery' {_sshadRoomARN = pRoomARN_}


-- | The room where smart home appliance discovery was initiated.
sshadRoomARN :: Lens' StartSmartHomeApplianceDiscovery Text
sshadRoomARN = lens _sshadRoomARN (\ s a -> s{_sshadRoomARN = a})

instance AWSRequest StartSmartHomeApplianceDiscovery
         where
        type Rs StartSmartHomeApplianceDiscovery =
             StartSmartHomeApplianceDiscoveryResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 StartSmartHomeApplianceDiscoveryResponse' <$>
                   (pure (fromEnum s)))

instance Hashable StartSmartHomeApplianceDiscovery
         where

instance NFData StartSmartHomeApplianceDiscovery
         where

instance ToHeaders StartSmartHomeApplianceDiscovery
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.StartSmartHomeApplianceDiscovery"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartSmartHomeApplianceDiscovery
         where
        toJSON StartSmartHomeApplianceDiscovery'{..}
          = object
              (catMaybes [Just ("RoomArn" .= _sshadRoomARN)])

instance ToPath StartSmartHomeApplianceDiscovery
         where
        toPath = const "/"

instance ToQuery StartSmartHomeApplianceDiscovery
         where
        toQuery = const mempty

-- | /See:/ 'startSmartHomeApplianceDiscoveryResponse' smart constructor.
newtype StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { _sshadrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSmartHomeApplianceDiscoveryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sshadrsResponseStatus' - -- | The response status code.
startSmartHomeApplianceDiscoveryResponse
    :: Int -- ^ 'sshadrsResponseStatus'
    -> StartSmartHomeApplianceDiscoveryResponse
startSmartHomeApplianceDiscoveryResponse pResponseStatus_ =
  StartSmartHomeApplianceDiscoveryResponse'
    {_sshadrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sshadrsResponseStatus :: Lens' StartSmartHomeApplianceDiscoveryResponse Int
sshadrsResponseStatus = lens _sshadrsResponseStatus (\ s a -> s{_sshadrsResponseStatus = a})

instance NFData
           StartSmartHomeApplianceDiscoveryResponse
         where
