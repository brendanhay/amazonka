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
-- Module      : Network.AWS.DeviceFarm.UpdateDevicePool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the name, description, and rules in a device pool given the attributes and the pool ARN. Rule updates are all-or-nothing, meaning they can only be updated as a whole (or not at all).
--
--
module Network.AWS.DeviceFarm.UpdateDevicePool
    (
    -- * Creating a Request
      updateDevicePool
    , UpdateDevicePool
    -- * Request Lenses
    , udpRules
    , udpName
    , udpDescription
    , udpArn

    -- * Destructuring the Response
    , updateDevicePoolResponse
    , UpdateDevicePoolResponse
    -- * Response Lenses
    , udprsDevicePool
    , udprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the update device pool operation.
--
--
--
-- /See:/ 'updateDevicePool' smart constructor.
data UpdateDevicePool = UpdateDevicePool'
  { _udpRules       :: !(Maybe [Rule])
  , _udpName        :: !(Maybe Text)
  , _udpDescription :: !(Maybe Text)
  , _udpArn         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udpRules' - Represents the rules you wish to modify for the device pool. Updating rules is optional; however, if you choose to update rules for your request, the update will replace the existing rules.
--
-- * 'udpName' - A string representing the name of the device pool you wish to update.
--
-- * 'udpDescription' - A description of the device pool you wish to update.
--
-- * 'udpArn' - The Amazon Resourc Name (ARN) of the Device Farm device pool you wish to update.
updateDevicePool
    :: Text -- ^ 'udpArn'
    -> UpdateDevicePool
updateDevicePool pArn_ =
  UpdateDevicePool'
    { _udpRules = Nothing
    , _udpName = Nothing
    , _udpDescription = Nothing
    , _udpArn = pArn_
    }


-- | Represents the rules you wish to modify for the device pool. Updating rules is optional; however, if you choose to update rules for your request, the update will replace the existing rules.
udpRules :: Lens' UpdateDevicePool [Rule]
udpRules = lens _udpRules (\ s a -> s{_udpRules = a}) . _Default . _Coerce

-- | A string representing the name of the device pool you wish to update.
udpName :: Lens' UpdateDevicePool (Maybe Text)
udpName = lens _udpName (\ s a -> s{_udpName = a})

-- | A description of the device pool you wish to update.
udpDescription :: Lens' UpdateDevicePool (Maybe Text)
udpDescription = lens _udpDescription (\ s a -> s{_udpDescription = a})

-- | The Amazon Resourc Name (ARN) of the Device Farm device pool you wish to update.
udpArn :: Lens' UpdateDevicePool Text
udpArn = lens _udpArn (\ s a -> s{_udpArn = a})

instance AWSRequest UpdateDevicePool where
        type Rs UpdateDevicePool = UpdateDevicePoolResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDevicePoolResponse' <$>
                   (x .?> "devicePool") <*> (pure (fromEnum s)))

instance Hashable UpdateDevicePool where

instance NFData UpdateDevicePool where

instance ToHeaders UpdateDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateDevicePool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDevicePool where
        toJSON UpdateDevicePool'{..}
          = object
              (catMaybes
                 [("rules" .=) <$> _udpRules,
                  ("name" .=) <$> _udpName,
                  ("description" .=) <$> _udpDescription,
                  Just ("arn" .= _udpArn)])

instance ToPath UpdateDevicePool where
        toPath = const "/"

instance ToQuery UpdateDevicePool where
        toQuery = const mempty

-- | Represents the result of an update device pool request.
--
--
--
-- /See:/ 'updateDevicePoolResponse' smart constructor.
data UpdateDevicePoolResponse = UpdateDevicePoolResponse'
  { _udprsDevicePool     :: !(Maybe DevicePool)
  , _udprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDevicePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udprsDevicePool' - The device pool you just updated.
--
-- * 'udprsResponseStatus' - -- | The response status code.
updateDevicePoolResponse
    :: Int -- ^ 'udprsResponseStatus'
    -> UpdateDevicePoolResponse
updateDevicePoolResponse pResponseStatus_ =
  UpdateDevicePoolResponse'
    {_udprsDevicePool = Nothing, _udprsResponseStatus = pResponseStatus_}


-- | The device pool you just updated.
udprsDevicePool :: Lens' UpdateDevicePoolResponse (Maybe DevicePool)
udprsDevicePool = lens _udprsDevicePool (\ s a -> s{_udprsDevicePool = a})

-- | -- | The response status code.
udprsResponseStatus :: Lens' UpdateDevicePoolResponse Int
udprsResponseStatus = lens _udprsResponseStatus (\ s a -> s{_udprsResponseStatus = a})

instance NFData UpdateDevicePoolResponse where
