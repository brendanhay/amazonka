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
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.
--
--
module Network.AWS.DeviceFarm.DeleteDevicePool
    (
    -- * Creating a Request
      deleteDevicePool
    , DeleteDevicePool
    -- * Request Lenses
    , ddpArn

    -- * Destructuring the Response
    , deleteDevicePoolResponse
    , DeleteDevicePoolResponse
    -- * Response Lenses
    , ddprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the delete device pool operation.
--
--
--
-- /See:/ 'deleteDevicePool' smart constructor.
newtype DeleteDevicePool = DeleteDevicePool'
  { _ddpArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpArn' - Represents the Amazon Resource Name (ARN) of the Device Farm device pool you wish to delete.
deleteDevicePool
    :: Text -- ^ 'ddpArn'
    -> DeleteDevicePool
deleteDevicePool pArn_ = DeleteDevicePool' {_ddpArn = pArn_}


-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool you wish to delete.
ddpArn :: Lens' DeleteDevicePool Text
ddpArn = lens _ddpArn (\ s a -> s{_ddpArn = a})

instance AWSRequest DeleteDevicePool where
        type Rs DeleteDevicePool = DeleteDevicePoolResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDevicePoolResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDevicePool where

instance NFData DeleteDevicePool where

instance ToHeaders DeleteDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteDevicePool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDevicePool where
        toJSON DeleteDevicePool'{..}
          = object (catMaybes [Just ("arn" .= _ddpArn)])

instance ToPath DeleteDevicePool where
        toPath = const "/"

instance ToQuery DeleteDevicePool where
        toQuery = const mempty

-- | Represents the result of a delete device pool request.
--
--
--
-- /See:/ 'deleteDevicePoolResponse' smart constructor.
newtype DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { _ddprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDevicePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddprsResponseStatus' - -- | The response status code.
deleteDevicePoolResponse
    :: Int -- ^ 'ddprsResponseStatus'
    -> DeleteDevicePoolResponse
deleteDevicePoolResponse pResponseStatus_ =
  DeleteDevicePoolResponse' {_ddprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddprsResponseStatus :: Lens' DeleteDevicePoolResponse Int
ddprsResponseStatus = lens _ddprsResponseStatus (\ s a -> s{_ddprsResponseStatus = a})

instance NFData DeleteDevicePoolResponse where
