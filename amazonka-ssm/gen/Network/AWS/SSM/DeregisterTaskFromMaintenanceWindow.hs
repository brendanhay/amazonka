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
-- Module      : Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a task from a Maintenance Window.
--
--
module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
    (
    -- * Creating a Request
      deregisterTaskFromMaintenanceWindow
    , DeregisterTaskFromMaintenanceWindow
    -- * Request Lenses
    , derWindowId
    , derWindowTaskId

    -- * Destructuring the Response
    , deregisterTaskFromMaintenanceWindowResponse
    , DeregisterTaskFromMaintenanceWindowResponse
    -- * Response Lenses
    , derrsWindowTaskId
    , derrsWindowId
    , derrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { _derWindowId     :: !Text
  , _derWindowTaskId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTaskFromMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derWindowId' - The ID of the Maintenance Window the task should be removed from.
--
-- * 'derWindowTaskId' - The ID of the task to remove from the Maintenance Window.
deregisterTaskFromMaintenanceWindow
    :: Text -- ^ 'derWindowId'
    -> Text -- ^ 'derWindowTaskId'
    -> DeregisterTaskFromMaintenanceWindow
deregisterTaskFromMaintenanceWindow pWindowId_ pWindowTaskId_ =
  DeregisterTaskFromMaintenanceWindow'
    {_derWindowId = pWindowId_, _derWindowTaskId = pWindowTaskId_}


-- | The ID of the Maintenance Window the task should be removed from.
derWindowId :: Lens' DeregisterTaskFromMaintenanceWindow Text
derWindowId = lens _derWindowId (\ s a -> s{_derWindowId = a})

-- | The ID of the task to remove from the Maintenance Window.
derWindowTaskId :: Lens' DeregisterTaskFromMaintenanceWindow Text
derWindowTaskId = lens _derWindowTaskId (\ s a -> s{_derWindowTaskId = a})

instance AWSRequest
           DeregisterTaskFromMaintenanceWindow
         where
        type Rs DeregisterTaskFromMaintenanceWindow =
             DeregisterTaskFromMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterTaskFromMaintenanceWindowResponse' <$>
                   (x .?> "WindowTaskId") <*> (x .?> "WindowId") <*>
                     (pure (fromEnum s)))

instance Hashable DeregisterTaskFromMaintenanceWindow
         where

instance NFData DeregisterTaskFromMaintenanceWindow
         where

instance ToHeaders
           DeregisterTaskFromMaintenanceWindow
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeregisterTaskFromMaintenanceWindow" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterTaskFromMaintenanceWindow
         where
        toJSON DeregisterTaskFromMaintenanceWindow'{..}
          = object
              (catMaybes
                 [Just ("WindowId" .= _derWindowId),
                  Just ("WindowTaskId" .= _derWindowTaskId)])

instance ToPath DeregisterTaskFromMaintenanceWindow
         where
        toPath = const "/"

instance ToQuery DeregisterTaskFromMaintenanceWindow
         where
        toQuery = const mempty

-- | /See:/ 'deregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { _derrsWindowTaskId   :: !(Maybe Text)
  , _derrsWindowId       :: !(Maybe Text)
  , _derrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTaskFromMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsWindowTaskId' - The ID of the task removed from the Maintenance Window.
--
-- * 'derrsWindowId' - The ID of the Maintenance Window the task was removed from.
--
-- * 'derrsResponseStatus' - -- | The response status code.
deregisterTaskFromMaintenanceWindowResponse
    :: Int -- ^ 'derrsResponseStatus'
    -> DeregisterTaskFromMaintenanceWindowResponse
deregisterTaskFromMaintenanceWindowResponse pResponseStatus_ =
  DeregisterTaskFromMaintenanceWindowResponse'
    { _derrsWindowTaskId = Nothing
    , _derrsWindowId = Nothing
    , _derrsResponseStatus = pResponseStatus_
    }


-- | The ID of the task removed from the Maintenance Window.
derrsWindowTaskId :: Lens' DeregisterTaskFromMaintenanceWindowResponse (Maybe Text)
derrsWindowTaskId = lens _derrsWindowTaskId (\ s a -> s{_derrsWindowTaskId = a})

-- | The ID of the Maintenance Window the task was removed from.
derrsWindowId :: Lens' DeregisterTaskFromMaintenanceWindowResponse (Maybe Text)
derrsWindowId = lens _derrsWindowId (\ s a -> s{_derrsWindowId = a})

-- | -- | The response status code.
derrsResponseStatus :: Lens' DeregisterTaskFromMaintenanceWindowResponse Int
derrsResponseStatus = lens _derrsResponseStatus (\ s a -> s{_derrsResponseStatus = a})

instance NFData
           DeregisterTaskFromMaintenanceWindowResponse
         where
