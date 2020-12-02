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
-- Module      : Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a target from a Maintenance Window.
--
--
module Network.AWS.SSM.DeregisterTargetFromMaintenanceWindow
    (
    -- * Creating a Request
      deregisterTargetFromMaintenanceWindow
    , DeregisterTargetFromMaintenanceWindow
    -- * Request Lenses
    , dtfmwSafe
    , dtfmwWindowId
    , dtfmwWindowTargetId

    -- * Destructuring the Response
    , deregisterTargetFromMaintenanceWindowResponse
    , DeregisterTargetFromMaintenanceWindowResponse
    -- * Response Lenses
    , dtfmwrsWindowTargetId
    , dtfmwrsWindowId
    , dtfmwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deregisterTargetFromMaintenanceWindow' smart constructor.
data DeregisterTargetFromMaintenanceWindow = DeregisterTargetFromMaintenanceWindow'
  { _dtfmwSafe           :: !(Maybe Bool)
  , _dtfmwWindowId       :: !Text
  , _dtfmwWindowTargetId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTargetFromMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtfmwSafe' - The system checks if the target is being referenced by a task. If the target is being referenced, the system returns an error and does not deregister the target from the Maintenance Window.
--
-- * 'dtfmwWindowId' - The ID of the Maintenance Window the target should be removed from.
--
-- * 'dtfmwWindowTargetId' - The ID of the target definition to remove.
deregisterTargetFromMaintenanceWindow
    :: Text -- ^ 'dtfmwWindowId'
    -> Text -- ^ 'dtfmwWindowTargetId'
    -> DeregisterTargetFromMaintenanceWindow
deregisterTargetFromMaintenanceWindow pWindowId_ pWindowTargetId_ =
  DeregisterTargetFromMaintenanceWindow'
    { _dtfmwSafe = Nothing
    , _dtfmwWindowId = pWindowId_
    , _dtfmwWindowTargetId = pWindowTargetId_
    }


-- | The system checks if the target is being referenced by a task. If the target is being referenced, the system returns an error and does not deregister the target from the Maintenance Window.
dtfmwSafe :: Lens' DeregisterTargetFromMaintenanceWindow (Maybe Bool)
dtfmwSafe = lens _dtfmwSafe (\ s a -> s{_dtfmwSafe = a})

-- | The ID of the Maintenance Window the target should be removed from.
dtfmwWindowId :: Lens' DeregisterTargetFromMaintenanceWindow Text
dtfmwWindowId = lens _dtfmwWindowId (\ s a -> s{_dtfmwWindowId = a})

-- | The ID of the target definition to remove.
dtfmwWindowTargetId :: Lens' DeregisterTargetFromMaintenanceWindow Text
dtfmwWindowTargetId = lens _dtfmwWindowTargetId (\ s a -> s{_dtfmwWindowTargetId = a})

instance AWSRequest
           DeregisterTargetFromMaintenanceWindow
         where
        type Rs DeregisterTargetFromMaintenanceWindow =
             DeregisterTargetFromMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterTargetFromMaintenanceWindowResponse' <$>
                   (x .?> "WindowTargetId") <*> (x .?> "WindowId") <*>
                     (pure (fromEnum s)))

instance Hashable
           DeregisterTargetFromMaintenanceWindow
         where

instance NFData DeregisterTargetFromMaintenanceWindow
         where

instance ToHeaders
           DeregisterTargetFromMaintenanceWindow
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeregisterTargetFromMaintenanceWindow" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterTargetFromMaintenanceWindow
         where
        toJSON DeregisterTargetFromMaintenanceWindow'{..}
          = object
              (catMaybes
                 [("Safe" .=) <$> _dtfmwSafe,
                  Just ("WindowId" .= _dtfmwWindowId),
                  Just ("WindowTargetId" .= _dtfmwWindowTargetId)])

instance ToPath DeregisterTargetFromMaintenanceWindow
         where
        toPath = const "/"

instance ToQuery
           DeregisterTargetFromMaintenanceWindow
         where
        toQuery = const mempty

-- | /See:/ 'deregisterTargetFromMaintenanceWindowResponse' smart constructor.
data DeregisterTargetFromMaintenanceWindowResponse = DeregisterTargetFromMaintenanceWindowResponse'
  { _dtfmwrsWindowTargetId :: !(Maybe Text)
  , _dtfmwrsWindowId       :: !(Maybe Text)
  , _dtfmwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterTargetFromMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtfmwrsWindowTargetId' - The ID of the removed target definition.
--
-- * 'dtfmwrsWindowId' - The ID of the Maintenance Window the target was removed from.
--
-- * 'dtfmwrsResponseStatus' - -- | The response status code.
deregisterTargetFromMaintenanceWindowResponse
    :: Int -- ^ 'dtfmwrsResponseStatus'
    -> DeregisterTargetFromMaintenanceWindowResponse
deregisterTargetFromMaintenanceWindowResponse pResponseStatus_ =
  DeregisterTargetFromMaintenanceWindowResponse'
    { _dtfmwrsWindowTargetId = Nothing
    , _dtfmwrsWindowId = Nothing
    , _dtfmwrsResponseStatus = pResponseStatus_
    }


-- | The ID of the removed target definition.
dtfmwrsWindowTargetId :: Lens' DeregisterTargetFromMaintenanceWindowResponse (Maybe Text)
dtfmwrsWindowTargetId = lens _dtfmwrsWindowTargetId (\ s a -> s{_dtfmwrsWindowTargetId = a})

-- | The ID of the Maintenance Window the target was removed from.
dtfmwrsWindowId :: Lens' DeregisterTargetFromMaintenanceWindowResponse (Maybe Text)
dtfmwrsWindowId = lens _dtfmwrsWindowId (\ s a -> s{_dtfmwrsWindowId = a})

-- | -- | The response status code.
dtfmwrsResponseStatus :: Lens' DeregisterTargetFromMaintenanceWindowResponse Int
dtfmwrsResponseStatus = lens _dtfmwrsResponseStatus (\ s a -> s{_dtfmwrsResponseStatus = a})

instance NFData
           DeregisterTargetFromMaintenanceWindowResponse
         where
