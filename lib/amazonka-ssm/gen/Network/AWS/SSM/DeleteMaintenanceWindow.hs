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
-- Module      : Network.AWS.SSM.DeleteMaintenanceWindow
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Maintenance Window.
--
--
module Network.AWS.SSM.DeleteMaintenanceWindow
    (
    -- * Creating a Request
      deleteMaintenanceWindow
    , DeleteMaintenanceWindow
    -- * Request Lenses
    , dmwWindowId

    -- * Destructuring the Response
    , deleteMaintenanceWindowResponse
    , DeleteMaintenanceWindowResponse
    -- * Response Lenses
    , dmwrsWindowId
    , dmwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteMaintenanceWindow' smart constructor.
newtype DeleteMaintenanceWindow = DeleteMaintenanceWindow'
  { _dmwWindowId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwWindowId' - The ID of the Maintenance Window to delete.
deleteMaintenanceWindow
    :: Text -- ^ 'dmwWindowId'
    -> DeleteMaintenanceWindow
deleteMaintenanceWindow pWindowId_ =
  DeleteMaintenanceWindow' {_dmwWindowId = pWindowId_}


-- | The ID of the Maintenance Window to delete.
dmwWindowId :: Lens' DeleteMaintenanceWindow Text
dmwWindowId = lens _dmwWindowId (\ s a -> s{_dmwWindowId = a})

instance AWSRequest DeleteMaintenanceWindow where
        type Rs DeleteMaintenanceWindow =
             DeleteMaintenanceWindowResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMaintenanceWindowResponse' <$>
                   (x .?> "WindowId") <*> (pure (fromEnum s)))

instance Hashable DeleteMaintenanceWindow where

instance NFData DeleteMaintenanceWindow where

instance ToHeaders DeleteMaintenanceWindow where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteMaintenanceWindow" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteMaintenanceWindow where
        toJSON DeleteMaintenanceWindow'{..}
          = object
              (catMaybes [Just ("WindowId" .= _dmwWindowId)])

instance ToPath DeleteMaintenanceWindow where
        toPath = const "/"

instance ToQuery DeleteMaintenanceWindow where
        toQuery = const mempty

-- | /See:/ 'deleteMaintenanceWindowResponse' smart constructor.
data DeleteMaintenanceWindowResponse = DeleteMaintenanceWindowResponse'
  { _dmwrsWindowId       :: !(Maybe Text)
  , _dmwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwrsWindowId' - The ID of the deleted Maintenance Window.
--
-- * 'dmwrsResponseStatus' - -- | The response status code.
deleteMaintenanceWindowResponse
    :: Int -- ^ 'dmwrsResponseStatus'
    -> DeleteMaintenanceWindowResponse
deleteMaintenanceWindowResponse pResponseStatus_ =
  DeleteMaintenanceWindowResponse'
    {_dmwrsWindowId = Nothing, _dmwrsResponseStatus = pResponseStatus_}


-- | The ID of the deleted Maintenance Window.
dmwrsWindowId :: Lens' DeleteMaintenanceWindowResponse (Maybe Text)
dmwrsWindowId = lens _dmwrsWindowId (\ s a -> s{_dmwrsWindowId = a})

-- | -- | The response status code.
dmwrsResponseStatus :: Lens' DeleteMaintenanceWindowResponse Int
dmwrsResponseStatus = lens _dmwrsResponseStatus (\ s a -> s{_dmwrsResponseStatus = a})

instance NFData DeleteMaintenanceWindowResponse where
