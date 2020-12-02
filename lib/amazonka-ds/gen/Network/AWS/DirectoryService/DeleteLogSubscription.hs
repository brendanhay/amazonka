{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Network.AWS.DirectoryService.DeleteLogSubscription
  ( -- * Creating a Request
    deleteLogSubscription,
    DeleteLogSubscription,

    -- * Request Lenses
    dlsDirectoryId,

    -- * Destructuring the Response
    deleteLogSubscriptionResponse,
    DeleteLogSubscriptionResponse,

    -- * Response Lenses
    dlsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLogSubscription' smart constructor.
newtype DeleteLogSubscription = DeleteLogSubscription'
  { _dlsDirectoryId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLogSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsDirectoryId' - Identifier of the directory whose log subscription you want to delete.
deleteLogSubscription ::
  -- | 'dlsDirectoryId'
  Text ->
  DeleteLogSubscription
deleteLogSubscription pDirectoryId_ =
  DeleteLogSubscription' {_dlsDirectoryId = pDirectoryId_}

-- | Identifier of the directory whose log subscription you want to delete.
dlsDirectoryId :: Lens' DeleteLogSubscription Text
dlsDirectoryId = lens _dlsDirectoryId (\s a -> s {_dlsDirectoryId = a})

instance AWSRequest DeleteLogSubscription where
  type Rs DeleteLogSubscription = DeleteLogSubscriptionResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> DeleteLogSubscriptionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteLogSubscription

instance NFData DeleteLogSubscription

instance ToHeaders DeleteLogSubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.DeleteLogSubscription" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteLogSubscription where
  toJSON DeleteLogSubscription' {..} =
    object (catMaybes [Just ("DirectoryId" .= _dlsDirectoryId)])

instance ToPath DeleteLogSubscription where
  toPath = const "/"

instance ToQuery DeleteLogSubscription where
  toQuery = const mempty

-- | /See:/ 'deleteLogSubscriptionResponse' smart constructor.
newtype DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { _dlsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteLogSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlsrsResponseStatus' - -- | The response status code.
deleteLogSubscriptionResponse ::
  -- | 'dlsrsResponseStatus'
  Int ->
  DeleteLogSubscriptionResponse
deleteLogSubscriptionResponse pResponseStatus_ =
  DeleteLogSubscriptionResponse'
    { _dlsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dlsrsResponseStatus :: Lens' DeleteLogSubscriptionResponse Int
dlsrsResponseStatus = lens _dlsrsResponseStatus (\s a -> s {_dlsrsResponseStatus = a})

instance NFData DeleteLogSubscriptionResponse
