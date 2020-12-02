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
-- Module      : Network.AWS.SSM.DeleteResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to data on managed instances are no longer synced to or from the target. Deleting a sync configuration does not delete data.
module Network.AWS.SSM.DeleteResourceDataSync
  ( -- * Creating a Request
    deleteResourceDataSync,
    DeleteResourceDataSync,

    -- * Request Lenses
    drdsSyncType,
    drdsSyncName,

    -- * Destructuring the Response
    deleteResourceDataSyncResponse,
    DeleteResourceDataSyncResponse,

    -- * Response Lenses
    drdsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'deleteResourceDataSync' smart constructor.
data DeleteResourceDataSync = DeleteResourceDataSync'
  { _drdsSyncType ::
      !(Maybe Text),
    _drdsSyncName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourceDataSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsSyncType' - Specify the type of resource data sync to delete.
--
-- * 'drdsSyncName' - The name of the configuration to delete.
deleteResourceDataSync ::
  -- | 'drdsSyncName'
  Text ->
  DeleteResourceDataSync
deleteResourceDataSync pSyncName_ =
  DeleteResourceDataSync'
    { _drdsSyncType = Nothing,
      _drdsSyncName = pSyncName_
    }

-- | Specify the type of resource data sync to delete.
drdsSyncType :: Lens' DeleteResourceDataSync (Maybe Text)
drdsSyncType = lens _drdsSyncType (\s a -> s {_drdsSyncType = a})

-- | The name of the configuration to delete.
drdsSyncName :: Lens' DeleteResourceDataSync Text
drdsSyncName = lens _drdsSyncName (\s a -> s {_drdsSyncName = a})

instance AWSRequest DeleteResourceDataSync where
  type Rs DeleteResourceDataSync = DeleteResourceDataSyncResponse
  request = postJSON ssm
  response =
    receiveEmpty
      ( \s h x ->
          DeleteResourceDataSyncResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteResourceDataSync

instance NFData DeleteResourceDataSync

instance ToHeaders DeleteResourceDataSync where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.DeleteResourceDataSync" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteResourceDataSync where
  toJSON DeleteResourceDataSync' {..} =
    object
      ( catMaybes
          [ ("SyncType" .=) <$> _drdsSyncType,
            Just ("SyncName" .= _drdsSyncName)
          ]
      )

instance ToPath DeleteResourceDataSync where
  toPath = const "/"

instance ToQuery DeleteResourceDataSync where
  toQuery = const mempty

-- | /See:/ 'deleteResourceDataSyncResponse' smart constructor.
newtype DeleteResourceDataSyncResponse = DeleteResourceDataSyncResponse'
  { _drdsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsrsResponseStatus' - -- | The response status code.
deleteResourceDataSyncResponse ::
  -- | 'drdsrsResponseStatus'
  Int ->
  DeleteResourceDataSyncResponse
deleteResourceDataSyncResponse pResponseStatus_ =
  DeleteResourceDataSyncResponse'
    { _drdsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drdsrsResponseStatus :: Lens' DeleteResourceDataSyncResponse Int
drdsrsResponseStatus = lens _drdsrsResponseStatus (\s a -> s {_drdsrsResponseStatus = a})

instance NFData DeleteResourceDataSyncResponse
