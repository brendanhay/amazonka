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
-- Module      : Network.AWS.SSM.UpdateResourceDataSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a resource data sync. After you create a resource data sync for a Region, you can't change the account options for that sync. For example, if you create a sync in the us-east-2 (Ohio) Region and you choose the Include only the current account option, you can't edit that sync later and choose the Include all accounts from my AWS Organizations configuration option. Instead, you must delete the first resource data sync, and create a new one.
module Network.AWS.SSM.UpdateResourceDataSync
  ( -- * Creating a Request
    updateResourceDataSync,
    UpdateResourceDataSync,

    -- * Request Lenses
    urdsSyncName,
    urdsSyncType,
    urdsSyncSource,

    -- * Destructuring the Response
    updateResourceDataSyncResponse,
    UpdateResourceDataSyncResponse,

    -- * Response Lenses
    urdsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'updateResourceDataSync' smart constructor.
data UpdateResourceDataSync = UpdateResourceDataSync'
  { _urdsSyncName ::
      !Text,
    _urdsSyncType :: !Text,
    _urdsSyncSource :: !ResourceDataSyncSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateResourceDataSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdsSyncName' - The name of the resource data sync you want to update.
--
-- * 'urdsSyncType' - The type of resource data sync. The supported @SyncType@ is SyncFromSource.
--
-- * 'urdsSyncSource' - Specify information about the data sources to synchronize.
updateResourceDataSync ::
  -- | 'urdsSyncName'
  Text ->
  -- | 'urdsSyncType'
  Text ->
  -- | 'urdsSyncSource'
  ResourceDataSyncSource ->
  UpdateResourceDataSync
updateResourceDataSync pSyncName_ pSyncType_ pSyncSource_ =
  UpdateResourceDataSync'
    { _urdsSyncName = pSyncName_,
      _urdsSyncType = pSyncType_,
      _urdsSyncSource = pSyncSource_
    }

-- | The name of the resource data sync you want to update.
urdsSyncName :: Lens' UpdateResourceDataSync Text
urdsSyncName = lens _urdsSyncName (\s a -> s {_urdsSyncName = a})

-- | The type of resource data sync. The supported @SyncType@ is SyncFromSource.
urdsSyncType :: Lens' UpdateResourceDataSync Text
urdsSyncType = lens _urdsSyncType (\s a -> s {_urdsSyncType = a})

-- | Specify information about the data sources to synchronize.
urdsSyncSource :: Lens' UpdateResourceDataSync ResourceDataSyncSource
urdsSyncSource = lens _urdsSyncSource (\s a -> s {_urdsSyncSource = a})

instance AWSRequest UpdateResourceDataSync where
  type Rs UpdateResourceDataSync = UpdateResourceDataSyncResponse
  request = postJSON ssm
  response =
    receiveEmpty
      ( \s h x ->
          UpdateResourceDataSyncResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateResourceDataSync

instance NFData UpdateResourceDataSync

instance ToHeaders UpdateResourceDataSync where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.UpdateResourceDataSync" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateResourceDataSync where
  toJSON UpdateResourceDataSync' {..} =
    object
      ( catMaybes
          [ Just ("SyncName" .= _urdsSyncName),
            Just ("SyncType" .= _urdsSyncType),
            Just ("SyncSource" .= _urdsSyncSource)
          ]
      )

instance ToPath UpdateResourceDataSync where
  toPath = const "/"

instance ToQuery UpdateResourceDataSync where
  toQuery = const mempty

-- | /See:/ 'updateResourceDataSyncResponse' smart constructor.
newtype UpdateResourceDataSyncResponse = UpdateResourceDataSyncResponse'
  { _urdsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdsrsResponseStatus' - -- | The response status code.
updateResourceDataSyncResponse ::
  -- | 'urdsrsResponseStatus'
  Int ->
  UpdateResourceDataSyncResponse
updateResourceDataSyncResponse pResponseStatus_ =
  UpdateResourceDataSyncResponse'
    { _urdsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
urdsrsResponseStatus :: Lens' UpdateResourceDataSyncResponse Int
urdsrsResponseStatus = lens _urdsrsResponseStatus (\s a -> s {_urdsrsResponseStatus = a})

instance NFData UpdateResourceDataSyncResponse
