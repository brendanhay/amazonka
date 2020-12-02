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
-- Module      : Network.AWS.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if there are no tapes in the pool and if there are no automatic tape creation policies that reference the custom tape pool.
module Network.AWS.StorageGateway.DeleteTapePool
  ( -- * Creating a Request
    deleteTapePool,
    DeleteTapePool,

    -- * Request Lenses
    dtpPoolARN,

    -- * Destructuring the Response
    deleteTapePoolResponse,
    DeleteTapePoolResponse,

    -- * Response Lenses
    dtprsPoolARN,
    dtprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteTapePool' smart constructor.
newtype DeleteTapePool = DeleteTapePool' {_dtpPoolARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTapePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtpPoolARN' - The Amazon Resource Name (ARN) of the custom tape pool to delete.
deleteTapePool ::
  -- | 'dtpPoolARN'
  Text ->
  DeleteTapePool
deleteTapePool pPoolARN_ = DeleteTapePool' {_dtpPoolARN = pPoolARN_}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
dtpPoolARN :: Lens' DeleteTapePool Text
dtpPoolARN = lens _dtpPoolARN (\s a -> s {_dtpPoolARN = a})

instance AWSRequest DeleteTapePool where
  type Rs DeleteTapePool = DeleteTapePoolResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DeleteTapePoolResponse'
            <$> (x .?> "PoolARN") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTapePool

instance NFData DeleteTapePool

instance ToHeaders DeleteTapePool where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.DeleteTapePool" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteTapePool where
  toJSON DeleteTapePool' {..} =
    object (catMaybes [Just ("PoolARN" .= _dtpPoolARN)])

instance ToPath DeleteTapePool where
  toPath = const "/"

instance ToQuery DeleteTapePool where
  toQuery = const mempty

-- | /See:/ 'deleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { _dtprsPoolARN ::
      !(Maybe Text),
    _dtprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTapePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtprsPoolARN' - The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- * 'dtprsResponseStatus' - -- | The response status code.
deleteTapePoolResponse ::
  -- | 'dtprsResponseStatus'
  Int ->
  DeleteTapePoolResponse
deleteTapePoolResponse pResponseStatus_ =
  DeleteTapePoolResponse'
    { _dtprsPoolARN = Nothing,
      _dtprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
dtprsPoolARN :: Lens' DeleteTapePoolResponse (Maybe Text)
dtprsPoolARN = lens _dtprsPoolARN (\s a -> s {_dtprsPoolARN = a})

-- | -- | The response status code.
dtprsResponseStatus :: Lens' DeleteTapePoolResponse Int
dtprsResponseStatus = lens _dtprsResponseStatus (\s a -> s {_dtprsResponseStatus = a})

instance NFData DeleteTapePoolResponse
