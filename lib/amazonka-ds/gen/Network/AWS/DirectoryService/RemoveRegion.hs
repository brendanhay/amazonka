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
-- Module      : Network.AWS.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the specified Region. You cannot remove the primary Region with this operation. Instead, use the @DeleteDirectory@ API.
module Network.AWS.DirectoryService.RemoveRegion
  ( -- * Creating a Request
    removeRegion,
    RemoveRegion,

    -- * Request Lenses
    rrDirectoryId,

    -- * Destructuring the Response
    removeRegionResponse,
    RemoveRegionResponse,

    -- * Response Lenses
    rrrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeRegion' smart constructor.
newtype RemoveRegion = RemoveRegion' {_rrDirectoryId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveRegion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDirectoryId' - The identifier of the directory for which you want to remove Region replication.
removeRegion ::
  -- | 'rrDirectoryId'
  Text ->
  RemoveRegion
removeRegion pDirectoryId_ =
  RemoveRegion' {_rrDirectoryId = pDirectoryId_}

-- | The identifier of the directory for which you want to remove Region replication.
rrDirectoryId :: Lens' RemoveRegion Text
rrDirectoryId = lens _rrDirectoryId (\s a -> s {_rrDirectoryId = a})

instance AWSRequest RemoveRegion where
  type Rs RemoveRegion = RemoveRegionResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> RemoveRegionResponse' <$> (pure (fromEnum s)))

instance Hashable RemoveRegion

instance NFData RemoveRegion

instance ToHeaders RemoveRegion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.RemoveRegion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RemoveRegion where
  toJSON RemoveRegion' {..} =
    object (catMaybes [Just ("DirectoryId" .= _rrDirectoryId)])

instance ToPath RemoveRegion where
  toPath = const "/"

instance ToQuery RemoveRegion where
  toQuery = const mempty

-- | /See:/ 'removeRegionResponse' smart constructor.
newtype RemoveRegionResponse = RemoveRegionResponse'
  { _rrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveRegionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrrsResponseStatus' - -- | The response status code.
removeRegionResponse ::
  -- | 'rrrsResponseStatus'
  Int ->
  RemoveRegionResponse
removeRegionResponse pResponseStatus_ =
  RemoveRegionResponse' {_rrrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
rrrsResponseStatus :: Lens' RemoveRegionResponse Int
rrrsResponseStatus = lens _rrrsResponseStatus (\s a -> s {_rrrsResponseStatus = a})

instance NFData RemoveRegionResponse
