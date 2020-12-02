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
-- Module      : Network.AWS.CloudWatchEvents.DeleteArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive.
module Network.AWS.CloudWatchEvents.DeleteArchive
  ( -- * Creating a Request
    deleteArchive,
    DeleteArchive,

    -- * Request Lenses
    dArchiveName,

    -- * Destructuring the Response
    deleteArchiveResponse,
    DeleteArchiveResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteArchive' smart constructor.
newtype DeleteArchive = DeleteArchive' {_dArchiveName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dArchiveName' - The name of the archive to delete.
deleteArchive ::
  -- | 'dArchiveName'
  Text ->
  DeleteArchive
deleteArchive pArchiveName_ =
  DeleteArchive' {_dArchiveName = pArchiveName_}

-- | The name of the archive to delete.
dArchiveName :: Lens' DeleteArchive Text
dArchiveName = lens _dArchiveName (\s a -> s {_dArchiveName = a})

instance AWSRequest DeleteArchive where
  type Rs DeleteArchive = DeleteArchiveResponse
  request = postJSON cloudWatchEvents
  response =
    receiveEmpty
      (\s h x -> DeleteArchiveResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteArchive

instance NFData DeleteArchive

instance ToHeaders DeleteArchive where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.DeleteArchive" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteArchive where
  toJSON DeleteArchive' {..} =
    object (catMaybes [Just ("ArchiveName" .= _dArchiveName)])

instance ToPath DeleteArchive where
  toPath = const "/"

instance ToQuery DeleteArchive where
  toQuery = const mempty

-- | /See:/ 'deleteArchiveResponse' smart constructor.
newtype DeleteArchiveResponse = DeleteArchiveResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteArchiveResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteArchiveResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteArchiveResponse
deleteArchiveResponse pResponseStatus_ =
  DeleteArchiveResponse' {_delrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteArchiveResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteArchiveResponse
