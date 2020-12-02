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
-- Module      : Network.AWS.WorkSpaces.DeleteWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image from your account. To delete an image, you must first delete any bundles that are associated with the image and unshare the image if it is shared with other accounts.
module Network.AWS.WorkSpaces.DeleteWorkspaceImage
  ( -- * Creating a Request
    deleteWorkspaceImage,
    DeleteWorkspaceImage,

    -- * Request Lenses
    dwiImageId,

    -- * Destructuring the Response
    deleteWorkspaceImageResponse,
    DeleteWorkspaceImageResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'deleteWorkspaceImage' smart constructor.
newtype DeleteWorkspaceImage = DeleteWorkspaceImage'
  { _dwiImageId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteWorkspaceImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwiImageId' - The identifier of the image.
deleteWorkspaceImage ::
  -- | 'dwiImageId'
  Text ->
  DeleteWorkspaceImage
deleteWorkspaceImage pImageId_ =
  DeleteWorkspaceImage' {_dwiImageId = pImageId_}

-- | The identifier of the image.
dwiImageId :: Lens' DeleteWorkspaceImage Text
dwiImageId = lens _dwiImageId (\s a -> s {_dwiImageId = a})

instance AWSRequest DeleteWorkspaceImage where
  type Rs DeleteWorkspaceImage = DeleteWorkspaceImageResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      (\s h x -> DeleteWorkspaceImageResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteWorkspaceImage

instance NFData DeleteWorkspaceImage

instance ToHeaders DeleteWorkspaceImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DeleteWorkspaceImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteWorkspaceImage where
  toJSON DeleteWorkspaceImage' {..} =
    object (catMaybes [Just ("ImageId" .= _dwiImageId)])

instance ToPath DeleteWorkspaceImage where
  toPath = const "/"

instance ToQuery DeleteWorkspaceImage where
  toQuery = const mempty

-- | /See:/ 'deleteWorkspaceImageResponse' smart constructor.
newtype DeleteWorkspaceImageResponse = DeleteWorkspaceImageResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteWorkspaceImageResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteWorkspaceImageResponse
deleteWorkspaceImageResponse pResponseStatus_ =
  DeleteWorkspaceImageResponse'
    { _delrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteWorkspaceImageResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteWorkspaceImageResponse
