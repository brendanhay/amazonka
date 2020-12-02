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
-- Module      : Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares an image with one account by specifying whether that account has permission to copy the image. If the copy image permission is granted, the image is shared with that account. If the copy image permission is revoked, the image is unshared with the account. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
  ( -- * Creating a Request
    updateWorkspaceImagePermission,
    UpdateWorkspaceImagePermission,

    -- * Request Lenses
    uwipImageId,
    uwipAllowCopyImage,
    uwipSharedAccountId,

    -- * Destructuring the Response
    updateWorkspaceImagePermissionResponse,
    UpdateWorkspaceImagePermissionResponse,

    -- * Response Lenses
    uwiprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'updateWorkspaceImagePermission' smart constructor.
data UpdateWorkspaceImagePermission = UpdateWorkspaceImagePermission'
  { _uwipImageId ::
      !Text,
    _uwipAllowCopyImage :: !Bool,
    _uwipSharedAccountId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkspaceImagePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwipImageId' - The identifier of the image.
--
-- * 'uwipAllowCopyImage' - The permission to copy the image. This permission can be revoked only after an image has been shared.
--
-- * 'uwipSharedAccountId' - The identifier of the AWS account to share or unshare the image with. /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
updateWorkspaceImagePermission ::
  -- | 'uwipImageId'
  Text ->
  -- | 'uwipAllowCopyImage'
  Bool ->
  -- | 'uwipSharedAccountId'
  Text ->
  UpdateWorkspaceImagePermission
updateWorkspaceImagePermission
  pImageId_
  pAllowCopyImage_
  pSharedAccountId_ =
    UpdateWorkspaceImagePermission'
      { _uwipImageId = pImageId_,
        _uwipAllowCopyImage = pAllowCopyImage_,
        _uwipSharedAccountId = pSharedAccountId_
      }

-- | The identifier of the image.
uwipImageId :: Lens' UpdateWorkspaceImagePermission Text
uwipImageId = lens _uwipImageId (\s a -> s {_uwipImageId = a})

-- | The permission to copy the image. This permission can be revoked only after an image has been shared.
uwipAllowCopyImage :: Lens' UpdateWorkspaceImagePermission Bool
uwipAllowCopyImage = lens _uwipAllowCopyImage (\s a -> s {_uwipAllowCopyImage = a})

-- | The identifier of the AWS account to share or unshare the image with. /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
uwipSharedAccountId :: Lens' UpdateWorkspaceImagePermission Text
uwipSharedAccountId = lens _uwipSharedAccountId (\s a -> s {_uwipSharedAccountId = a})

instance AWSRequest UpdateWorkspaceImagePermission where
  type
    Rs UpdateWorkspaceImagePermission =
      UpdateWorkspaceImagePermissionResponse
  request = postJSON workSpaces
  response =
    receiveEmpty
      ( \s h x ->
          UpdateWorkspaceImagePermissionResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateWorkspaceImagePermission

instance NFData UpdateWorkspaceImagePermission

instance ToHeaders UpdateWorkspaceImagePermission where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.UpdateWorkspaceImagePermission" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateWorkspaceImagePermission where
  toJSON UpdateWorkspaceImagePermission' {..} =
    object
      ( catMaybes
          [ Just ("ImageId" .= _uwipImageId),
            Just ("AllowCopyImage" .= _uwipAllowCopyImage),
            Just ("SharedAccountId" .= _uwipSharedAccountId)
          ]
      )

instance ToPath UpdateWorkspaceImagePermission where
  toPath = const "/"

instance ToQuery UpdateWorkspaceImagePermission where
  toQuery = const mempty

-- | /See:/ 'updateWorkspaceImagePermissionResponse' smart constructor.
newtype UpdateWorkspaceImagePermissionResponse = UpdateWorkspaceImagePermissionResponse'
  { _uwiprsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateWorkspaceImagePermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwiprsResponseStatus' - -- | The response status code.
updateWorkspaceImagePermissionResponse ::
  -- | 'uwiprsResponseStatus'
  Int ->
  UpdateWorkspaceImagePermissionResponse
updateWorkspaceImagePermissionResponse pResponseStatus_ =
  UpdateWorkspaceImagePermissionResponse'
    { _uwiprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uwiprsResponseStatus :: Lens' UpdateWorkspaceImagePermissionResponse Int
uwiprsResponseStatus = lens _uwiprsResponseStatus (\s a -> s {_uwiprsResponseStatus = a})

instance NFData UpdateWorkspaceImagePermissionResponse
