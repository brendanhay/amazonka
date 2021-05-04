{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares an image with one account in the same AWS Region by
-- specifying whether that account has permission to copy the image. If the
-- copy image permission is granted, the image is shared with that account.
-- If the copy image permission is revoked, the image is unshared with the
-- account.
--
-- After an image has been shared, the recipient account can copy the image
-- to other AWS Regions as needed.
--
-- In the China (Ningxia) Region, you can copy images only within the same
-- Region.
--
-- In the AWS GovCloud (US-West) Region, to copy images to and from other
-- AWS Regions, contact AWS Support.
--
-- For more information about sharing images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image>.
--
-- -   To delete an image that has been shared, you must unshare the image
--     before you delete it.
--
-- -   Sharing Bring Your Own License (BYOL) images across AWS accounts
--     isn\'t supported at this time in the AWS GovCloud (US-West) Region.
--     To share BYOL images across accounts in the AWS GovCloud (US-West)
--     Region, contact AWS Support.
module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
  ( -- * Creating a Request
    UpdateWorkspaceImagePermission (..),
    newUpdateWorkspaceImagePermission,

    -- * Request Lenses
    updateWorkspaceImagePermission_imageId,
    updateWorkspaceImagePermission_allowCopyImage,
    updateWorkspaceImagePermission_sharedAccountId,

    -- * Destructuring the Response
    UpdateWorkspaceImagePermissionResponse (..),
    newUpdateWorkspaceImagePermissionResponse,

    -- * Response Lenses
    updateWorkspaceImagePermissionResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newUpdateWorkspaceImagePermission' smart constructor.
data UpdateWorkspaceImagePermission = UpdateWorkspaceImagePermission'
  { -- | The identifier of the image.
    imageId :: Prelude.Text,
    -- | The permission to copy the image. This permission can be revoked only
    -- after an image has been shared.
    allowCopyImage :: Prelude.Bool,
    -- | The identifier of the AWS account to share or unshare the image with.
    --
    -- Before sharing the image, confirm that you are sharing to the correct
    -- AWS account ID.
    sharedAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceImagePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'updateWorkspaceImagePermission_imageId' - The identifier of the image.
--
-- 'allowCopyImage', 'updateWorkspaceImagePermission_allowCopyImage' - The permission to copy the image. This permission can be revoked only
-- after an image has been shared.
--
-- 'sharedAccountId', 'updateWorkspaceImagePermission_sharedAccountId' - The identifier of the AWS account to share or unshare the image with.
--
-- Before sharing the image, confirm that you are sharing to the correct
-- AWS account ID.
newUpdateWorkspaceImagePermission ::
  -- | 'imageId'
  Prelude.Text ->
  -- | 'allowCopyImage'
  Prelude.Bool ->
  -- | 'sharedAccountId'
  Prelude.Text ->
  UpdateWorkspaceImagePermission
newUpdateWorkspaceImagePermission
  pImageId_
  pAllowCopyImage_
  pSharedAccountId_ =
    UpdateWorkspaceImagePermission'
      { imageId =
          pImageId_,
        allowCopyImage = pAllowCopyImage_,
        sharedAccountId = pSharedAccountId_
      }

-- | The identifier of the image.
updateWorkspaceImagePermission_imageId :: Lens.Lens' UpdateWorkspaceImagePermission Prelude.Text
updateWorkspaceImagePermission_imageId = Lens.lens (\UpdateWorkspaceImagePermission' {imageId} -> imageId) (\s@UpdateWorkspaceImagePermission' {} a -> s {imageId = a} :: UpdateWorkspaceImagePermission)

-- | The permission to copy the image. This permission can be revoked only
-- after an image has been shared.
updateWorkspaceImagePermission_allowCopyImage :: Lens.Lens' UpdateWorkspaceImagePermission Prelude.Bool
updateWorkspaceImagePermission_allowCopyImage = Lens.lens (\UpdateWorkspaceImagePermission' {allowCopyImage} -> allowCopyImage) (\s@UpdateWorkspaceImagePermission' {} a -> s {allowCopyImage = a} :: UpdateWorkspaceImagePermission)

-- | The identifier of the AWS account to share or unshare the image with.
--
-- Before sharing the image, confirm that you are sharing to the correct
-- AWS account ID.
updateWorkspaceImagePermission_sharedAccountId :: Lens.Lens' UpdateWorkspaceImagePermission Prelude.Text
updateWorkspaceImagePermission_sharedAccountId = Lens.lens (\UpdateWorkspaceImagePermission' {sharedAccountId} -> sharedAccountId) (\s@UpdateWorkspaceImagePermission' {} a -> s {sharedAccountId = a} :: UpdateWorkspaceImagePermission)

instance
  Prelude.AWSRequest
    UpdateWorkspaceImagePermission
  where
  type
    Rs UpdateWorkspaceImagePermission =
      UpdateWorkspaceImagePermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkspaceImagePermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateWorkspaceImagePermission

instance
  Prelude.NFData
    UpdateWorkspaceImagePermission

instance
  Prelude.ToHeaders
    UpdateWorkspaceImagePermission
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.UpdateWorkspaceImagePermission" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateWorkspaceImagePermission
  where
  toJSON UpdateWorkspaceImagePermission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ImageId" Prelude..= imageId),
            Prelude.Just
              ("AllowCopyImage" Prelude..= allowCopyImage),
            Prelude.Just
              ("SharedAccountId" Prelude..= sharedAccountId)
          ]
      )

instance
  Prelude.ToPath
    UpdateWorkspaceImagePermission
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateWorkspaceImagePermission
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceImagePermissionResponse' smart constructor.
data UpdateWorkspaceImagePermissionResponse = UpdateWorkspaceImagePermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceImagePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceImagePermissionResponse_httpStatus' - The response's http status code.
newUpdateWorkspaceImagePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkspaceImagePermissionResponse
newUpdateWorkspaceImagePermissionResponse
  pHttpStatus_ =
    UpdateWorkspaceImagePermissionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateWorkspaceImagePermissionResponse_httpStatus :: Lens.Lens' UpdateWorkspaceImagePermissionResponse Prelude.Int
updateWorkspaceImagePermissionResponse_httpStatus = Lens.lens (\UpdateWorkspaceImagePermissionResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceImagePermissionResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceImagePermissionResponse)

instance
  Prelude.NFData
    UpdateWorkspaceImagePermissionResponse
