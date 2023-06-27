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
-- Module      : Amazonka.WorkSpaces.UpdateWorkspaceImagePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares an image with one account in the same Amazon Web
-- Services Region by specifying whether that account has permission to
-- copy the image. If the copy image permission is granted, the image is
-- shared with that account. If the copy image permission is revoked, the
-- image is unshared with the account.
--
-- After an image has been shared, the recipient account can copy the image
-- to other Regions as needed.
--
-- In the China (Ningxia) Region, you can copy images only within the same
-- Region.
--
-- In Amazon Web Services GovCloud (US), to copy images to and from other
-- Regions, contact Amazon Web Services Support.
--
-- For more information about sharing images, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image>.
--
-- -   To delete an image that has been shared, you must unshare the image
--     before you delete it.
--
-- -   Sharing Bring Your Own License (BYOL) images across Amazon Web
--     Services accounts isn\'t supported at this time in Amazon Web
--     Services GovCloud (US). To share BYOL images across accounts in
--     Amazon Web Services GovCloud (US), contact Amazon Web Services
--     Support.
module Amazonka.WorkSpaces.UpdateWorkspaceImagePermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newUpdateWorkspaceImagePermission' smart constructor.
data UpdateWorkspaceImagePermission = UpdateWorkspaceImagePermission'
  { -- | The identifier of the image.
    imageId :: Prelude.Text,
    -- | The permission to copy the image. This permission can be revoked only
    -- after an image has been shared.
    allowCopyImage :: Prelude.Bool,
    -- | The identifier of the Amazon Web Services account to share or unshare
    -- the image with.
    --
    -- Before sharing the image, confirm that you are sharing to the correct
    -- Amazon Web Services account ID.
    sharedAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'sharedAccountId', 'updateWorkspaceImagePermission_sharedAccountId' - The identifier of the Amazon Web Services account to share or unshare
-- the image with.
--
-- Before sharing the image, confirm that you are sharing to the correct
-- Amazon Web Services account ID.
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

-- | The identifier of the Amazon Web Services account to share or unshare
-- the image with.
--
-- Before sharing the image, confirm that you are sharing to the correct
-- Amazon Web Services account ID.
updateWorkspaceImagePermission_sharedAccountId :: Lens.Lens' UpdateWorkspaceImagePermission Prelude.Text
updateWorkspaceImagePermission_sharedAccountId = Lens.lens (\UpdateWorkspaceImagePermission' {sharedAccountId} -> sharedAccountId) (\s@UpdateWorkspaceImagePermission' {} a -> s {sharedAccountId = a} :: UpdateWorkspaceImagePermission)

instance
  Core.AWSRequest
    UpdateWorkspaceImagePermission
  where
  type
    AWSResponse UpdateWorkspaceImagePermission =
      UpdateWorkspaceImagePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWorkspaceImagePermissionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateWorkspaceImagePermission
  where
  hashWithSalt
    _salt
    UpdateWorkspaceImagePermission' {..} =
      _salt
        `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` allowCopyImage
        `Prelude.hashWithSalt` sharedAccountId

instance
  Prelude.NFData
    UpdateWorkspaceImagePermission
  where
  rnf UpdateWorkspaceImagePermission' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf allowCopyImage
      `Prelude.seq` Prelude.rnf sharedAccountId

instance
  Data.ToHeaders
    UpdateWorkspaceImagePermission
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.UpdateWorkspaceImagePermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspaceImagePermission where
  toJSON UpdateWorkspaceImagePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ImageId" Data..= imageId),
            Prelude.Just
              ("AllowCopyImage" Data..= allowCopyImage),
            Prelude.Just
              ("SharedAccountId" Data..= sharedAccountId)
          ]
      )

instance Data.ToPath UpdateWorkspaceImagePermission where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWorkspaceImagePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceImagePermissionResponse' smart constructor.
data UpdateWorkspaceImagePermissionResponse = UpdateWorkspaceImagePermissionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateWorkspaceImagePermissionResponse' {..} =
    Prelude.rnf httpStatus
