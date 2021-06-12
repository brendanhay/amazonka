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
-- Module      : Network.AWS.AppStream.UpdateImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates permissions for the specified private image.
module Network.AWS.AppStream.UpdateImagePermissions
  ( -- * Creating a Request
    UpdateImagePermissions (..),
    newUpdateImagePermissions,

    -- * Request Lenses
    updateImagePermissions_name,
    updateImagePermissions_sharedAccountId,
    updateImagePermissions_imagePermissions,

    -- * Destructuring the Response
    UpdateImagePermissionsResponse (..),
    newUpdateImagePermissionsResponse,

    -- * Response Lenses
    updateImagePermissionsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateImagePermissions' smart constructor.
data UpdateImagePermissions = UpdateImagePermissions'
  { -- | The name of the private image.
    name :: Core.Text,
    -- | The 12-digit identifier of the AWS account for which you want add or
    -- update image permissions.
    sharedAccountId :: Core.Text,
    -- | The permissions for the image.
    imagePermissions :: ImagePermissions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateImagePermissions_name' - The name of the private image.
--
-- 'sharedAccountId', 'updateImagePermissions_sharedAccountId' - The 12-digit identifier of the AWS account for which you want add or
-- update image permissions.
--
-- 'imagePermissions', 'updateImagePermissions_imagePermissions' - The permissions for the image.
newUpdateImagePermissions ::
  -- | 'name'
  Core.Text ->
  -- | 'sharedAccountId'
  Core.Text ->
  -- | 'imagePermissions'
  ImagePermissions ->
  UpdateImagePermissions
newUpdateImagePermissions
  pName_
  pSharedAccountId_
  pImagePermissions_ =
    UpdateImagePermissions'
      { name = pName_,
        sharedAccountId = pSharedAccountId_,
        imagePermissions = pImagePermissions_
      }

-- | The name of the private image.
updateImagePermissions_name :: Lens.Lens' UpdateImagePermissions Core.Text
updateImagePermissions_name = Lens.lens (\UpdateImagePermissions' {name} -> name) (\s@UpdateImagePermissions' {} a -> s {name = a} :: UpdateImagePermissions)

-- | The 12-digit identifier of the AWS account for which you want add or
-- update image permissions.
updateImagePermissions_sharedAccountId :: Lens.Lens' UpdateImagePermissions Core.Text
updateImagePermissions_sharedAccountId = Lens.lens (\UpdateImagePermissions' {sharedAccountId} -> sharedAccountId) (\s@UpdateImagePermissions' {} a -> s {sharedAccountId = a} :: UpdateImagePermissions)

-- | The permissions for the image.
updateImagePermissions_imagePermissions :: Lens.Lens' UpdateImagePermissions ImagePermissions
updateImagePermissions_imagePermissions = Lens.lens (\UpdateImagePermissions' {imagePermissions} -> imagePermissions) (\s@UpdateImagePermissions' {} a -> s {imagePermissions = a} :: UpdateImagePermissions)

instance Core.AWSRequest UpdateImagePermissions where
  type
    AWSResponse UpdateImagePermissions =
      UpdateImagePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateImagePermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateImagePermissions

instance Core.NFData UpdateImagePermissions

instance Core.ToHeaders UpdateImagePermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.UpdateImagePermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateImagePermissions where
  toJSON UpdateImagePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ("SharedAccountId" Core..= sharedAccountId),
            Core.Just
              ("ImagePermissions" Core..= imagePermissions)
          ]
      )

instance Core.ToPath UpdateImagePermissions where
  toPath = Core.const "/"

instance Core.ToQuery UpdateImagePermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateImagePermissionsResponse' smart constructor.
data UpdateImagePermissionsResponse = UpdateImagePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateImagePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateImagePermissionsResponse_httpStatus' - The response's http status code.
newUpdateImagePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateImagePermissionsResponse
newUpdateImagePermissionsResponse pHttpStatus_ =
  UpdateImagePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateImagePermissionsResponse_httpStatus :: Lens.Lens' UpdateImagePermissionsResponse Core.Int
updateImagePermissionsResponse_httpStatus = Lens.lens (\UpdateImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@UpdateImagePermissionsResponse' {} a -> s {httpStatus = a} :: UpdateImagePermissionsResponse)

instance Core.NFData UpdateImagePermissionsResponse
