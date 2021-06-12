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
-- Module      : Network.AWS.AppStream.DeleteImagePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions for the specified private image. After you delete
-- permissions for an image, AWS accounts to which you previously granted
-- these permissions can no longer use the image.
module Network.AWS.AppStream.DeleteImagePermissions
  ( -- * Creating a Request
    DeleteImagePermissions (..),
    newDeleteImagePermissions,

    -- * Request Lenses
    deleteImagePermissions_name,
    deleteImagePermissions_sharedAccountId,

    -- * Destructuring the Response
    DeleteImagePermissionsResponse (..),
    newDeleteImagePermissionsResponse,

    -- * Response Lenses
    deleteImagePermissionsResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteImagePermissions' smart constructor.
data DeleteImagePermissions = DeleteImagePermissions'
  { -- | The name of the private image.
    name :: Core.Text,
    -- | The 12-digit identifier of the AWS account for which to delete image
    -- permissions.
    sharedAccountId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImagePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteImagePermissions_name' - The name of the private image.
--
-- 'sharedAccountId', 'deleteImagePermissions_sharedAccountId' - The 12-digit identifier of the AWS account for which to delete image
-- permissions.
newDeleteImagePermissions ::
  -- | 'name'
  Core.Text ->
  -- | 'sharedAccountId'
  Core.Text ->
  DeleteImagePermissions
newDeleteImagePermissions pName_ pSharedAccountId_ =
  DeleteImagePermissions'
    { name = pName_,
      sharedAccountId = pSharedAccountId_
    }

-- | The name of the private image.
deleteImagePermissions_name :: Lens.Lens' DeleteImagePermissions Core.Text
deleteImagePermissions_name = Lens.lens (\DeleteImagePermissions' {name} -> name) (\s@DeleteImagePermissions' {} a -> s {name = a} :: DeleteImagePermissions)

-- | The 12-digit identifier of the AWS account for which to delete image
-- permissions.
deleteImagePermissions_sharedAccountId :: Lens.Lens' DeleteImagePermissions Core.Text
deleteImagePermissions_sharedAccountId = Lens.lens (\DeleteImagePermissions' {sharedAccountId} -> sharedAccountId) (\s@DeleteImagePermissions' {} a -> s {sharedAccountId = a} :: DeleteImagePermissions)

instance Core.AWSRequest DeleteImagePermissions where
  type
    AWSResponse DeleteImagePermissions =
      DeleteImagePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImagePermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteImagePermissions

instance Core.NFData DeleteImagePermissions

instance Core.ToHeaders DeleteImagePermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteImagePermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteImagePermissions where
  toJSON DeleteImagePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just
              ("SharedAccountId" Core..= sharedAccountId)
          ]
      )

instance Core.ToPath DeleteImagePermissions where
  toPath = Core.const "/"

instance Core.ToQuery DeleteImagePermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteImagePermissionsResponse' smart constructor.
data DeleteImagePermissionsResponse = DeleteImagePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteImagePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteImagePermissionsResponse_httpStatus' - The response's http status code.
newDeleteImagePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteImagePermissionsResponse
newDeleteImagePermissionsResponse pHttpStatus_ =
  DeleteImagePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteImagePermissionsResponse_httpStatus :: Lens.Lens' DeleteImagePermissionsResponse Core.Int
deleteImagePermissionsResponse_httpStatus = Lens.lens (\DeleteImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DeleteImagePermissionsResponse' {} a -> s {httpStatus = a} :: DeleteImagePermissionsResponse)

instance Core.NFData DeleteImagePermissionsResponse
