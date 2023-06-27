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
-- Module      : Amazonka.AppStream.DeleteImagePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions for the specified private image. After you delete
-- permissions for an image, AWS accounts to which you previously granted
-- these permissions can no longer use the image.
module Amazonka.AppStream.DeleteImagePermissions
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImagePermissions' smart constructor.
data DeleteImagePermissions = DeleteImagePermissions'
  { -- | The name of the private image.
    name :: Prelude.Text,
    -- | The 12-digit identifier of the AWS account for which to delete image
    -- permissions.
    sharedAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'sharedAccountId'
  Prelude.Text ->
  DeleteImagePermissions
newDeleteImagePermissions pName_ pSharedAccountId_ =
  DeleteImagePermissions'
    { name = pName_,
      sharedAccountId = pSharedAccountId_
    }

-- | The name of the private image.
deleteImagePermissions_name :: Lens.Lens' DeleteImagePermissions Prelude.Text
deleteImagePermissions_name = Lens.lens (\DeleteImagePermissions' {name} -> name) (\s@DeleteImagePermissions' {} a -> s {name = a} :: DeleteImagePermissions)

-- | The 12-digit identifier of the AWS account for which to delete image
-- permissions.
deleteImagePermissions_sharedAccountId :: Lens.Lens' DeleteImagePermissions Prelude.Text
deleteImagePermissions_sharedAccountId = Lens.lens (\DeleteImagePermissions' {sharedAccountId} -> sharedAccountId) (\s@DeleteImagePermissions' {} a -> s {sharedAccountId = a} :: DeleteImagePermissions)

instance Core.AWSRequest DeleteImagePermissions where
  type
    AWSResponse DeleteImagePermissions =
      DeleteImagePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImagePermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImagePermissions where
  hashWithSalt _salt DeleteImagePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sharedAccountId

instance Prelude.NFData DeleteImagePermissions where
  rnf DeleteImagePermissions' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sharedAccountId

instance Data.ToHeaders DeleteImagePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DeleteImagePermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteImagePermissions where
  toJSON DeleteImagePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("SharedAccountId" Data..= sharedAccountId)
          ]
      )

instance Data.ToPath DeleteImagePermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteImagePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImagePermissionsResponse' smart constructor.
data DeleteImagePermissionsResponse = DeleteImagePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteImagePermissionsResponse
newDeleteImagePermissionsResponse pHttpStatus_ =
  DeleteImagePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteImagePermissionsResponse_httpStatus :: Lens.Lens' DeleteImagePermissionsResponse Prelude.Int
deleteImagePermissionsResponse_httpStatus = Lens.lens (\DeleteImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@DeleteImagePermissionsResponse' {} a -> s {httpStatus = a} :: DeleteImagePermissionsResponse)

instance
  Prelude.NFData
    DeleteImagePermissionsResponse
  where
  rnf DeleteImagePermissionsResponse' {..} =
    Prelude.rnf httpStatus
