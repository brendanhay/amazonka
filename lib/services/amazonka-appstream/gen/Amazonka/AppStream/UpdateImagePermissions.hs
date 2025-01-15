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
-- Module      : Amazonka.AppStream.UpdateImagePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates permissions for the specified private image.
module Amazonka.AppStream.UpdateImagePermissions
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

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateImagePermissions' smart constructor.
data UpdateImagePermissions = UpdateImagePermissions'
  { -- | The name of the private image.
    name :: Prelude.Text,
    -- | The 12-digit identifier of the AWS account for which you want add or
    -- update image permissions.
    sharedAccountId :: Prelude.Text,
    -- | The permissions for the image.
    imagePermissions :: ImagePermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'sharedAccountId'
  Prelude.Text ->
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
updateImagePermissions_name :: Lens.Lens' UpdateImagePermissions Prelude.Text
updateImagePermissions_name = Lens.lens (\UpdateImagePermissions' {name} -> name) (\s@UpdateImagePermissions' {} a -> s {name = a} :: UpdateImagePermissions)

-- | The 12-digit identifier of the AWS account for which you want add or
-- update image permissions.
updateImagePermissions_sharedAccountId :: Lens.Lens' UpdateImagePermissions Prelude.Text
updateImagePermissions_sharedAccountId = Lens.lens (\UpdateImagePermissions' {sharedAccountId} -> sharedAccountId) (\s@UpdateImagePermissions' {} a -> s {sharedAccountId = a} :: UpdateImagePermissions)

-- | The permissions for the image.
updateImagePermissions_imagePermissions :: Lens.Lens' UpdateImagePermissions ImagePermissions
updateImagePermissions_imagePermissions = Lens.lens (\UpdateImagePermissions' {imagePermissions} -> imagePermissions) (\s@UpdateImagePermissions' {} a -> s {imagePermissions = a} :: UpdateImagePermissions)

instance Core.AWSRequest UpdateImagePermissions where
  type
    AWSResponse UpdateImagePermissions =
      UpdateImagePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateImagePermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateImagePermissions where
  hashWithSalt _salt UpdateImagePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sharedAccountId
      `Prelude.hashWithSalt` imagePermissions

instance Prelude.NFData UpdateImagePermissions where
  rnf UpdateImagePermissions' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf sharedAccountId `Prelude.seq`
        Prelude.rnf imagePermissions

instance Data.ToHeaders UpdateImagePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.UpdateImagePermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateImagePermissions where
  toJSON UpdateImagePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("SharedAccountId" Data..= sharedAccountId),
            Prelude.Just
              ("ImagePermissions" Data..= imagePermissions)
          ]
      )

instance Data.ToPath UpdateImagePermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateImagePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateImagePermissionsResponse' smart constructor.
data UpdateImagePermissionsResponse = UpdateImagePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateImagePermissionsResponse
newUpdateImagePermissionsResponse pHttpStatus_ =
  UpdateImagePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateImagePermissionsResponse_httpStatus :: Lens.Lens' UpdateImagePermissionsResponse Prelude.Int
updateImagePermissionsResponse_httpStatus = Lens.lens (\UpdateImagePermissionsResponse' {httpStatus} -> httpStatus) (\s@UpdateImagePermissionsResponse' {} a -> s {httpStatus = a} :: UpdateImagePermissionsResponse)

instance
  Prelude.NFData
    UpdateImagePermissionsResponse
  where
  rnf UpdateImagePermissionsResponse' {..} =
    Prelude.rnf httpStatus
