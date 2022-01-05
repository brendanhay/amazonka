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
-- Module      : Amazonka.RAM.GetPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of an RAM permission in JSON format.
module Amazonka.RAM.GetPermission
  ( -- * Creating a Request
    GetPermission (..),
    newGetPermission,

    -- * Request Lenses
    getPermission_permissionVersion,
    getPermission_permissionArn,

    -- * Destructuring the Response
    GetPermissionResponse (..),
    newGetPermissionResponse,

    -- * Response Lenses
    getPermissionResponse_permission,
    getPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPermission' smart constructor.
data GetPermission = GetPermission'
  { -- | The identifier for the version of the permission.
    permissionVersion :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the permission.
    permissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionVersion', 'getPermission_permissionVersion' - The identifier for the version of the permission.
--
-- 'permissionArn', 'getPermission_permissionArn' - The Amazon Resource Name (ARN) of the permission.
newGetPermission ::
  -- | 'permissionArn'
  Prelude.Text ->
  GetPermission
newGetPermission pPermissionArn_ =
  GetPermission'
    { permissionVersion = Prelude.Nothing,
      permissionArn = pPermissionArn_
    }

-- | The identifier for the version of the permission.
getPermission_permissionVersion :: Lens.Lens' GetPermission (Prelude.Maybe Prelude.Int)
getPermission_permissionVersion = Lens.lens (\GetPermission' {permissionVersion} -> permissionVersion) (\s@GetPermission' {} a -> s {permissionVersion = a} :: GetPermission)

-- | The Amazon Resource Name (ARN) of the permission.
getPermission_permissionArn :: Lens.Lens' GetPermission Prelude.Text
getPermission_permissionArn = Lens.lens (\GetPermission' {permissionArn} -> permissionArn) (\s@GetPermission' {} a -> s {permissionArn = a} :: GetPermission)

instance Core.AWSRequest GetPermission where
  type
    AWSResponse GetPermission =
      GetPermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPermissionResponse'
            Prelude.<$> (x Core..?> "permission")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPermission where
  hashWithSalt _salt GetPermission' {..} =
    _salt `Prelude.hashWithSalt` permissionVersion
      `Prelude.hashWithSalt` permissionArn

instance Prelude.NFData GetPermission where
  rnf GetPermission' {..} =
    Prelude.rnf permissionVersion
      `Prelude.seq` Prelude.rnf permissionArn

instance Core.ToHeaders GetPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPermission where
  toJSON GetPermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("permissionVersion" Core..=)
              Prelude.<$> permissionVersion,
            Prelude.Just
              ("permissionArn" Core..= permissionArn)
          ]
      )

instance Core.ToPath GetPermission where
  toPath = Prelude.const "/getpermission"

instance Core.ToQuery GetPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPermissionResponse' smart constructor.
data GetPermissionResponse = GetPermissionResponse'
  { -- | Information about the permission.
    permission :: Prelude.Maybe ResourceSharePermissionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 'getPermissionResponse_permission' - Information about the permission.
--
-- 'httpStatus', 'getPermissionResponse_httpStatus' - The response's http status code.
newGetPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPermissionResponse
newGetPermissionResponse pHttpStatus_ =
  GetPermissionResponse'
    { permission =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the permission.
getPermissionResponse_permission :: Lens.Lens' GetPermissionResponse (Prelude.Maybe ResourceSharePermissionDetail)
getPermissionResponse_permission = Lens.lens (\GetPermissionResponse' {permission} -> permission) (\s@GetPermissionResponse' {} a -> s {permission = a} :: GetPermissionResponse)

-- | The response's http status code.
getPermissionResponse_httpStatus :: Lens.Lens' GetPermissionResponse Prelude.Int
getPermissionResponse_httpStatus = Lens.lens (\GetPermissionResponse' {httpStatus} -> httpStatus) (\s@GetPermissionResponse' {} a -> s {httpStatus = a} :: GetPermissionResponse)

instance Prelude.NFData GetPermissionResponse where
  rnf GetPermissionResponse' {..} =
    Prelude.rnf permission
      `Prelude.seq` Prelude.rnf httpStatus
