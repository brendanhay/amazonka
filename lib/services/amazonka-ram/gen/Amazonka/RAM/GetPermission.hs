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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPermission' smart constructor.
data GetPermission = GetPermission'
  { -- | Specifies identifier for the version of the RAM permission to retrieve.
    -- If you don\'t specify this parameter, the operation retrieves the
    -- default version.
    permissionVersion :: Prelude.Maybe Prelude.Int,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the permission whose contents you want to retrieve. To find the ARN
    -- for a permission, use either the ListPermissions operation or go to the
    -- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
    -- page in the RAM console and then choose the name of the permission. The
    -- ARN is displayed on the detail page.
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
-- 'permissionVersion', 'getPermission_permissionVersion' - Specifies identifier for the version of the RAM permission to retrieve.
-- If you don\'t specify this parameter, the operation retrieves the
-- default version.
--
-- 'permissionArn', 'getPermission_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission whose contents you want to retrieve. To find the ARN
-- for a permission, use either the ListPermissions operation or go to the
-- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
-- page in the RAM console and then choose the name of the permission. The
-- ARN is displayed on the detail page.
newGetPermission ::
  -- | 'permissionArn'
  Prelude.Text ->
  GetPermission
newGetPermission pPermissionArn_ =
  GetPermission'
    { permissionVersion = Prelude.Nothing,
      permissionArn = pPermissionArn_
    }

-- | Specifies identifier for the version of the RAM permission to retrieve.
-- If you don\'t specify this parameter, the operation retrieves the
-- default version.
getPermission_permissionVersion :: Lens.Lens' GetPermission (Prelude.Maybe Prelude.Int)
getPermission_permissionVersion = Lens.lens (\GetPermission' {permissionVersion} -> permissionVersion) (\s@GetPermission' {} a -> s {permissionVersion = a} :: GetPermission)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the permission whose contents you want to retrieve. To find the ARN
-- for a permission, use either the ListPermissions operation or go to the
-- <https://console.aws.amazon.com/ram/home#Permissions: Permissions library>
-- page in the RAM console and then choose the name of the permission. The
-- ARN is displayed on the detail page.
getPermission_permissionArn :: Lens.Lens' GetPermission Prelude.Text
getPermission_permissionArn = Lens.lens (\GetPermission' {permissionArn} -> permissionArn) (\s@GetPermission' {} a -> s {permissionArn = a} :: GetPermission)

instance Core.AWSRequest GetPermission where
  type
    AWSResponse GetPermission =
      GetPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPermissionResponse'
            Prelude.<$> (x Data..?> "permission")
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

instance Data.ToHeaders GetPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPermission where
  toJSON GetPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("permissionVersion" Data..=)
              Prelude.<$> permissionVersion,
            Prelude.Just
              ("permissionArn" Data..= permissionArn)
          ]
      )

instance Data.ToPath GetPermission where
  toPath = Prelude.const "/getpermission"

instance Data.ToQuery GetPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPermissionResponse' smart constructor.
data GetPermissionResponse = GetPermissionResponse'
  { -- | An object that contains information about the permission.
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
-- 'permission', 'getPermissionResponse_permission' - An object that contains information about the permission.
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

-- | An object that contains information about the permission.
getPermissionResponse_permission :: Lens.Lens' GetPermissionResponse (Prelude.Maybe ResourceSharePermissionDetail)
getPermissionResponse_permission = Lens.lens (\GetPermissionResponse' {permission} -> permission) (\s@GetPermissionResponse' {} a -> s {permission = a} :: GetPermissionResponse)

-- | The response's http status code.
getPermissionResponse_httpStatus :: Lens.Lens' GetPermissionResponse Prelude.Int
getPermissionResponse_httpStatus = Lens.lens (\GetPermissionResponse' {httpStatus} -> httpStatus) (\s@GetPermissionResponse' {} a -> s {httpStatus = a} :: GetPermissionResponse)

instance Prelude.NFData GetPermissionResponse where
  rnf GetPermissionResponse' {..} =
    Prelude.rnf permission
      `Prelude.seq` Prelude.rnf httpStatus
