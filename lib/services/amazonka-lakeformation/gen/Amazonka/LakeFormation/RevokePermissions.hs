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
-- Module      : Amazonka.LakeFormation.RevokePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes permissions to the principal to access metadata in the Data
-- Catalog and data organized in underlying data storage such as Amazon S3.
module Amazonka.LakeFormation.RevokePermissions
  ( -- * Creating a Request
    RevokePermissions (..),
    newRevokePermissions,

    -- * Request Lenses
    revokePermissions_catalogId,
    revokePermissions_permissionsWithGrantOption,
    revokePermissions_principal,
    revokePermissions_resource,
    revokePermissions_permissions,

    -- * Destructuring the Response
    RevokePermissionsResponse (..),
    newRevokePermissionsResponse,

    -- * Response Lenses
    revokePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRevokePermissions' smart constructor.
data RevokePermissions = RevokePermissions'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Indicates a list of permissions for which to revoke the grant option
    -- allowing the principal to pass permissions to other principals.
    permissionsWithGrantOption :: Prelude.Maybe [Permission],
    -- | The principal to be revoked permissions on the resource.
    principal :: DataLakePrincipal,
    -- | The resource to which permissions are to be revoked.
    resource :: Resource,
    -- | The permissions revoked to the principal on the resource. For
    -- information about permissions, see
    -- <https://docs-aws.amazon.com/lake-formation/latest/dg/security-data-access.html Security and Access Control to Metadata and Data>.
    permissions :: [Permission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'revokePermissions_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'permissionsWithGrantOption', 'revokePermissions_permissionsWithGrantOption' - Indicates a list of permissions for which to revoke the grant option
-- allowing the principal to pass permissions to other principals.
--
-- 'principal', 'revokePermissions_principal' - The principal to be revoked permissions on the resource.
--
-- 'resource', 'revokePermissions_resource' - The resource to which permissions are to be revoked.
--
-- 'permissions', 'revokePermissions_permissions' - The permissions revoked to the principal on the resource. For
-- information about permissions, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/security-data-access.html Security and Access Control to Metadata and Data>.
newRevokePermissions ::
  -- | 'principal'
  DataLakePrincipal ->
  -- | 'resource'
  Resource ->
  RevokePermissions
newRevokePermissions pPrincipal_ pResource_ =
  RevokePermissions'
    { catalogId = Prelude.Nothing,
      permissionsWithGrantOption = Prelude.Nothing,
      principal = pPrincipal_,
      resource = pResource_,
      permissions = Prelude.mempty
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
revokePermissions_catalogId :: Lens.Lens' RevokePermissions (Prelude.Maybe Prelude.Text)
revokePermissions_catalogId = Lens.lens (\RevokePermissions' {catalogId} -> catalogId) (\s@RevokePermissions' {} a -> s {catalogId = a} :: RevokePermissions)

-- | Indicates a list of permissions for which to revoke the grant option
-- allowing the principal to pass permissions to other principals.
revokePermissions_permissionsWithGrantOption :: Lens.Lens' RevokePermissions (Prelude.Maybe [Permission])
revokePermissions_permissionsWithGrantOption = Lens.lens (\RevokePermissions' {permissionsWithGrantOption} -> permissionsWithGrantOption) (\s@RevokePermissions' {} a -> s {permissionsWithGrantOption = a} :: RevokePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The principal to be revoked permissions on the resource.
revokePermissions_principal :: Lens.Lens' RevokePermissions DataLakePrincipal
revokePermissions_principal = Lens.lens (\RevokePermissions' {principal} -> principal) (\s@RevokePermissions' {} a -> s {principal = a} :: RevokePermissions)

-- | The resource to which permissions are to be revoked.
revokePermissions_resource :: Lens.Lens' RevokePermissions Resource
revokePermissions_resource = Lens.lens (\RevokePermissions' {resource} -> resource) (\s@RevokePermissions' {} a -> s {resource = a} :: RevokePermissions)

-- | The permissions revoked to the principal on the resource. For
-- information about permissions, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/security-data-access.html Security and Access Control to Metadata and Data>.
revokePermissions_permissions :: Lens.Lens' RevokePermissions [Permission]
revokePermissions_permissions = Lens.lens (\RevokePermissions' {permissions} -> permissions) (\s@RevokePermissions' {} a -> s {permissions = a} :: RevokePermissions) Prelude.. Lens.coerced

instance Core.AWSRequest RevokePermissions where
  type
    AWSResponse RevokePermissions =
      RevokePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokePermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokePermissions where
  hashWithSalt _salt RevokePermissions' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` permissionsWithGrantOption
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` permissions

instance Prelude.NFData RevokePermissions where
  rnf RevokePermissions' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf permissionsWithGrantOption
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf permissions

instance Data.ToHeaders RevokePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RevokePermissions where
  toJSON RevokePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("PermissionsWithGrantOption" Data..=)
              Prelude.<$> permissionsWithGrantOption,
            Prelude.Just ("Principal" Data..= principal),
            Prelude.Just ("Resource" Data..= resource),
            Prelude.Just ("Permissions" Data..= permissions)
          ]
      )

instance Data.ToPath RevokePermissions where
  toPath = Prelude.const "/RevokePermissions"

instance Data.ToQuery RevokePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRevokePermissionsResponse' smart constructor.
data RevokePermissionsResponse = RevokePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'revokePermissionsResponse_httpStatus' - The response's http status code.
newRevokePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokePermissionsResponse
newRevokePermissionsResponse pHttpStatus_ =
  RevokePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
revokePermissionsResponse_httpStatus :: Lens.Lens' RevokePermissionsResponse Prelude.Int
revokePermissionsResponse_httpStatus = Lens.lens (\RevokePermissionsResponse' {httpStatus} -> httpStatus) (\s@RevokePermissionsResponse' {} a -> s {httpStatus = a} :: RevokePermissionsResponse)

instance Prelude.NFData RevokePermissionsResponse where
  rnf RevokePermissionsResponse' {..} =
    Prelude.rnf httpStatus
