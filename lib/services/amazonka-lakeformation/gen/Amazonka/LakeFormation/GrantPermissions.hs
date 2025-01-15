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
-- Module      : Amazonka.LakeFormation.GrantPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permissions to the principal to access metadata in the Data
-- Catalog and data organized in underlying data storage such as Amazon S3.
--
-- For information about permissions, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/security-data-access.html Security and Access Control to Metadata and Data>.
module Amazonka.LakeFormation.GrantPermissions
  ( -- * Creating a Request
    GrantPermissions (..),
    newGrantPermissions,

    -- * Request Lenses
    grantPermissions_catalogId,
    grantPermissions_permissionsWithGrantOption,
    grantPermissions_principal,
    grantPermissions_resource,
    grantPermissions_permissions,

    -- * Destructuring the Response
    GrantPermissionsResponse (..),
    newGrantPermissionsResponse,

    -- * Response Lenses
    grantPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGrantPermissions' smart constructor.
data GrantPermissions = GrantPermissions'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Indicates a list of the granted permissions that the principal may pass
    -- to other users. These permissions may only be a subset of the
    -- permissions granted in the @Privileges@.
    permissionsWithGrantOption :: Prelude.Maybe [Permission],
    -- | The principal to be granted the permissions on the resource. Supported
    -- principals are IAM users or IAM roles, and they are defined by their
    -- principal type and their ARN.
    --
    -- Note that if you define a resource with a particular ARN, then later
    -- delete, and recreate a resource with that same ARN, the resource
    -- maintains the permissions already granted.
    principal :: DataLakePrincipal,
    -- | The resource to which permissions are to be granted. Resources in Lake
    -- Formation are the Data Catalog, databases, and tables.
    resource :: Resource,
    -- | The permissions granted to the principal on the resource. Lake Formation
    -- defines privileges to grant and revoke access to metadata in the Data
    -- Catalog and data organized in underlying data storage such as Amazon S3.
    -- Lake Formation requires that each principal be authorized to perform a
    -- specific task on Lake Formation resources.
    permissions :: [Permission]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'grantPermissions_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'permissionsWithGrantOption', 'grantPermissions_permissionsWithGrantOption' - Indicates a list of the granted permissions that the principal may pass
-- to other users. These permissions may only be a subset of the
-- permissions granted in the @Privileges@.
--
-- 'principal', 'grantPermissions_principal' - The principal to be granted the permissions on the resource. Supported
-- principals are IAM users or IAM roles, and they are defined by their
-- principal type and their ARN.
--
-- Note that if you define a resource with a particular ARN, then later
-- delete, and recreate a resource with that same ARN, the resource
-- maintains the permissions already granted.
--
-- 'resource', 'grantPermissions_resource' - The resource to which permissions are to be granted. Resources in Lake
-- Formation are the Data Catalog, databases, and tables.
--
-- 'permissions', 'grantPermissions_permissions' - The permissions granted to the principal on the resource. Lake Formation
-- defines privileges to grant and revoke access to metadata in the Data
-- Catalog and data organized in underlying data storage such as Amazon S3.
-- Lake Formation requires that each principal be authorized to perform a
-- specific task on Lake Formation resources.
newGrantPermissions ::
  -- | 'principal'
  DataLakePrincipal ->
  -- | 'resource'
  Resource ->
  GrantPermissions
newGrantPermissions pPrincipal_ pResource_ =
  GrantPermissions'
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
grantPermissions_catalogId :: Lens.Lens' GrantPermissions (Prelude.Maybe Prelude.Text)
grantPermissions_catalogId = Lens.lens (\GrantPermissions' {catalogId} -> catalogId) (\s@GrantPermissions' {} a -> s {catalogId = a} :: GrantPermissions)

-- | Indicates a list of the granted permissions that the principal may pass
-- to other users. These permissions may only be a subset of the
-- permissions granted in the @Privileges@.
grantPermissions_permissionsWithGrantOption :: Lens.Lens' GrantPermissions (Prelude.Maybe [Permission])
grantPermissions_permissionsWithGrantOption = Lens.lens (\GrantPermissions' {permissionsWithGrantOption} -> permissionsWithGrantOption) (\s@GrantPermissions' {} a -> s {permissionsWithGrantOption = a} :: GrantPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The principal to be granted the permissions on the resource. Supported
-- principals are IAM users or IAM roles, and they are defined by their
-- principal type and their ARN.
--
-- Note that if you define a resource with a particular ARN, then later
-- delete, and recreate a resource with that same ARN, the resource
-- maintains the permissions already granted.
grantPermissions_principal :: Lens.Lens' GrantPermissions DataLakePrincipal
grantPermissions_principal = Lens.lens (\GrantPermissions' {principal} -> principal) (\s@GrantPermissions' {} a -> s {principal = a} :: GrantPermissions)

-- | The resource to which permissions are to be granted. Resources in Lake
-- Formation are the Data Catalog, databases, and tables.
grantPermissions_resource :: Lens.Lens' GrantPermissions Resource
grantPermissions_resource = Lens.lens (\GrantPermissions' {resource} -> resource) (\s@GrantPermissions' {} a -> s {resource = a} :: GrantPermissions)

-- | The permissions granted to the principal on the resource. Lake Formation
-- defines privileges to grant and revoke access to metadata in the Data
-- Catalog and data organized in underlying data storage such as Amazon S3.
-- Lake Formation requires that each principal be authorized to perform a
-- specific task on Lake Formation resources.
grantPermissions_permissions :: Lens.Lens' GrantPermissions [Permission]
grantPermissions_permissions = Lens.lens (\GrantPermissions' {permissions} -> permissions) (\s@GrantPermissions' {} a -> s {permissions = a} :: GrantPermissions) Prelude.. Lens.coerced

instance Core.AWSRequest GrantPermissions where
  type
    AWSResponse GrantPermissions =
      GrantPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          GrantPermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GrantPermissions where
  hashWithSalt _salt GrantPermissions' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` permissionsWithGrantOption
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` permissions

instance Prelude.NFData GrantPermissions where
  rnf GrantPermissions' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf permissionsWithGrantOption `Prelude.seq`
        Prelude.rnf principal `Prelude.seq`
          Prelude.rnf resource `Prelude.seq`
            Prelude.rnf permissions

instance Data.ToHeaders GrantPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GrantPermissions where
  toJSON GrantPermissions' {..} =
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

instance Data.ToPath GrantPermissions where
  toPath = Prelude.const "/GrantPermissions"

instance Data.ToQuery GrantPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGrantPermissionsResponse' smart constructor.
data GrantPermissionsResponse = GrantPermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrantPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'grantPermissionsResponse_httpStatus' - The response's http status code.
newGrantPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GrantPermissionsResponse
newGrantPermissionsResponse pHttpStatus_ =
  GrantPermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
grantPermissionsResponse_httpStatus :: Lens.Lens' GrantPermissionsResponse Prelude.Int
grantPermissionsResponse_httpStatus = Lens.lens (\GrantPermissionsResponse' {httpStatus} -> httpStatus) (\s@GrantPermissionsResponse' {} a -> s {httpStatus = a} :: GrantPermissionsResponse)

instance Prelude.NFData GrantPermissionsResponse where
  rnf GrantPermissionsResponse' {..} =
    Prelude.rnf httpStatus
