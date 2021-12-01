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
-- Module      : Amazonka.LakeFormation.ListPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the principal permissions on the resource, filtered by
-- the permissions of the caller. For example, if you are granted an ALTER
-- permission, you are able to see only the principal permissions for
-- ALTER.
--
-- This operation returns only those permissions that have been explicitly
-- granted.
--
-- For information about permissions, see
-- <https://docs-aws.amazon.com/lake-formation/latest/dg/security-data-access.html Security and Access Control to Metadata and Data>.
module Amazonka.LakeFormation.ListPermissions
  ( -- * Creating a Request
    ListPermissions (..),
    newListPermissions,

    -- * Request Lenses
    listPermissions_resourceType,
    listPermissions_catalogId,
    listPermissions_nextToken,
    listPermissions_principal,
    listPermissions_resource,
    listPermissions_maxResults,

    -- * Destructuring the Response
    ListPermissionsResponse (..),
    newListPermissionsResponse,

    -- * Response Lenses
    listPermissionsResponse_nextToken,
    listPermissionsResponse_principalResourcePermissions,
    listPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissions' smart constructor.
data ListPermissions = ListPermissions'
  { -- | Specifies a resource type to filter the permissions returned.
    resourceType :: Prelude.Maybe DataLakeResourceType,
    -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A continuation token, if this is not the first call to retrieve this
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a principal to filter the permissions returned.
    principal :: Prelude.Maybe DataLakePrincipal,
    -- | A resource where you will get a list of the principal permissions.
    --
    -- This operation does not support getting privileges on a table with
    -- columns. Instead, call this operation on the table, and the operation
    -- returns the table and the table w columns.
    resource :: Prelude.Maybe Resource,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listPermissions_resourceType' - Specifies a resource type to filter the permissions returned.
--
-- 'catalogId', 'listPermissions_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'nextToken', 'listPermissions_nextToken' - A continuation token, if this is not the first call to retrieve this
-- list.
--
-- 'principal', 'listPermissions_principal' - Specifies a principal to filter the permissions returned.
--
-- 'resource', 'listPermissions_resource' - A resource where you will get a list of the principal permissions.
--
-- This operation does not support getting privileges on a table with
-- columns. Instead, call this operation on the table, and the operation
-- returns the table and the table w columns.
--
-- 'maxResults', 'listPermissions_maxResults' - The maximum number of results to return.
newListPermissions ::
  ListPermissions
newListPermissions =
  ListPermissions'
    { resourceType = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principal = Prelude.Nothing,
      resource = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Specifies a resource type to filter the permissions returned.
listPermissions_resourceType :: Lens.Lens' ListPermissions (Prelude.Maybe DataLakeResourceType)
listPermissions_resourceType = Lens.lens (\ListPermissions' {resourceType} -> resourceType) (\s@ListPermissions' {} a -> s {resourceType = a} :: ListPermissions)

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
listPermissions_catalogId :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_catalogId = Lens.lens (\ListPermissions' {catalogId} -> catalogId) (\s@ListPermissions' {} a -> s {catalogId = a} :: ListPermissions)

-- | A continuation token, if this is not the first call to retrieve this
-- list.
listPermissions_nextToken :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Text)
listPermissions_nextToken = Lens.lens (\ListPermissions' {nextToken} -> nextToken) (\s@ListPermissions' {} a -> s {nextToken = a} :: ListPermissions)

-- | Specifies a principal to filter the permissions returned.
listPermissions_principal :: Lens.Lens' ListPermissions (Prelude.Maybe DataLakePrincipal)
listPermissions_principal = Lens.lens (\ListPermissions' {principal} -> principal) (\s@ListPermissions' {} a -> s {principal = a} :: ListPermissions)

-- | A resource where you will get a list of the principal permissions.
--
-- This operation does not support getting privileges on a table with
-- columns. Instead, call this operation on the table, and the operation
-- returns the table and the table w columns.
listPermissions_resource :: Lens.Lens' ListPermissions (Prelude.Maybe Resource)
listPermissions_resource = Lens.lens (\ListPermissions' {resource} -> resource) (\s@ListPermissions' {} a -> s {resource = a} :: ListPermissions)

-- | The maximum number of results to return.
listPermissions_maxResults :: Lens.Lens' ListPermissions (Prelude.Maybe Prelude.Natural)
listPermissions_maxResults = Lens.lens (\ListPermissions' {maxResults} -> maxResults) (\s@ListPermissions' {} a -> s {maxResults = a} :: ListPermissions)

instance Core.AWSRequest ListPermissions where
  type
    AWSResponse ListPermissions =
      ListPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "PrincipalResourcePermissions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissions where
  hashWithSalt salt' ListPermissions' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListPermissions where
  rnf ListPermissions' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf catalogId

instance Core.ToHeaders ListPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.ListPermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPermissions where
  toJSON ListPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Principal" Core..=) Prelude.<$> principal,
            ("Resource" Core..=) Prelude.<$> resource,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPermissions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionsResponse' smart constructor.
data ListPermissionsResponse = ListPermissionsResponse'
  { -- | A continuation token, if this is not the first call to retrieve this
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of principals and their permissions on the resource for the
    -- specified principal and resource types.
    principalResourcePermissions :: Prelude.Maybe [PrincipalResourcePermissions],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionsResponse_nextToken' - A continuation token, if this is not the first call to retrieve this
-- list.
--
-- 'principalResourcePermissions', 'listPermissionsResponse_principalResourcePermissions' - A list of principals and their permissions on the resource for the
-- specified principal and resource types.
--
-- 'httpStatus', 'listPermissionsResponse_httpStatus' - The response's http status code.
newListPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionsResponse
newListPermissionsResponse pHttpStatus_ =
  ListPermissionsResponse'
    { nextToken =
        Prelude.Nothing,
      principalResourcePermissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if this is not the first call to retrieve this
-- list.
listPermissionsResponse_nextToken :: Lens.Lens' ListPermissionsResponse (Prelude.Maybe Prelude.Text)
listPermissionsResponse_nextToken = Lens.lens (\ListPermissionsResponse' {nextToken} -> nextToken) (\s@ListPermissionsResponse' {} a -> s {nextToken = a} :: ListPermissionsResponse)

-- | A list of principals and their permissions on the resource for the
-- specified principal and resource types.
listPermissionsResponse_principalResourcePermissions :: Lens.Lens' ListPermissionsResponse (Prelude.Maybe [PrincipalResourcePermissions])
listPermissionsResponse_principalResourcePermissions = Lens.lens (\ListPermissionsResponse' {principalResourcePermissions} -> principalResourcePermissions) (\s@ListPermissionsResponse' {} a -> s {principalResourcePermissions = a} :: ListPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionsResponse_httpStatus :: Lens.Lens' ListPermissionsResponse Prelude.Int
listPermissionsResponse_httpStatus = Lens.lens (\ListPermissionsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionsResponse' {} a -> s {httpStatus = a} :: ListPermissionsResponse)

instance Prelude.NFData ListPermissionsResponse where
  rnf ListPermissionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf principalResourcePermissions
