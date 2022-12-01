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
-- Module      : Amazonka.RAM.ListPermissionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available versions of the specified RAM permission.
module Amazonka.RAM.ListPermissionVersions
  ( -- * Creating a Request
    ListPermissionVersions (..),
    newListPermissionVersions,

    -- * Request Lenses
    listPermissionVersions_nextToken,
    listPermissionVersions_maxResults,
    listPermissionVersions_permissionArn,

    -- * Destructuring the Response
    ListPermissionVersionsResponse (..),
    newListPermissionVersionsResponse,

    -- * Response Lenses
    listPermissionVersionsResponse_nextToken,
    listPermissionVersionsResponse_permissions,
    listPermissionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissionVersions' smart constructor.
data ListPermissionVersions = ListPermissionVersions'
  { -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the RAM permission whose versions you want to list. You can use the
    -- @permissionVersion@ parameter on the AssociateResourceSharePermission
    -- operation to specify a non-default version to attach.
    permissionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionVersions_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'maxResults', 'listPermissionVersions_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'permissionArn', 'listPermissionVersions_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the RAM permission whose versions you want to list. You can use the
-- @permissionVersion@ parameter on the AssociateResourceSharePermission
-- operation to specify a non-default version to attach.
newListPermissionVersions ::
  -- | 'permissionArn'
  Prelude.Text ->
  ListPermissionVersions
newListPermissionVersions pPermissionArn_ =
  ListPermissionVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      permissionArn = pPermissionArn_
    }

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listPermissionVersions_nextToken :: Lens.Lens' ListPermissionVersions (Prelude.Maybe Prelude.Text)
listPermissionVersions_nextToken = Lens.lens (\ListPermissionVersions' {nextToken} -> nextToken) (\s@ListPermissionVersions' {} a -> s {nextToken = a} :: ListPermissionVersions)

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listPermissionVersions_maxResults :: Lens.Lens' ListPermissionVersions (Prelude.Maybe Prelude.Natural)
listPermissionVersions_maxResults = Lens.lens (\ListPermissionVersions' {maxResults} -> maxResults) (\s@ListPermissionVersions' {} a -> s {maxResults = a} :: ListPermissionVersions)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the RAM permission whose versions you want to list. You can use the
-- @permissionVersion@ parameter on the AssociateResourceSharePermission
-- operation to specify a non-default version to attach.
listPermissionVersions_permissionArn :: Lens.Lens' ListPermissionVersions Prelude.Text
listPermissionVersions_permissionArn = Lens.lens (\ListPermissionVersions' {permissionArn} -> permissionArn) (\s@ListPermissionVersions' {} a -> s {permissionArn = a} :: ListPermissionVersions)

instance Core.AWSRequest ListPermissionVersions where
  type
    AWSResponse ListPermissionVersions =
      ListPermissionVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissionVersions where
  hashWithSalt _salt ListPermissionVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` permissionArn

instance Prelude.NFData ListPermissionVersions where
  rnf ListPermissionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf permissionArn

instance Core.ToHeaders ListPermissionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPermissionVersions where
  toJSON ListPermissionVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("permissionArn" Core..= permissionArn)
          ]
      )

instance Core.ToPath ListPermissionVersions where
  toPath = Prelude.const "/listpermissionversions"

instance Core.ToQuery ListPermissionVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionVersionsResponse' smart constructor.
data ListPermissionVersionsResponse = ListPermissionVersionsResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain details for each of the available
    -- versions.
    permissions :: Prelude.Maybe [ResourceSharePermissionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionVersionsResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'permissions', 'listPermissionVersionsResponse_permissions' - An array of objects that contain details for each of the available
-- versions.
--
-- 'httpStatus', 'listPermissionVersionsResponse_httpStatus' - The response's http status code.
newListPermissionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionVersionsResponse
newListPermissionVersionsResponse pHttpStatus_ =
  ListPermissionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listPermissionVersionsResponse_nextToken :: Lens.Lens' ListPermissionVersionsResponse (Prelude.Maybe Prelude.Text)
listPermissionVersionsResponse_nextToken = Lens.lens (\ListPermissionVersionsResponse' {nextToken} -> nextToken) (\s@ListPermissionVersionsResponse' {} a -> s {nextToken = a} :: ListPermissionVersionsResponse)

-- | An array of objects that contain details for each of the available
-- versions.
listPermissionVersionsResponse_permissions :: Lens.Lens' ListPermissionVersionsResponse (Prelude.Maybe [ResourceSharePermissionSummary])
listPermissionVersionsResponse_permissions = Lens.lens (\ListPermissionVersionsResponse' {permissions} -> permissions) (\s@ListPermissionVersionsResponse' {} a -> s {permissions = a} :: ListPermissionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionVersionsResponse_httpStatus :: Lens.Lens' ListPermissionVersionsResponse Prelude.Int
listPermissionVersionsResponse_httpStatus = Lens.lens (\ListPermissionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionVersionsResponse' {} a -> s {httpStatus = a} :: ListPermissionVersionsResponse)

instance
  Prelude.NFData
    ListPermissionVersionsResponse
  where
  rnf ListPermissionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus
