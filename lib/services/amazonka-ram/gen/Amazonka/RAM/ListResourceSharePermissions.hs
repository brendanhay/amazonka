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
-- Module      : Amazonka.RAM.ListResourceSharePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the RAM permissions that are associated with a resource share.
module Amazonka.RAM.ListResourceSharePermissions
  ( -- * Creating a Request
    ListResourceSharePermissions (..),
    newListResourceSharePermissions,

    -- * Request Lenses
    listResourceSharePermissions_maxResults,
    listResourceSharePermissions_nextToken,
    listResourceSharePermissions_resourceShareArn,

    -- * Destructuring the Response
    ListResourceSharePermissionsResponse (..),
    newListResourceSharePermissionsResponse,

    -- * Response Lenses
    listResourceSharePermissionsResponse_nextToken,
    listResourceSharePermissionsResponse_permissions,
    listResourceSharePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceSharePermissions' smart constructor.
data ListResourceSharePermissions = ListResourceSharePermissions'
  { -- | Specifies the total number of results that you want included on each
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
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the resource share for which you want to retrieve the associated
    -- permissions.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSharePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceSharePermissions_maxResults' - Specifies the total number of results that you want included on each
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
-- 'nextToken', 'listResourceSharePermissions_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'resourceShareArn', 'listResourceSharePermissions_resourceShareArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the resource share for which you want to retrieve the associated
-- permissions.
newListResourceSharePermissions ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  ListResourceSharePermissions
newListResourceSharePermissions pResourceShareArn_ =
  ListResourceSharePermissions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
    }

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
listResourceSharePermissions_maxResults :: Lens.Lens' ListResourceSharePermissions (Prelude.Maybe Prelude.Natural)
listResourceSharePermissions_maxResults = Lens.lens (\ListResourceSharePermissions' {maxResults} -> maxResults) (\s@ListResourceSharePermissions' {} a -> s {maxResults = a} :: ListResourceSharePermissions)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listResourceSharePermissions_nextToken :: Lens.Lens' ListResourceSharePermissions (Prelude.Maybe Prelude.Text)
listResourceSharePermissions_nextToken = Lens.lens (\ListResourceSharePermissions' {nextToken} -> nextToken) (\s@ListResourceSharePermissions' {} a -> s {nextToken = a} :: ListResourceSharePermissions)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the resource share for which you want to retrieve the associated
-- permissions.
listResourceSharePermissions_resourceShareArn :: Lens.Lens' ListResourceSharePermissions Prelude.Text
listResourceSharePermissions_resourceShareArn = Lens.lens (\ListResourceSharePermissions' {resourceShareArn} -> resourceShareArn) (\s@ListResourceSharePermissions' {} a -> s {resourceShareArn = a} :: ListResourceSharePermissions)

instance Core.AWSRequest ListResourceSharePermissions where
  type
    AWSResponse ListResourceSharePermissions =
      ListResourceSharePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceSharePermissionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceSharePermissions
  where
  hashWithSalt _salt ListResourceSharePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData ListResourceSharePermissions where
  rnf ListResourceSharePermissions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Data.ToHeaders ListResourceSharePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceSharePermissions where
  toJSON ListResourceSharePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("resourceShareArn" Data..= resourceShareArn)
          ]
      )

instance Data.ToPath ListResourceSharePermissions where
  toPath =
    Prelude.const "/listresourcesharepermissions"

instance Data.ToQuery ListResourceSharePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceSharePermissionsResponse' smart constructor.
data ListResourceSharePermissionsResponse = ListResourceSharePermissionsResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the permissions associated with the
    -- resource share.
    permissions :: Prelude.Maybe [ResourceSharePermissionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSharePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSharePermissionsResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'permissions', 'listResourceSharePermissionsResponse_permissions' - An array of objects that describe the permissions associated with the
-- resource share.
--
-- 'httpStatus', 'listResourceSharePermissionsResponse_httpStatus' - The response's http status code.
newListResourceSharePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceSharePermissionsResponse
newListResourceSharePermissionsResponse pHttpStatus_ =
  ListResourceSharePermissionsResponse'
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
listResourceSharePermissionsResponse_nextToken :: Lens.Lens' ListResourceSharePermissionsResponse (Prelude.Maybe Prelude.Text)
listResourceSharePermissionsResponse_nextToken = Lens.lens (\ListResourceSharePermissionsResponse' {nextToken} -> nextToken) (\s@ListResourceSharePermissionsResponse' {} a -> s {nextToken = a} :: ListResourceSharePermissionsResponse)

-- | An array of objects that describe the permissions associated with the
-- resource share.
listResourceSharePermissionsResponse_permissions :: Lens.Lens' ListResourceSharePermissionsResponse (Prelude.Maybe [ResourceSharePermissionSummary])
listResourceSharePermissionsResponse_permissions = Lens.lens (\ListResourceSharePermissionsResponse' {permissions} -> permissions) (\s@ListResourceSharePermissionsResponse' {} a -> s {permissions = a} :: ListResourceSharePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResourceSharePermissionsResponse_httpStatus :: Lens.Lens' ListResourceSharePermissionsResponse Prelude.Int
listResourceSharePermissionsResponse_httpStatus = Lens.lens (\ListResourceSharePermissionsResponse' {httpStatus} -> httpStatus) (\s@ListResourceSharePermissionsResponse' {} a -> s {httpStatus = a} :: ListResourceSharePermissionsResponse)

instance
  Prelude.NFData
    ListResourceSharePermissionsResponse
  where
  rnf ListResourceSharePermissionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus
