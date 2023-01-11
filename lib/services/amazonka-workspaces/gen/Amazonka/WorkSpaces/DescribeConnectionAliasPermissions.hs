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
-- Module      : Amazonka.WorkSpaces.DescribeConnectionAliasPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of a connection alias has
-- granted to another Amazon Web Services account for the specified
-- connection alias. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
module Amazonka.WorkSpaces.DescribeConnectionAliasPermissions
  ( -- * Creating a Request
    DescribeConnectionAliasPermissions (..),
    newDescribeConnectionAliasPermissions,

    -- * Request Lenses
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_aliasId,

    -- * Destructuring the Response
    DescribeConnectionAliasPermissionsResponse (..),
    newDescribeConnectionAliasPermissionsResponse,

    -- * Response Lenses
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeConnectionAliasPermissions' smart constructor.
data DescribeConnectionAliasPermissions = DescribeConnectionAliasPermissions'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the connection alias.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeConnectionAliasPermissions_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'describeConnectionAliasPermissions_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'aliasId', 'describeConnectionAliasPermissions_aliasId' - The identifier of the connection alias.
newDescribeConnectionAliasPermissions ::
  -- | 'aliasId'
  Prelude.Text ->
  DescribeConnectionAliasPermissions
newDescribeConnectionAliasPermissions pAliasId_ =
  DescribeConnectionAliasPermissions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      aliasId = pAliasId_
    }

-- | The maximum number of results to return.
describeConnectionAliasPermissions_maxResults :: Lens.Lens' DescribeConnectionAliasPermissions (Prelude.Maybe Prelude.Natural)
describeConnectionAliasPermissions_maxResults = Lens.lens (\DescribeConnectionAliasPermissions' {maxResults} -> maxResults) (\s@DescribeConnectionAliasPermissions' {} a -> s {maxResults = a} :: DescribeConnectionAliasPermissions)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeConnectionAliasPermissions_nextToken :: Lens.Lens' DescribeConnectionAliasPermissions (Prelude.Maybe Prelude.Text)
describeConnectionAliasPermissions_nextToken = Lens.lens (\DescribeConnectionAliasPermissions' {nextToken} -> nextToken) (\s@DescribeConnectionAliasPermissions' {} a -> s {nextToken = a} :: DescribeConnectionAliasPermissions)

-- | The identifier of the connection alias.
describeConnectionAliasPermissions_aliasId :: Lens.Lens' DescribeConnectionAliasPermissions Prelude.Text
describeConnectionAliasPermissions_aliasId = Lens.lens (\DescribeConnectionAliasPermissions' {aliasId} -> aliasId) (\s@DescribeConnectionAliasPermissions' {} a -> s {aliasId = a} :: DescribeConnectionAliasPermissions)

instance
  Core.AWSRequest
    DescribeConnectionAliasPermissions
  where
  type
    AWSResponse DescribeConnectionAliasPermissions =
      DescribeConnectionAliasPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasPermissionsResponse'
            Prelude.<$> (x Data..?> "AliasId")
              Prelude.<*> (x Data..?> "ConnectionAliasPermissions")
              Prelude.<*> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConnectionAliasPermissions
  where
  hashWithSalt
    _salt
    DescribeConnectionAliasPermissions' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` aliasId

instance
  Prelude.NFData
    DescribeConnectionAliasPermissions
  where
  rnf DescribeConnectionAliasPermissions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf aliasId

instance
  Data.ToHeaders
    DescribeConnectionAliasPermissions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeConnectionAliasPermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeConnectionAliasPermissions
  where
  toJSON DescribeConnectionAliasPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("AliasId" Data..= aliasId)
          ]
      )

instance
  Data.ToPath
    DescribeConnectionAliasPermissions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeConnectionAliasPermissions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConnectionAliasPermissionsResponse' smart constructor.
data DescribeConnectionAliasPermissionsResponse = DescribeConnectionAliasPermissionsResponse'
  { -- | The identifier of the connection alias.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | The permissions associated with a connection alias.
    connectionAliasPermissions :: Prelude.Maybe (Prelude.NonEmpty ConnectionAliasPermission),
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'describeConnectionAliasPermissionsResponse_aliasId' - The identifier of the connection alias.
--
-- 'connectionAliasPermissions', 'describeConnectionAliasPermissionsResponse_connectionAliasPermissions' - The permissions associated with a connection alias.
--
-- 'nextToken', 'describeConnectionAliasPermissionsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeConnectionAliasPermissionsResponse_httpStatus' - The response's http status code.
newDescribeConnectionAliasPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectionAliasPermissionsResponse
newDescribeConnectionAliasPermissionsResponse
  pHttpStatus_ =
    DescribeConnectionAliasPermissionsResponse'
      { aliasId =
          Prelude.Nothing,
        connectionAliasPermissions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier of the connection alias.
describeConnectionAliasPermissionsResponse_aliasId :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Prelude.Maybe Prelude.Text)
describeConnectionAliasPermissionsResponse_aliasId = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {aliasId} -> aliasId) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {aliasId = a} :: DescribeConnectionAliasPermissionsResponse)

-- | The permissions associated with a connection alias.
describeConnectionAliasPermissionsResponse_connectionAliasPermissions :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ConnectionAliasPermission))
describeConnectionAliasPermissionsResponse_connectionAliasPermissions = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {connectionAliasPermissions} -> connectionAliasPermissions) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {connectionAliasPermissions = a} :: DescribeConnectionAliasPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeConnectionAliasPermissionsResponse_nextToken :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Prelude.Maybe Prelude.Text)
describeConnectionAliasPermissionsResponse_nextToken = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {nextToken} -> nextToken) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {nextToken = a} :: DescribeConnectionAliasPermissionsResponse)

-- | The response's http status code.
describeConnectionAliasPermissionsResponse_httpStatus :: Lens.Lens' DescribeConnectionAliasPermissionsResponse Prelude.Int
describeConnectionAliasPermissionsResponse_httpStatus = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {httpStatus = a} :: DescribeConnectionAliasPermissionsResponse)

instance
  Prelude.NFData
    DescribeConnectionAliasPermissionsResponse
  where
  rnf DescribeConnectionAliasPermissionsResponse' {..} =
    Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf connectionAliasPermissions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
