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
-- Module      : Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions that the owner of a connection alias has
-- granted to another AWS account for the specified connection alias. For
-- more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
module Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
  ( -- * Creating a Request
    DescribeConnectionAliasPermissions (..),
    newDescribeConnectionAliasPermissions,

    -- * Request Lenses
    describeConnectionAliasPermissions_nextToken,
    describeConnectionAliasPermissions_maxResults,
    describeConnectionAliasPermissions_aliasId,

    -- * Destructuring the Response
    DescribeConnectionAliasPermissionsResponse (..),
    newDescribeConnectionAliasPermissionsResponse,

    -- * Response Lenses
    describeConnectionAliasPermissionsResponse_nextToken,
    describeConnectionAliasPermissionsResponse_aliasId,
    describeConnectionAliasPermissionsResponse_connectionAliasPermissions,
    describeConnectionAliasPermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeConnectionAliasPermissions' smart constructor.
data DescribeConnectionAliasPermissions = DescribeConnectionAliasPermissions'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the connection alias.
    aliasId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConnectionAliasPermissions_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'maxResults', 'describeConnectionAliasPermissions_maxResults' - The maximum number of results to return.
--
-- 'aliasId', 'describeConnectionAliasPermissions_aliasId' - The identifier of the connection alias.
newDescribeConnectionAliasPermissions ::
  -- | 'aliasId'
  Core.Text ->
  DescribeConnectionAliasPermissions
newDescribeConnectionAliasPermissions pAliasId_ =
  DescribeConnectionAliasPermissions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      aliasId = pAliasId_
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeConnectionAliasPermissions_nextToken :: Lens.Lens' DescribeConnectionAliasPermissions (Core.Maybe Core.Text)
describeConnectionAliasPermissions_nextToken = Lens.lens (\DescribeConnectionAliasPermissions' {nextToken} -> nextToken) (\s@DescribeConnectionAliasPermissions' {} a -> s {nextToken = a} :: DescribeConnectionAliasPermissions)

-- | The maximum number of results to return.
describeConnectionAliasPermissions_maxResults :: Lens.Lens' DescribeConnectionAliasPermissions (Core.Maybe Core.Natural)
describeConnectionAliasPermissions_maxResults = Lens.lens (\DescribeConnectionAliasPermissions' {maxResults} -> maxResults) (\s@DescribeConnectionAliasPermissions' {} a -> s {maxResults = a} :: DescribeConnectionAliasPermissions)

-- | The identifier of the connection alias.
describeConnectionAliasPermissions_aliasId :: Lens.Lens' DescribeConnectionAliasPermissions Core.Text
describeConnectionAliasPermissions_aliasId = Lens.lens (\DescribeConnectionAliasPermissions' {aliasId} -> aliasId) (\s@DescribeConnectionAliasPermissions' {} a -> s {aliasId = a} :: DescribeConnectionAliasPermissions)

instance
  Core.AWSRequest
    DescribeConnectionAliasPermissions
  where
  type
    AWSResponse DescribeConnectionAliasPermissions =
      DescribeConnectionAliasPermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionAliasPermissionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AliasId")
            Core.<*> (x Core..?> "ConnectionAliasPermissions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeConnectionAliasPermissions

instance
  Core.NFData
    DescribeConnectionAliasPermissions

instance
  Core.ToHeaders
    DescribeConnectionAliasPermissions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeConnectionAliasPermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeConnectionAliasPermissions
  where
  toJSON DescribeConnectionAliasPermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AliasId" Core..= aliasId)
          ]
      )

instance
  Core.ToPath
    DescribeConnectionAliasPermissions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeConnectionAliasPermissions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeConnectionAliasPermissionsResponse' smart constructor.
data DescribeConnectionAliasPermissionsResponse = DescribeConnectionAliasPermissionsResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifier of the connection alias.
    aliasId :: Core.Maybe Core.Text,
    -- | The permissions associated with a connection alias.
    connectionAliasPermissions :: Core.Maybe (Core.NonEmpty ConnectionAliasPermission),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConnectionAliasPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConnectionAliasPermissionsResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'aliasId', 'describeConnectionAliasPermissionsResponse_aliasId' - The identifier of the connection alias.
--
-- 'connectionAliasPermissions', 'describeConnectionAliasPermissionsResponse_connectionAliasPermissions' - The permissions associated with a connection alias.
--
-- 'httpStatus', 'describeConnectionAliasPermissionsResponse_httpStatus' - The response's http status code.
newDescribeConnectionAliasPermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConnectionAliasPermissionsResponse
newDescribeConnectionAliasPermissionsResponse
  pHttpStatus_ =
    DescribeConnectionAliasPermissionsResponse'
      { nextToken =
          Core.Nothing,
        aliasId = Core.Nothing,
        connectionAliasPermissions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeConnectionAliasPermissionsResponse_nextToken :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe Core.Text)
describeConnectionAliasPermissionsResponse_nextToken = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {nextToken} -> nextToken) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {nextToken = a} :: DescribeConnectionAliasPermissionsResponse)

-- | The identifier of the connection alias.
describeConnectionAliasPermissionsResponse_aliasId :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe Core.Text)
describeConnectionAliasPermissionsResponse_aliasId = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {aliasId} -> aliasId) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {aliasId = a} :: DescribeConnectionAliasPermissionsResponse)

-- | The permissions associated with a connection alias.
describeConnectionAliasPermissionsResponse_connectionAliasPermissions :: Lens.Lens' DescribeConnectionAliasPermissionsResponse (Core.Maybe (Core.NonEmpty ConnectionAliasPermission))
describeConnectionAliasPermissionsResponse_connectionAliasPermissions = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {connectionAliasPermissions} -> connectionAliasPermissions) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {connectionAliasPermissions = a} :: DescribeConnectionAliasPermissionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConnectionAliasPermissionsResponse_httpStatus :: Lens.Lens' DescribeConnectionAliasPermissionsResponse Core.Int
describeConnectionAliasPermissionsResponse_httpStatus = Lens.lens (\DescribeConnectionAliasPermissionsResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionAliasPermissionsResponse' {} a -> s {httpStatus = a} :: DescribeConnectionAliasPermissionsResponse)

instance
  Core.NFData
    DescribeConnectionAliasPermissionsResponse
