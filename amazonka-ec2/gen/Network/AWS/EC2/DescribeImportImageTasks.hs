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
-- Module      : Network.AWS.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot
-- tasks that are already created.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeImportImageTasks
  ( -- * Creating a Request
    DescribeImportImageTasks (..),
    newDescribeImportImageTasks,

    -- * Request Lenses
    describeImportImageTasks_nextToken,
    describeImportImageTasks_dryRun,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_maxResults,
    describeImportImageTasks_filters,

    -- * Destructuring the Response
    DescribeImportImageTasksResponse (..),
    newDescribeImportImageTasksResponse,

    -- * Response Lenses
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
  { -- | A token that indicates the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The IDs of the import image tasks.
    importTaskIds :: Core.Maybe [Core.Text],
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Int,
    -- | Filter tasks using the @task-state@ filter and one of the following
    -- values: @active@, @completed@, @deleting@, or @deleted@.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportImageTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImportImageTasks_nextToken' - A token that indicates the next page of results.
--
-- 'dryRun', 'describeImportImageTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'importTaskIds', 'describeImportImageTasks_importTaskIds' - The IDs of the import image tasks.
--
-- 'maxResults', 'describeImportImageTasks_maxResults' - The maximum number of results to return in a single call.
--
-- 'filters', 'describeImportImageTasks_filters' - Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
newDescribeImportImageTasks ::
  DescribeImportImageTasks
newDescribeImportImageTasks =
  DescribeImportImageTasks'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      importTaskIds = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token that indicates the next page of results.
describeImportImageTasks_nextToken :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Text)
describeImportImageTasks_nextToken = Lens.lens (\DescribeImportImageTasks' {nextToken} -> nextToken) (\s@DescribeImportImageTasks' {} a -> s {nextToken = a} :: DescribeImportImageTasks)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImportImageTasks_dryRun :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Bool)
describeImportImageTasks_dryRun = Lens.lens (\DescribeImportImageTasks' {dryRun} -> dryRun) (\s@DescribeImportImageTasks' {} a -> s {dryRun = a} :: DescribeImportImageTasks)

-- | The IDs of the import image tasks.
describeImportImageTasks_importTaskIds :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Core.Text])
describeImportImageTasks_importTaskIds = Lens.lens (\DescribeImportImageTasks' {importTaskIds} -> importTaskIds) (\s@DescribeImportImageTasks' {} a -> s {importTaskIds = a} :: DescribeImportImageTasks) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return in a single call.
describeImportImageTasks_maxResults :: Lens.Lens' DescribeImportImageTasks (Core.Maybe Core.Int)
describeImportImageTasks_maxResults = Lens.lens (\DescribeImportImageTasks' {maxResults} -> maxResults) (\s@DescribeImportImageTasks' {} a -> s {maxResults = a} :: DescribeImportImageTasks)

-- | Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
describeImportImageTasks_filters :: Lens.Lens' DescribeImportImageTasks (Core.Maybe [Filter])
describeImportImageTasks_filters = Lens.lens (\DescribeImportImageTasks' {filters} -> filters) (\s@DescribeImportImageTasks' {} a -> s {filters = a} :: DescribeImportImageTasks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeImportImageTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImportImageTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImportImageTasksResponse_importImageTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeImportImageTasks_nextToken
          Lens..~ rs
          Lens.^? describeImportImageTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeImportImageTasks where
  type
    AWSResponse DescribeImportImageTasks =
      DescribeImportImageTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImportImageTasksResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "importImageTaskSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImportImageTasks

instance Core.NFData DescribeImportImageTasks

instance Core.ToHeaders DescribeImportImageTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeImportImageTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImportImageTasks where
  toQuery DescribeImportImageTasks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeImportImageTasks" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "ImportTaskId"
              Core.<$> importTaskIds
          ),
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filters" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
  { -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of zero or more import image tasks that are currently active or
    -- were completed or canceled in the previous 7 days.
    importImageTasks :: Core.Maybe [ImportImageTask],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportImageTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImportImageTasksResponse_nextToken' - The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'importImageTasks', 'describeImportImageTasksResponse_importImageTasks' - A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
--
-- 'httpStatus', 'describeImportImageTasksResponse_httpStatus' - The response's http status code.
newDescribeImportImageTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImportImageTasksResponse
newDescribeImportImageTasksResponse pHttpStatus_ =
  DescribeImportImageTasksResponse'
    { nextToken =
        Core.Nothing,
      importImageTasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeImportImageTasksResponse_nextToken :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe Core.Text)
describeImportImageTasksResponse_nextToken = Lens.lens (\DescribeImportImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeImportImageTasksResponse' {} a -> s {nextToken = a} :: DescribeImportImageTasksResponse)

-- | A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
describeImportImageTasksResponse_importImageTasks :: Lens.Lens' DescribeImportImageTasksResponse (Core.Maybe [ImportImageTask])
describeImportImageTasksResponse_importImageTasks = Lens.lens (\DescribeImportImageTasksResponse' {importImageTasks} -> importImageTasks) (\s@DescribeImportImageTasksResponse' {} a -> s {importImageTasks = a} :: DescribeImportImageTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeImportImageTasksResponse_httpStatus :: Lens.Lens' DescribeImportImageTasksResponse Core.Int
describeImportImageTasksResponse_httpStatus = Lens.lens (\DescribeImportImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeImportImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeImportImageTasksResponse)

instance Core.NFData DescribeImportImageTasksResponse
