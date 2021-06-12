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
-- Module      : Network.AWS.EC2.DescribeExportImageTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export image tasks or all of your export image
-- tasks.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeExportImageTasks
  ( -- * Creating a Request
    DescribeExportImageTasks (..),
    newDescribeExportImageTasks,

    -- * Request Lenses
    describeExportImageTasks_nextToken,
    describeExportImageTasks_dryRun,
    describeExportImageTasks_maxResults,
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_filters,

    -- * Destructuring the Response
    DescribeExportImageTasksResponse (..),
    newDescribeExportImageTasksResponse,

    -- * Response Lenses
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportImageTasks' smart constructor.
data DescribeExportImageTasks = DescribeExportImageTasks'
  { -- | A token that indicates the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The IDs of the export image tasks.
    exportImageTaskIds :: Core.Maybe [Core.Text],
    -- | Filter tasks using the @task-state@ filter and one of the following
    -- values: @active@, @completed@, @deleting@, or @deleted@.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeExportImageTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeExportImageTasks_nextToken' - A token that indicates the next page of results.
--
-- 'dryRun', 'describeExportImageTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeExportImageTasks_maxResults' - The maximum number of results to return in a single call.
--
-- 'exportImageTaskIds', 'describeExportImageTasks_exportImageTaskIds' - The IDs of the export image tasks.
--
-- 'filters', 'describeExportImageTasks_filters' - Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
newDescribeExportImageTasks ::
  DescribeExportImageTasks
newDescribeExportImageTasks =
  DescribeExportImageTasks'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      exportImageTaskIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token that indicates the next page of results.
describeExportImageTasks_nextToken :: Lens.Lens' DescribeExportImageTasks (Core.Maybe Core.Text)
describeExportImageTasks_nextToken = Lens.lens (\DescribeExportImageTasks' {nextToken} -> nextToken) (\s@DescribeExportImageTasks' {} a -> s {nextToken = a} :: DescribeExportImageTasks)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeExportImageTasks_dryRun :: Lens.Lens' DescribeExportImageTasks (Core.Maybe Core.Bool)
describeExportImageTasks_dryRun = Lens.lens (\DescribeExportImageTasks' {dryRun} -> dryRun) (\s@DescribeExportImageTasks' {} a -> s {dryRun = a} :: DescribeExportImageTasks)

-- | The maximum number of results to return in a single call.
describeExportImageTasks_maxResults :: Lens.Lens' DescribeExportImageTasks (Core.Maybe Core.Natural)
describeExportImageTasks_maxResults = Lens.lens (\DescribeExportImageTasks' {maxResults} -> maxResults) (\s@DescribeExportImageTasks' {} a -> s {maxResults = a} :: DescribeExportImageTasks)

-- | The IDs of the export image tasks.
describeExportImageTasks_exportImageTaskIds :: Lens.Lens' DescribeExportImageTasks (Core.Maybe [Core.Text])
describeExportImageTasks_exportImageTaskIds = Lens.lens (\DescribeExportImageTasks' {exportImageTaskIds} -> exportImageTaskIds) (\s@DescribeExportImageTasks' {} a -> s {exportImageTaskIds = a} :: DescribeExportImageTasks) Core.. Lens.mapping Lens._Coerce

-- | Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
describeExportImageTasks_filters :: Lens.Lens' DescribeExportImageTasks (Core.Maybe [Filter])
describeExportImageTasks_filters = Lens.lens (\DescribeExportImageTasks' {filters} -> filters) (\s@DescribeExportImageTasks' {} a -> s {filters = a} :: DescribeExportImageTasks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeExportImageTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportImageTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeExportImageTasksResponse_exportImageTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeExportImageTasks_nextToken
          Lens..~ rs
          Lens.^? describeExportImageTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeExportImageTasks where
  type
    AWSResponse DescribeExportImageTasks =
      DescribeExportImageTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportImageTasksResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "exportImageTaskSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExportImageTasks

instance Core.NFData DescribeExportImageTasks

instance Core.ToHeaders DescribeExportImageTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeExportImageTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExportImageTasks where
  toQuery DescribeExportImageTasks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeExportImageTasks" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "ExportImageTaskId"
              Core.<$> exportImageTaskIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeExportImageTasksResponse' smart constructor.
data DescribeExportImageTasksResponse = DescribeExportImageTasksResponse'
  { -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the export image tasks.
    exportImageTasks :: Core.Maybe [ExportImageTask],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeExportImageTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeExportImageTasksResponse_nextToken' - The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'exportImageTasks', 'describeExportImageTasksResponse_exportImageTasks' - Information about the export image tasks.
--
-- 'httpStatus', 'describeExportImageTasksResponse_httpStatus' - The response's http status code.
newDescribeExportImageTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeExportImageTasksResponse
newDescribeExportImageTasksResponse pHttpStatus_ =
  DescribeExportImageTasksResponse'
    { nextToken =
        Core.Nothing,
      exportImageTasks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeExportImageTasksResponse_nextToken :: Lens.Lens' DescribeExportImageTasksResponse (Core.Maybe Core.Text)
describeExportImageTasksResponse_nextToken = Lens.lens (\DescribeExportImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportImageTasksResponse' {} a -> s {nextToken = a} :: DescribeExportImageTasksResponse)

-- | Information about the export image tasks.
describeExportImageTasksResponse_exportImageTasks :: Lens.Lens' DescribeExportImageTasksResponse (Core.Maybe [ExportImageTask])
describeExportImageTasksResponse_exportImageTasks = Lens.lens (\DescribeExportImageTasksResponse' {exportImageTasks} -> exportImageTasks) (\s@DescribeExportImageTasksResponse' {} a -> s {exportImageTasks = a} :: DescribeExportImageTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeExportImageTasksResponse_httpStatus :: Lens.Lens' DescribeExportImageTasksResponse Core.Int
describeExportImageTasksResponse_httpStatus = Lens.lens (\DescribeExportImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportImageTasksResponse)

instance Core.NFData DescribeExportImageTasksResponse
