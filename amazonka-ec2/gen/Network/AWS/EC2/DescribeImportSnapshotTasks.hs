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
-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeImportSnapshotTasks
  ( -- * Creating a Request
    DescribeImportSnapshotTasks (..),
    newDescribeImportSnapshotTasks,

    -- * Request Lenses
    describeImportSnapshotTasks_nextToken,
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasks_filters,

    -- * Destructuring the Response
    DescribeImportSnapshotTasksResponse (..),
    newDescribeImportSnapshotTasksResponse,

    -- * Response Lenses
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImportSnapshotTasks' smart constructor.
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
  { -- | A token that indicates the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | A list of import snapshot task IDs.
    importTaskIds :: Core.Maybe [Core.Text],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Core.Maybe Core.Int,
    -- | The filters.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportSnapshotTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImportSnapshotTasks_nextToken' - A token that indicates the next page of results.
--
-- 'dryRun', 'describeImportSnapshotTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'importTaskIds', 'describeImportSnapshotTasks_importTaskIds' - A list of import snapshot task IDs.
--
-- 'maxResults', 'describeImportSnapshotTasks_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'filters', 'describeImportSnapshotTasks_filters' - The filters.
newDescribeImportSnapshotTasks ::
  DescribeImportSnapshotTasks
newDescribeImportSnapshotTasks =
  DescribeImportSnapshotTasks'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      importTaskIds = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token that indicates the next page of results.
describeImportSnapshotTasks_nextToken :: Lens.Lens' DescribeImportSnapshotTasks (Core.Maybe Core.Text)
describeImportSnapshotTasks_nextToken = Lens.lens (\DescribeImportSnapshotTasks' {nextToken} -> nextToken) (\s@DescribeImportSnapshotTasks' {} a -> s {nextToken = a} :: DescribeImportSnapshotTasks)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImportSnapshotTasks_dryRun :: Lens.Lens' DescribeImportSnapshotTasks (Core.Maybe Core.Bool)
describeImportSnapshotTasks_dryRun = Lens.lens (\DescribeImportSnapshotTasks' {dryRun} -> dryRun) (\s@DescribeImportSnapshotTasks' {} a -> s {dryRun = a} :: DescribeImportSnapshotTasks)

-- | A list of import snapshot task IDs.
describeImportSnapshotTasks_importTaskIds :: Lens.Lens' DescribeImportSnapshotTasks (Core.Maybe [Core.Text])
describeImportSnapshotTasks_importTaskIds = Lens.lens (\DescribeImportSnapshotTasks' {importTaskIds} -> importTaskIds) (\s@DescribeImportSnapshotTasks' {} a -> s {importTaskIds = a} :: DescribeImportSnapshotTasks) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
describeImportSnapshotTasks_maxResults :: Lens.Lens' DescribeImportSnapshotTasks (Core.Maybe Core.Int)
describeImportSnapshotTasks_maxResults = Lens.lens (\DescribeImportSnapshotTasks' {maxResults} -> maxResults) (\s@DescribeImportSnapshotTasks' {} a -> s {maxResults = a} :: DescribeImportSnapshotTasks)

-- | The filters.
describeImportSnapshotTasks_filters :: Lens.Lens' DescribeImportSnapshotTasks (Core.Maybe [Filter])
describeImportSnapshotTasks_filters = Lens.lens (\DescribeImportSnapshotTasks' {filters} -> filters) (\s@DescribeImportSnapshotTasks' {} a -> s {filters = a} :: DescribeImportSnapshotTasks) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeImportSnapshotTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImportSnapshotTasksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImportSnapshotTasksResponse_importSnapshotTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeImportSnapshotTasks_nextToken
          Lens..~ rs
          Lens.^? describeImportSnapshotTasksResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeImportSnapshotTasks where
  type
    AWSResponse DescribeImportSnapshotTasks =
      DescribeImportSnapshotTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImportSnapshotTasksResponse'
            Core.<$> ( x Core..@? "importSnapshotTaskSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImportSnapshotTasks

instance Core.NFData DescribeImportSnapshotTasks

instance Core.ToHeaders DescribeImportSnapshotTasks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeImportSnapshotTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImportSnapshotTasks where
  toQuery DescribeImportSnapshotTasks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeImportSnapshotTasks" :: Core.ByteString),
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

-- | /See:/ 'newDescribeImportSnapshotTasksResponse' smart constructor.
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
  { -- | A list of zero or more import snapshot tasks that are currently active
    -- or were completed or canceled in the previous 7 days.
    importSnapshotTasks :: Core.Maybe [ImportSnapshotTask],
    -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImportSnapshotTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importSnapshotTasks', 'describeImportSnapshotTasksResponse_importSnapshotTasks' - A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
--
-- 'nextToken', 'describeImportSnapshotTasksResponse_nextToken' - The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'httpStatus', 'describeImportSnapshotTasksResponse_httpStatus' - The response's http status code.
newDescribeImportSnapshotTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImportSnapshotTasksResponse
newDescribeImportSnapshotTasksResponse pHttpStatus_ =
  DescribeImportSnapshotTasksResponse'
    { importSnapshotTasks =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
describeImportSnapshotTasksResponse_importSnapshotTasks :: Lens.Lens' DescribeImportSnapshotTasksResponse (Core.Maybe [ImportSnapshotTask])
describeImportSnapshotTasksResponse_importSnapshotTasks = Lens.lens (\DescribeImportSnapshotTasksResponse' {importSnapshotTasks} -> importSnapshotTasks) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {importSnapshotTasks = a} :: DescribeImportSnapshotTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeImportSnapshotTasksResponse_nextToken :: Lens.Lens' DescribeImportSnapshotTasksResponse (Core.Maybe Core.Text)
describeImportSnapshotTasksResponse_nextToken = Lens.lens (\DescribeImportSnapshotTasksResponse' {nextToken} -> nextToken) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {nextToken = a} :: DescribeImportSnapshotTasksResponse)

-- | The response's http status code.
describeImportSnapshotTasksResponse_httpStatus :: Lens.Lens' DescribeImportSnapshotTasksResponse Core.Int
describeImportSnapshotTasksResponse_httpStatus = Lens.lens (\DescribeImportSnapshotTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {httpStatus = a} :: DescribeImportSnapshotTasksResponse)

instance
  Core.NFData
    DescribeImportSnapshotTasksResponse
