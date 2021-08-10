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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExportImageTasks' smart constructor.
data DescribeExportImageTasks = DescribeExportImageTasks'
  { -- | A token that indicates the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The IDs of the export image tasks.
    exportImageTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | Filter tasks using the @task-state@ filter and one of the following
    -- values: @active@, @completed@, @deleting@, or @deleted@.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      exportImageTaskIds = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | A token that indicates the next page of results.
describeExportImageTasks_nextToken :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Text)
describeExportImageTasks_nextToken = Lens.lens (\DescribeExportImageTasks' {nextToken} -> nextToken) (\s@DescribeExportImageTasks' {} a -> s {nextToken = a} :: DescribeExportImageTasks)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeExportImageTasks_dryRun :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Bool)
describeExportImageTasks_dryRun = Lens.lens (\DescribeExportImageTasks' {dryRun} -> dryRun) (\s@DescribeExportImageTasks' {} a -> s {dryRun = a} :: DescribeExportImageTasks)

-- | The maximum number of results to return in a single call.
describeExportImageTasks_maxResults :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Natural)
describeExportImageTasks_maxResults = Lens.lens (\DescribeExportImageTasks' {maxResults} -> maxResults) (\s@DescribeExportImageTasks' {} a -> s {maxResults = a} :: DescribeExportImageTasks)

-- | The IDs of the export image tasks.
describeExportImageTasks_exportImageTaskIds :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe [Prelude.Text])
describeExportImageTasks_exportImageTaskIds = Lens.lens (\DescribeExportImageTasks' {exportImageTaskIds} -> exportImageTaskIds) (\s@DescribeExportImageTasks' {} a -> s {exportImageTaskIds = a} :: DescribeExportImageTasks) Prelude.. Lens.mapping Lens._Coerce

-- | Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
describeExportImageTasks_filters :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe [Filter])
describeExportImageTasks_filters = Lens.lens (\DescribeExportImageTasks' {filters} -> filters) (\s@DescribeExportImageTasks' {} a -> s {filters = a} :: DescribeExportImageTasks) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeExportImageTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeExportImageTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeExportImageTasksResponse_exportImageTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeExportImageTasks_nextToken
          Lens..~ rs
          Lens.^? describeExportImageTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeExportImageTasks where
  type
    AWSResponse DescribeExportImageTasks =
      DescribeExportImageTasksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportImageTasksResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "exportImageTaskSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportImageTasks

instance Prelude.NFData DescribeExportImageTasks

instance Core.ToHeaders DescribeExportImageTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeExportImageTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExportImageTasks where
  toQuery DescribeExportImageTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeExportImageTasks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "ExportImageTaskId"
              Prelude.<$> exportImageTaskIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeExportImageTasksResponse' smart constructor.
data DescribeExportImageTasksResponse = DescribeExportImageTasksResponse'
  { -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the export image tasks.
    exportImageTasks :: Prelude.Maybe [ExportImageTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeExportImageTasksResponse
newDescribeExportImageTasksResponse pHttpStatus_ =
  DescribeExportImageTasksResponse'
    { nextToken =
        Prelude.Nothing,
      exportImageTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeExportImageTasksResponse_nextToken :: Lens.Lens' DescribeExportImageTasksResponse (Prelude.Maybe Prelude.Text)
describeExportImageTasksResponse_nextToken = Lens.lens (\DescribeExportImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportImageTasksResponse' {} a -> s {nextToken = a} :: DescribeExportImageTasksResponse)

-- | Information about the export image tasks.
describeExportImageTasksResponse_exportImageTasks :: Lens.Lens' DescribeExportImageTasksResponse (Prelude.Maybe [ExportImageTask])
describeExportImageTasksResponse_exportImageTasks = Lens.lens (\DescribeExportImageTasksResponse' {exportImageTasks} -> exportImageTasks) (\s@DescribeExportImageTasksResponse' {} a -> s {exportImageTasks = a} :: DescribeExportImageTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeExportImageTasksResponse_httpStatus :: Lens.Lens' DescribeExportImageTasksResponse Prelude.Int
describeExportImageTasksResponse_httpStatus = Lens.lens (\DescribeExportImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportImageTasksResponse)

instance
  Prelude.NFData
    DescribeExportImageTasksResponse
