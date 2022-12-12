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
-- Module      : Amazonka.EC2.DescribeExportImageTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export image tasks or all of your export image
-- tasks.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeExportImageTasks
  ( -- * Creating a Request
    DescribeExportImageTasks (..),
    newDescribeExportImageTasks,

    -- * Request Lenses
    describeExportImageTasks_dryRun,
    describeExportImageTasks_exportImageTaskIds,
    describeExportImageTasks_filters,
    describeExportImageTasks_maxResults,
    describeExportImageTasks_nextToken,

    -- * Destructuring the Response
    DescribeExportImageTasksResponse (..),
    newDescribeExportImageTasksResponse,

    -- * Response Lenses
    describeExportImageTasksResponse_exportImageTasks,
    describeExportImageTasksResponse_nextToken,
    describeExportImageTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExportImageTasks' smart constructor.
data DescribeExportImageTasks = DescribeExportImageTasks'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the export image tasks.
    exportImageTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | Filter tasks using the @task-state@ filter and one of the following
    -- values: @active@, @completed@, @deleting@, or @deleted@.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'dryRun', 'describeExportImageTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'exportImageTaskIds', 'describeExportImageTasks_exportImageTaskIds' - The IDs of the export image tasks.
--
-- 'filters', 'describeExportImageTasks_filters' - Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
--
-- 'maxResults', 'describeExportImageTasks_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'describeExportImageTasks_nextToken' - A token that indicates the next page of results.
newDescribeExportImageTasks ::
  DescribeExportImageTasks
newDescribeExportImageTasks =
  DescribeExportImageTasks'
    { dryRun = Prelude.Nothing,
      exportImageTaskIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeExportImageTasks_dryRun :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Bool)
describeExportImageTasks_dryRun = Lens.lens (\DescribeExportImageTasks' {dryRun} -> dryRun) (\s@DescribeExportImageTasks' {} a -> s {dryRun = a} :: DescribeExportImageTasks)

-- | The IDs of the export image tasks.
describeExportImageTasks_exportImageTaskIds :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe [Prelude.Text])
describeExportImageTasks_exportImageTaskIds = Lens.lens (\DescribeExportImageTasks' {exportImageTaskIds} -> exportImageTaskIds) (\s@DescribeExportImageTasks' {} a -> s {exportImageTaskIds = a} :: DescribeExportImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
describeExportImageTasks_filters :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe [Filter])
describeExportImageTasks_filters = Lens.lens (\DescribeExportImageTasks' {filters} -> filters) (\s@DescribeExportImageTasks' {} a -> s {filters = a} :: DescribeExportImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call.
describeExportImageTasks_maxResults :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Natural)
describeExportImageTasks_maxResults = Lens.lens (\DescribeExportImageTasks' {maxResults} -> maxResults) (\s@DescribeExportImageTasks' {} a -> s {maxResults = a} :: DescribeExportImageTasks)

-- | A token that indicates the next page of results.
describeExportImageTasks_nextToken :: Lens.Lens' DescribeExportImageTasks (Prelude.Maybe Prelude.Text)
describeExportImageTasks_nextToken = Lens.lens (\DescribeExportImageTasks' {nextToken} -> nextToken) (\s@DescribeExportImageTasks' {} a -> s {nextToken = a} :: DescribeExportImageTasks)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeExportImageTasksResponse'
            Prelude.<$> ( x Data..@? "exportImageTaskSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExportImageTasks where
  hashWithSalt _salt DescribeExportImageTasks' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` exportImageTaskIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeExportImageTasks where
  rnf DescribeExportImageTasks' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf exportImageTaskIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeExportImageTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeExportImageTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeExportImageTasks where
  toQuery DescribeExportImageTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeExportImageTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "ExportImageTaskId"
              Prelude.<$> exportImageTaskIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeExportImageTasksResponse' smart constructor.
data DescribeExportImageTasksResponse = DescribeExportImageTasksResponse'
  { -- | Information about the export image tasks.
    exportImageTasks :: Prelude.Maybe [ExportImageTask],
    -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'exportImageTasks', 'describeExportImageTasksResponse_exportImageTasks' - Information about the export image tasks.
--
-- 'nextToken', 'describeExportImageTasksResponse_nextToken' - The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'httpStatus', 'describeExportImageTasksResponse_httpStatus' - The response's http status code.
newDescribeExportImageTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportImageTasksResponse
newDescribeExportImageTasksResponse pHttpStatus_ =
  DescribeExportImageTasksResponse'
    { exportImageTasks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the export image tasks.
describeExportImageTasksResponse_exportImageTasks :: Lens.Lens' DescribeExportImageTasksResponse (Prelude.Maybe [ExportImageTask])
describeExportImageTasksResponse_exportImageTasks = Lens.lens (\DescribeExportImageTasksResponse' {exportImageTasks} -> exportImageTasks) (\s@DescribeExportImageTasksResponse' {} a -> s {exportImageTasks = a} :: DescribeExportImageTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeExportImageTasksResponse_nextToken :: Lens.Lens' DescribeExportImageTasksResponse (Prelude.Maybe Prelude.Text)
describeExportImageTasksResponse_nextToken = Lens.lens (\DescribeExportImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeExportImageTasksResponse' {} a -> s {nextToken = a} :: DescribeExportImageTasksResponse)

-- | The response's http status code.
describeExportImageTasksResponse_httpStatus :: Lens.Lens' DescribeExportImageTasksResponse Prelude.Int
describeExportImageTasksResponse_httpStatus = Lens.lens (\DescribeExportImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeExportImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeExportImageTasksResponse)

instance
  Prelude.NFData
    DescribeExportImageTasksResponse
  where
  rnf DescribeExportImageTasksResponse' {..} =
    Prelude.rnf exportImageTasks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
