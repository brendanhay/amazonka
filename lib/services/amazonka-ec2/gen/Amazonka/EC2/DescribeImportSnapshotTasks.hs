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
-- Module      : Amazonka.EC2.DescribeImportSnapshotTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeImportSnapshotTasks
  ( -- * Creating a Request
    DescribeImportSnapshotTasks (..),
    newDescribeImportSnapshotTasks,

    -- * Request Lenses
    describeImportSnapshotTasks_dryRun,
    describeImportSnapshotTasks_filters,
    describeImportSnapshotTasks_importTaskIds,
    describeImportSnapshotTasks_maxResults,
    describeImportSnapshotTasks_nextToken,

    -- * Destructuring the Response
    DescribeImportSnapshotTasksResponse (..),
    newDescribeImportSnapshotTasksResponse,

    -- * Response Lenses
    describeImportSnapshotTasksResponse_importSnapshotTasks,
    describeImportSnapshotTasksResponse_nextToken,
    describeImportSnapshotTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImportSnapshotTasks' smart constructor.
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    filters :: Prelude.Maybe [Filter],
    -- | A list of import snapshot task IDs.
    importTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token that indicates the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImportSnapshotTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeImportSnapshotTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeImportSnapshotTasks_filters' - The filters.
--
-- 'importTaskIds', 'describeImportSnapshotTasks_importTaskIds' - A list of import snapshot task IDs.
--
-- 'maxResults', 'describeImportSnapshotTasks_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'describeImportSnapshotTasks_nextToken' - A token that indicates the next page of results.
newDescribeImportSnapshotTasks ::
  DescribeImportSnapshotTasks
newDescribeImportSnapshotTasks =
  DescribeImportSnapshotTasks'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      importTaskIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImportSnapshotTasks_dryRun :: Lens.Lens' DescribeImportSnapshotTasks (Prelude.Maybe Prelude.Bool)
describeImportSnapshotTasks_dryRun = Lens.lens (\DescribeImportSnapshotTasks' {dryRun} -> dryRun) (\s@DescribeImportSnapshotTasks' {} a -> s {dryRun = a} :: DescribeImportSnapshotTasks)

-- | The filters.
describeImportSnapshotTasks_filters :: Lens.Lens' DescribeImportSnapshotTasks (Prelude.Maybe [Filter])
describeImportSnapshotTasks_filters = Lens.lens (\DescribeImportSnapshotTasks' {filters} -> filters) (\s@DescribeImportSnapshotTasks' {} a -> s {filters = a} :: DescribeImportSnapshotTasks) Prelude.. Lens.mapping Lens.coerced

-- | A list of import snapshot task IDs.
describeImportSnapshotTasks_importTaskIds :: Lens.Lens' DescribeImportSnapshotTasks (Prelude.Maybe [Prelude.Text])
describeImportSnapshotTasks_importTaskIds = Lens.lens (\DescribeImportSnapshotTasks' {importTaskIds} -> importTaskIds) (\s@DescribeImportSnapshotTasks' {} a -> s {importTaskIds = a} :: DescribeImportSnapshotTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
describeImportSnapshotTasks_maxResults :: Lens.Lens' DescribeImportSnapshotTasks (Prelude.Maybe Prelude.Int)
describeImportSnapshotTasks_maxResults = Lens.lens (\DescribeImportSnapshotTasks' {maxResults} -> maxResults) (\s@DescribeImportSnapshotTasks' {} a -> s {maxResults = a} :: DescribeImportSnapshotTasks)

-- | A token that indicates the next page of results.
describeImportSnapshotTasks_nextToken :: Lens.Lens' DescribeImportSnapshotTasks (Prelude.Maybe Prelude.Text)
describeImportSnapshotTasks_nextToken = Lens.lens (\DescribeImportSnapshotTasks' {nextToken} -> nextToken) (\s@DescribeImportSnapshotTasks' {} a -> s {nextToken = a} :: DescribeImportSnapshotTasks)

instance Core.AWSPager DescribeImportSnapshotTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImportSnapshotTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImportSnapshotTasksResponse_importSnapshotTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImportSnapshotTasks_nextToken
          Lens..~ rs
          Lens.^? describeImportSnapshotTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeImportSnapshotTasks where
  type
    AWSResponse DescribeImportSnapshotTasks =
      DescribeImportSnapshotTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImportSnapshotTasksResponse'
            Prelude.<$> ( x Data..@? "importSnapshotTaskSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImportSnapshotTasks where
  hashWithSalt _salt DescribeImportSnapshotTasks' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` importTaskIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeImportSnapshotTasks where
  rnf DescribeImportSnapshotTasks' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf importTaskIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeImportSnapshotTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeImportSnapshotTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImportSnapshotTasks where
  toQuery DescribeImportSnapshotTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeImportSnapshotTasks" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filters" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "ImportTaskId"
              Prelude.<$> importTaskIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeImportSnapshotTasksResponse' smart constructor.
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
  { -- | A list of zero or more import snapshot tasks that are currently active
    -- or were completed or canceled in the previous 7 days.
    importSnapshotTasks :: Prelude.Maybe [ImportSnapshotTask],
    -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeImportSnapshotTasksResponse
newDescribeImportSnapshotTasksResponse pHttpStatus_ =
  DescribeImportSnapshotTasksResponse'
    { importSnapshotTasks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
describeImportSnapshotTasksResponse_importSnapshotTasks :: Lens.Lens' DescribeImportSnapshotTasksResponse (Prelude.Maybe [ImportSnapshotTask])
describeImportSnapshotTasksResponse_importSnapshotTasks = Lens.lens (\DescribeImportSnapshotTasksResponse' {importSnapshotTasks} -> importSnapshotTasks) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {importSnapshotTasks = a} :: DescribeImportSnapshotTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeImportSnapshotTasksResponse_nextToken :: Lens.Lens' DescribeImportSnapshotTasksResponse (Prelude.Maybe Prelude.Text)
describeImportSnapshotTasksResponse_nextToken = Lens.lens (\DescribeImportSnapshotTasksResponse' {nextToken} -> nextToken) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {nextToken = a} :: DescribeImportSnapshotTasksResponse)

-- | The response's http status code.
describeImportSnapshotTasksResponse_httpStatus :: Lens.Lens' DescribeImportSnapshotTasksResponse Prelude.Int
describeImportSnapshotTasksResponse_httpStatus = Lens.lens (\DescribeImportSnapshotTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeImportSnapshotTasksResponse' {} a -> s {httpStatus = a} :: DescribeImportSnapshotTasksResponse)

instance
  Prelude.NFData
    DescribeImportSnapshotTasksResponse
  where
  rnf DescribeImportSnapshotTasksResponse' {..} =
    Prelude.rnf importSnapshotTasks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
