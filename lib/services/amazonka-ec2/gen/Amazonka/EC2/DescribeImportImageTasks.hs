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
-- Module      : Amazonka.EC2.DescribeImportImageTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an import virtual machine or import snapshot
-- tasks that are already created.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeImportImageTasks
  ( -- * Creating a Request
    DescribeImportImageTasks (..),
    newDescribeImportImageTasks,

    -- * Request Lenses
    describeImportImageTasks_nextToken,
    describeImportImageTasks_importTaskIds,
    describeImportImageTasks_filters,
    describeImportImageTasks_dryRun,
    describeImportImageTasks_maxResults,

    -- * Destructuring the Response
    DescribeImportImageTasksResponse (..),
    newDescribeImportImageTasksResponse,

    -- * Response Lenses
    describeImportImageTasksResponse_nextToken,
    describeImportImageTasksResponse_importImageTasks,
    describeImportImageTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImportImageTasks' smart constructor.
data DescribeImportImageTasks = DescribeImportImageTasks'
  { -- | A token that indicates the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the import image tasks.
    importTaskIds :: Prelude.Maybe [Prelude.Text],
    -- | Filter tasks using the @task-state@ filter and one of the following
    -- values: @active@, @completed@, @deleting@, or @deleted@.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'importTaskIds', 'describeImportImageTasks_importTaskIds' - The IDs of the import image tasks.
--
-- 'filters', 'describeImportImageTasks_filters' - Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
--
-- 'dryRun', 'describeImportImageTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeImportImageTasks_maxResults' - The maximum number of results to return in a single call.
newDescribeImportImageTasks ::
  DescribeImportImageTasks
newDescribeImportImageTasks =
  DescribeImportImageTasks'
    { nextToken =
        Prelude.Nothing,
      importTaskIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token that indicates the next page of results.
describeImportImageTasks_nextToken :: Lens.Lens' DescribeImportImageTasks (Prelude.Maybe Prelude.Text)
describeImportImageTasks_nextToken = Lens.lens (\DescribeImportImageTasks' {nextToken} -> nextToken) (\s@DescribeImportImageTasks' {} a -> s {nextToken = a} :: DescribeImportImageTasks)

-- | The IDs of the import image tasks.
describeImportImageTasks_importTaskIds :: Lens.Lens' DescribeImportImageTasks (Prelude.Maybe [Prelude.Text])
describeImportImageTasks_importTaskIds = Lens.lens (\DescribeImportImageTasks' {importTaskIds} -> importTaskIds) (\s@DescribeImportImageTasks' {} a -> s {importTaskIds = a} :: DescribeImportImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | Filter tasks using the @task-state@ filter and one of the following
-- values: @active@, @completed@, @deleting@, or @deleted@.
describeImportImageTasks_filters :: Lens.Lens' DescribeImportImageTasks (Prelude.Maybe [Filter])
describeImportImageTasks_filters = Lens.lens (\DescribeImportImageTasks' {filters} -> filters) (\s@DescribeImportImageTasks' {} a -> s {filters = a} :: DescribeImportImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImportImageTasks_dryRun :: Lens.Lens' DescribeImportImageTasks (Prelude.Maybe Prelude.Bool)
describeImportImageTasks_dryRun = Lens.lens (\DescribeImportImageTasks' {dryRun} -> dryRun) (\s@DescribeImportImageTasks' {} a -> s {dryRun = a} :: DescribeImportImageTasks)

-- | The maximum number of results to return in a single call.
describeImportImageTasks_maxResults :: Lens.Lens' DescribeImportImageTasks (Prelude.Maybe Prelude.Int)
describeImportImageTasks_maxResults = Lens.lens (\DescribeImportImageTasks' {maxResults} -> maxResults) (\s@DescribeImportImageTasks' {} a -> s {maxResults = a} :: DescribeImportImageTasks)

instance Core.AWSPager DescribeImportImageTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImportImageTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImportImageTasksResponse_importImageTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImportImageTasks_nextToken
          Lens..~ rs
          Lens.^? describeImportImageTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeImportImageTasks where
  type
    AWSResponse DescribeImportImageTasks =
      DescribeImportImageTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImportImageTasksResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "importImageTaskSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImportImageTasks where
  hashWithSalt _salt DescribeImportImageTasks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` importTaskIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeImportImageTasks where
  rnf DescribeImportImageTasks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf importTaskIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeImportImageTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeImportImageTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImportImageTasks where
  toQuery DescribeImportImageTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeImportImageTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "ImportTaskId"
              Prelude.<$> importTaskIds
          ),
        Data.toQuery
          (Data.toQueryList "Filters" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newDescribeImportImageTasksResponse' smart constructor.
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'
  { -- | The token to use to get the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of zero or more import image tasks that are currently active or
    -- were completed or canceled in the previous 7 days.
    importImageTasks :: Prelude.Maybe [ImportImageTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeImportImageTasksResponse
newDescribeImportImageTasksResponse pHttpStatus_ =
  DescribeImportImageTasksResponse'
    { nextToken =
        Prelude.Nothing,
      importImageTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
describeImportImageTasksResponse_nextToken :: Lens.Lens' DescribeImportImageTasksResponse (Prelude.Maybe Prelude.Text)
describeImportImageTasksResponse_nextToken = Lens.lens (\DescribeImportImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeImportImageTasksResponse' {} a -> s {nextToken = a} :: DescribeImportImageTasksResponse)

-- | A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
describeImportImageTasksResponse_importImageTasks :: Lens.Lens' DescribeImportImageTasksResponse (Prelude.Maybe [ImportImageTask])
describeImportImageTasksResponse_importImageTasks = Lens.lens (\DescribeImportImageTasksResponse' {importImageTasks} -> importImageTasks) (\s@DescribeImportImageTasksResponse' {} a -> s {importImageTasks = a} :: DescribeImportImageTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImportImageTasksResponse_httpStatus :: Lens.Lens' DescribeImportImageTasksResponse Prelude.Int
describeImportImageTasksResponse_httpStatus = Lens.lens (\DescribeImportImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeImportImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeImportImageTasksResponse)

instance
  Prelude.NFData
    DescribeImportImageTasksResponse
  where
  rnf DescribeImportImageTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf importImageTasks
      `Prelude.seq` Prelude.rnf httpStatus
