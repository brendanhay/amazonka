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
-- Module      : Amazonka.EC2.DescribeStoreImageTasks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the progress of the AMI store tasks. You can describe the
-- store tasks for specified AMIs. If you don\'t specify the AMIs, you get
-- a paginated list of store tasks from the last 31 days.
--
-- For each AMI task, the response indicates if the task is @InProgress@,
-- @Completed@, or @Failed@. For tasks @InProgress@, the response shows the
-- estimated progress as a percentage.
--
-- Tasks are listed in reverse chronological order. Currently, only tasks
-- from the past 31 days can be viewed.
--
-- To use this API, you must have the required permissions. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html#ami-s3-permissions Permissions for storing and restoring AMIs using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-store-restore.html Store and restore an AMI using Amazon S3>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeStoreImageTasks
  ( -- * Creating a Request
    DescribeStoreImageTasks (..),
    newDescribeStoreImageTasks,

    -- * Request Lenses
    describeStoreImageTasks_nextToken,
    describeStoreImageTasks_imageIds,
    describeStoreImageTasks_filters,
    describeStoreImageTasks_dryRun,
    describeStoreImageTasks_maxResults,

    -- * Destructuring the Response
    DescribeStoreImageTasksResponse (..),
    newDescribeStoreImageTasksResponse,

    -- * Response Lenses
    describeStoreImageTasksResponse_nextToken,
    describeStoreImageTasksResponse_storeImageTaskResults,
    describeStoreImageTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStoreImageTasks' smart constructor.
data DescribeStoreImageTasks = DescribeStoreImageTasks'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The AMI IDs for which to show progress. Up to 20 AMI IDs can be included
    -- in a request.
    imageIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @task-state@ - Returns tasks in a certain state (@InProgress@ |
    --     @Completed@ | @Failed@)
    --
    -- -   @bucket@ - Returns task information for tasks that targeted a
    --     specific bucket. For the filter value, specify the bucket name.
    filters :: Prelude.Maybe [Filter],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 1 and 200. You cannot specify this
    -- parameter and the @ImageIDs@ parameter in the same call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStoreImageTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStoreImageTasks_nextToken' - The token for the next page of results.
--
-- 'imageIds', 'describeStoreImageTasks_imageIds' - The AMI IDs for which to show progress. Up to 20 AMI IDs can be included
-- in a request.
--
-- 'filters', 'describeStoreImageTasks_filters' - The filters.
--
-- -   @task-state@ - Returns tasks in a certain state (@InProgress@ |
--     @Completed@ | @Failed@)
--
-- -   @bucket@ - Returns task information for tasks that targeted a
--     specific bucket. For the filter value, specify the bucket name.
--
-- 'dryRun', 'describeStoreImageTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeStoreImageTasks_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200. You cannot specify this
-- parameter and the @ImageIDs@ parameter in the same call.
newDescribeStoreImageTasks ::
  DescribeStoreImageTasks
newDescribeStoreImageTasks =
  DescribeStoreImageTasks'
    { nextToken =
        Prelude.Nothing,
      imageIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeStoreImageTasks_nextToken :: Lens.Lens' DescribeStoreImageTasks (Prelude.Maybe Prelude.Text)
describeStoreImageTasks_nextToken = Lens.lens (\DescribeStoreImageTasks' {nextToken} -> nextToken) (\s@DescribeStoreImageTasks' {} a -> s {nextToken = a} :: DescribeStoreImageTasks)

-- | The AMI IDs for which to show progress. Up to 20 AMI IDs can be included
-- in a request.
describeStoreImageTasks_imageIds :: Lens.Lens' DescribeStoreImageTasks (Prelude.Maybe [Prelude.Text])
describeStoreImageTasks_imageIds = Lens.lens (\DescribeStoreImageTasks' {imageIds} -> imageIds) (\s@DescribeStoreImageTasks' {} a -> s {imageIds = a} :: DescribeStoreImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
--
-- -   @task-state@ - Returns tasks in a certain state (@InProgress@ |
--     @Completed@ | @Failed@)
--
-- -   @bucket@ - Returns task information for tasks that targeted a
--     specific bucket. For the filter value, specify the bucket name.
describeStoreImageTasks_filters :: Lens.Lens' DescribeStoreImageTasks (Prelude.Maybe [Filter])
describeStoreImageTasks_filters = Lens.lens (\DescribeStoreImageTasks' {filters} -> filters) (\s@DescribeStoreImageTasks' {} a -> s {filters = a} :: DescribeStoreImageTasks) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeStoreImageTasks_dryRun :: Lens.Lens' DescribeStoreImageTasks (Prelude.Maybe Prelude.Bool)
describeStoreImageTasks_dryRun = Lens.lens (\DescribeStoreImageTasks' {dryRun} -> dryRun) (\s@DescribeStoreImageTasks' {} a -> s {dryRun = a} :: DescribeStoreImageTasks)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 1 and 200. You cannot specify this
-- parameter and the @ImageIDs@ parameter in the same call.
describeStoreImageTasks_maxResults :: Lens.Lens' DescribeStoreImageTasks (Prelude.Maybe Prelude.Natural)
describeStoreImageTasks_maxResults = Lens.lens (\DescribeStoreImageTasks' {maxResults} -> maxResults) (\s@DescribeStoreImageTasks' {} a -> s {maxResults = a} :: DescribeStoreImageTasks)

instance Core.AWSPager DescribeStoreImageTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStoreImageTasksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStoreImageTasksResponse_storeImageTaskResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStoreImageTasks_nextToken
          Lens..~ rs
          Lens.^? describeStoreImageTasksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeStoreImageTasks where
  type
    AWSResponse DescribeStoreImageTasks =
      DescribeStoreImageTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeStoreImageTasksResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "storeImageTaskResultSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStoreImageTasks where
  hashWithSalt _salt DescribeStoreImageTasks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeStoreImageTasks where
  rnf DescribeStoreImageTasks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeStoreImageTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStoreImageTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStoreImageTasks where
  toQuery DescribeStoreImageTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStoreImageTasks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          (Core.toQueryList "ImageId" Prelude.<$> imageIds),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeStoreImageTasksResponse' smart constructor.
data DescribeStoreImageTasksResponse = DescribeStoreImageTasksResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The information about the AMI store tasks.
    storeImageTaskResults :: Prelude.Maybe [StoreImageTaskResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStoreImageTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStoreImageTasksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'storeImageTaskResults', 'describeStoreImageTasksResponse_storeImageTaskResults' - The information about the AMI store tasks.
--
-- 'httpStatus', 'describeStoreImageTasksResponse_httpStatus' - The response's http status code.
newDescribeStoreImageTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStoreImageTasksResponse
newDescribeStoreImageTasksResponse pHttpStatus_ =
  DescribeStoreImageTasksResponse'
    { nextToken =
        Prelude.Nothing,
      storeImageTaskResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeStoreImageTasksResponse_nextToken :: Lens.Lens' DescribeStoreImageTasksResponse (Prelude.Maybe Prelude.Text)
describeStoreImageTasksResponse_nextToken = Lens.lens (\DescribeStoreImageTasksResponse' {nextToken} -> nextToken) (\s@DescribeStoreImageTasksResponse' {} a -> s {nextToken = a} :: DescribeStoreImageTasksResponse)

-- | The information about the AMI store tasks.
describeStoreImageTasksResponse_storeImageTaskResults :: Lens.Lens' DescribeStoreImageTasksResponse (Prelude.Maybe [StoreImageTaskResult])
describeStoreImageTasksResponse_storeImageTaskResults = Lens.lens (\DescribeStoreImageTasksResponse' {storeImageTaskResults} -> storeImageTaskResults) (\s@DescribeStoreImageTasksResponse' {} a -> s {storeImageTaskResults = a} :: DescribeStoreImageTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStoreImageTasksResponse_httpStatus :: Lens.Lens' DescribeStoreImageTasksResponse Prelude.Int
describeStoreImageTasksResponse_httpStatus = Lens.lens (\DescribeStoreImageTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeStoreImageTasksResponse' {} a -> s {httpStatus = a} :: DescribeStoreImageTasksResponse)

instance
  Prelude.NFData
    DescribeStoreImageTasksResponse
  where
  rnf DescribeStoreImageTasksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storeImageTaskResults
      `Prelude.seq` Prelude.rnf httpStatus
