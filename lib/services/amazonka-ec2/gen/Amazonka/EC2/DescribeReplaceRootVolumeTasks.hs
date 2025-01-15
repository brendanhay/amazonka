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
-- Module      : Amazonka.EC2.DescribeReplaceRootVolumeTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a root volume replacement task. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/replace-root.html Replace a root volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeReplaceRootVolumeTasks
  ( -- * Creating a Request
    DescribeReplaceRootVolumeTasks (..),
    newDescribeReplaceRootVolumeTasks,

    -- * Request Lenses
    describeReplaceRootVolumeTasks_dryRun,
    describeReplaceRootVolumeTasks_filters,
    describeReplaceRootVolumeTasks_maxResults,
    describeReplaceRootVolumeTasks_nextToken,
    describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds,

    -- * Destructuring the Response
    DescribeReplaceRootVolumeTasksResponse (..),
    newDescribeReplaceRootVolumeTasksResponse,

    -- * Response Lenses
    describeReplaceRootVolumeTasksResponse_nextToken,
    describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks,
    describeReplaceRootVolumeTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeReplaceRootVolumeTasks' smart constructor.
data DescribeReplaceRootVolumeTasks = DescribeReplaceRootVolumeTasks'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Filter to use:
    --
    -- -   @instance-id@ - The ID of the instance for which the root volume
    --     replacement task was created.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the root volume replacement task to view.
    replaceRootVolumeTaskIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplaceRootVolumeTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeReplaceRootVolumeTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeReplaceRootVolumeTasks_filters' - Filter to use:
--
-- -   @instance-id@ - The ID of the instance for which the root volume
--     replacement task was created.
--
-- 'maxResults', 'describeReplaceRootVolumeTasks_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeReplaceRootVolumeTasks_nextToken' - The token for the next page of results.
--
-- 'replaceRootVolumeTaskIds', 'describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds' - The ID of the root volume replacement task to view.
newDescribeReplaceRootVolumeTasks ::
  DescribeReplaceRootVolumeTasks
newDescribeReplaceRootVolumeTasks =
  DescribeReplaceRootVolumeTasks'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      replaceRootVolumeTaskIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeReplaceRootVolumeTasks_dryRun :: Lens.Lens' DescribeReplaceRootVolumeTasks (Prelude.Maybe Prelude.Bool)
describeReplaceRootVolumeTasks_dryRun = Lens.lens (\DescribeReplaceRootVolumeTasks' {dryRun} -> dryRun) (\s@DescribeReplaceRootVolumeTasks' {} a -> s {dryRun = a} :: DescribeReplaceRootVolumeTasks)

-- | Filter to use:
--
-- -   @instance-id@ - The ID of the instance for which the root volume
--     replacement task was created.
describeReplaceRootVolumeTasks_filters :: Lens.Lens' DescribeReplaceRootVolumeTasks (Prelude.Maybe [Filter])
describeReplaceRootVolumeTasks_filters = Lens.lens (\DescribeReplaceRootVolumeTasks' {filters} -> filters) (\s@DescribeReplaceRootVolumeTasks' {} a -> s {filters = a} :: DescribeReplaceRootVolumeTasks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeReplaceRootVolumeTasks_maxResults :: Lens.Lens' DescribeReplaceRootVolumeTasks (Prelude.Maybe Prelude.Natural)
describeReplaceRootVolumeTasks_maxResults = Lens.lens (\DescribeReplaceRootVolumeTasks' {maxResults} -> maxResults) (\s@DescribeReplaceRootVolumeTasks' {} a -> s {maxResults = a} :: DescribeReplaceRootVolumeTasks)

-- | The token for the next page of results.
describeReplaceRootVolumeTasks_nextToken :: Lens.Lens' DescribeReplaceRootVolumeTasks (Prelude.Maybe Prelude.Text)
describeReplaceRootVolumeTasks_nextToken = Lens.lens (\DescribeReplaceRootVolumeTasks' {nextToken} -> nextToken) (\s@DescribeReplaceRootVolumeTasks' {} a -> s {nextToken = a} :: DescribeReplaceRootVolumeTasks)

-- | The ID of the root volume replacement task to view.
describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds :: Lens.Lens' DescribeReplaceRootVolumeTasks (Prelude.Maybe [Prelude.Text])
describeReplaceRootVolumeTasks_replaceRootVolumeTaskIds = Lens.lens (\DescribeReplaceRootVolumeTasks' {replaceRootVolumeTaskIds} -> replaceRootVolumeTaskIds) (\s@DescribeReplaceRootVolumeTasks' {} a -> s {replaceRootVolumeTaskIds = a} :: DescribeReplaceRootVolumeTasks) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeReplaceRootVolumeTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplaceRootVolumeTasksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeReplaceRootVolumeTasks_nextToken
              Lens..~ rs
              Lens.^? describeReplaceRootVolumeTasksResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReplaceRootVolumeTasks
  where
  type
    AWSResponse DescribeReplaceRootVolumeTasks =
      DescribeReplaceRootVolumeTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeReplaceRootVolumeTasksResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "replaceRootVolumeTaskSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplaceRootVolumeTasks
  where
  hashWithSalt
    _salt
    DescribeReplaceRootVolumeTasks' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` replaceRootVolumeTaskIds

instance
  Prelude.NFData
    DescribeReplaceRootVolumeTasks
  where
  rnf DescribeReplaceRootVolumeTasks' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf replaceRootVolumeTaskIds

instance
  Data.ToHeaders
    DescribeReplaceRootVolumeTasks
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeReplaceRootVolumeTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeReplaceRootVolumeTasks where
  toQuery DescribeReplaceRootVolumeTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeReplaceRootVolumeTasks" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "ReplaceRootVolumeTaskId"
              Prelude.<$> replaceRootVolumeTaskIds
          )
      ]

-- | /See:/ 'newDescribeReplaceRootVolumeTasksResponse' smart constructor.
data DescribeReplaceRootVolumeTasksResponse = DescribeReplaceRootVolumeTasksResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the root volume replacement task.
    replaceRootVolumeTasks :: Prelude.Maybe [ReplaceRootVolumeTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplaceRootVolumeTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReplaceRootVolumeTasksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'replaceRootVolumeTasks', 'describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks' - Information about the root volume replacement task.
--
-- 'httpStatus', 'describeReplaceRootVolumeTasksResponse_httpStatus' - The response's http status code.
newDescribeReplaceRootVolumeTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplaceRootVolumeTasksResponse
newDescribeReplaceRootVolumeTasksResponse
  pHttpStatus_ =
    DescribeReplaceRootVolumeTasksResponse'
      { nextToken =
          Prelude.Nothing,
        replaceRootVolumeTasks =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeReplaceRootVolumeTasksResponse_nextToken :: Lens.Lens' DescribeReplaceRootVolumeTasksResponse (Prelude.Maybe Prelude.Text)
describeReplaceRootVolumeTasksResponse_nextToken = Lens.lens (\DescribeReplaceRootVolumeTasksResponse' {nextToken} -> nextToken) (\s@DescribeReplaceRootVolumeTasksResponse' {} a -> s {nextToken = a} :: DescribeReplaceRootVolumeTasksResponse)

-- | Information about the root volume replacement task.
describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks :: Lens.Lens' DescribeReplaceRootVolumeTasksResponse (Prelude.Maybe [ReplaceRootVolumeTask])
describeReplaceRootVolumeTasksResponse_replaceRootVolumeTasks = Lens.lens (\DescribeReplaceRootVolumeTasksResponse' {replaceRootVolumeTasks} -> replaceRootVolumeTasks) (\s@DescribeReplaceRootVolumeTasksResponse' {} a -> s {replaceRootVolumeTasks = a} :: DescribeReplaceRootVolumeTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplaceRootVolumeTasksResponse_httpStatus :: Lens.Lens' DescribeReplaceRootVolumeTasksResponse Prelude.Int
describeReplaceRootVolumeTasksResponse_httpStatus = Lens.lens (\DescribeReplaceRootVolumeTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeReplaceRootVolumeTasksResponse' {} a -> s {httpStatus = a} :: DescribeReplaceRootVolumeTasksResponse)

instance
  Prelude.NFData
    DescribeReplaceRootVolumeTasksResponse
  where
  rnf DescribeReplaceRootVolumeTasksResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf replaceRootVolumeTasks `Prelude.seq`
        Prelude.rnf httpStatus
