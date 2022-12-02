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
-- Module      : Amazonka.OpsWorks.DescribeEcsClusters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon ECS clusters that are registered with a stack. If you
-- specify only a stack ID, you can use the @MaxResults@ and @NextToken@
-- parameters to paginate the response. However, AWS OpsWorks Stacks
-- currently supports only one cluster per layer, so the result set has a
-- maximum of one element.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack or an attached
-- policy that explicitly grants permission. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- This call accepts only one resource-identifying parameter.
--
-- This operation returns paginated results.
module Amazonka.OpsWorks.DescribeEcsClusters
  ( -- * Creating a Request
    DescribeEcsClusters (..),
    newDescribeEcsClusters,

    -- * Request Lenses
    describeEcsClusters_stackId,
    describeEcsClusters_nextToken,
    describeEcsClusters_ecsClusterArns,
    describeEcsClusters_maxResults,

    -- * Destructuring the Response
    DescribeEcsClustersResponse (..),
    newDescribeEcsClustersResponse,

    -- * Response Lenses
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEcsClusters' smart constructor.
data DescribeEcsClusters = DescribeEcsClusters'
  { -- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster
    -- that is registered with the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s@NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @DescribeEcsClusters@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of ARNs, one for each cluster to be described.
    ecsClusterArns :: Prelude.Maybe [Prelude.Text],
    -- | To receive a paginated response, use this parameter to specify the
    -- maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEcsClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeEcsClusters_stackId' - A stack ID. @DescribeEcsClusters@ returns a description of the cluster
-- that is registered with the stack.
--
-- 'nextToken', 'describeEcsClusters_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s@NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @DescribeEcsClusters@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
--
-- 'ecsClusterArns', 'describeEcsClusters_ecsClusterArns' - A list of ARNs, one for each cluster to be described.
--
-- 'maxResults', 'describeEcsClusters_maxResults' - To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
newDescribeEcsClusters ::
  DescribeEcsClusters
newDescribeEcsClusters =
  DescribeEcsClusters'
    { stackId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      ecsClusterArns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster
-- that is registered with the stack.
describeEcsClusters_stackId :: Lens.Lens' DescribeEcsClusters (Prelude.Maybe Prelude.Text)
describeEcsClusters_stackId = Lens.lens (\DescribeEcsClusters' {stackId} -> stackId) (\s@DescribeEcsClusters' {} a -> s {stackId = a} :: DescribeEcsClusters)

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s@NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @DescribeEcsClusters@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
describeEcsClusters_nextToken :: Lens.Lens' DescribeEcsClusters (Prelude.Maybe Prelude.Text)
describeEcsClusters_nextToken = Lens.lens (\DescribeEcsClusters' {nextToken} -> nextToken) (\s@DescribeEcsClusters' {} a -> s {nextToken = a} :: DescribeEcsClusters)

-- | A list of ARNs, one for each cluster to be described.
describeEcsClusters_ecsClusterArns :: Lens.Lens' DescribeEcsClusters (Prelude.Maybe [Prelude.Text])
describeEcsClusters_ecsClusterArns = Lens.lens (\DescribeEcsClusters' {ecsClusterArns} -> ecsClusterArns) (\s@DescribeEcsClusters' {} a -> s {ecsClusterArns = a} :: DescribeEcsClusters) Prelude.. Lens.mapping Lens.coerced

-- | To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
describeEcsClusters_maxResults :: Lens.Lens' DescribeEcsClusters (Prelude.Maybe Prelude.Int)
describeEcsClusters_maxResults = Lens.lens (\DescribeEcsClusters' {maxResults} -> maxResults) (\s@DescribeEcsClusters' {} a -> s {maxResults = a} :: DescribeEcsClusters)

instance Core.AWSPager DescribeEcsClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEcsClustersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEcsClustersResponse_ecsClusters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEcsClusters_nextToken
          Lens..~ rs
          Lens.^? describeEcsClustersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEcsClusters where
  type
    AWSResponse DescribeEcsClusters =
      DescribeEcsClustersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEcsClustersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "EcsClusters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEcsClusters where
  hashWithSalt _salt DescribeEcsClusters' {..} =
    _salt `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` ecsClusterArns
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeEcsClusters where
  rnf DescribeEcsClusters' {..} =
    Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ecsClusterArns
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeEcsClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeEcsClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEcsClusters where
  toJSON DescribeEcsClusters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StackId" Data..=) Prelude.<$> stackId,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("EcsClusterArns" Data..=)
              Prelude.<$> ecsClusterArns,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeEcsClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEcsClusters where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeEcsClusters@ request.
--
-- /See:/ 'newDescribeEcsClustersResponse' smart constructor.
data DescribeEcsClustersResponse = DescribeEcsClustersResponse'
  { -- | If a paginated request does not return all of the remaining results,
    -- this parameter is set to a token that you can assign to the request
    -- object\'s @NextToken@ parameter to retrieve the next set of results. If
    -- the previous paginated request returned all of the remaining results,
    -- this parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @EcsCluster@ objects containing the cluster descriptions.
    ecsClusters :: Prelude.Maybe [EcsCluster],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEcsClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEcsClustersResponse_nextToken' - If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to retrieve the next set of results. If
-- the previous paginated request returned all of the remaining results,
-- this parameter is set to @null@.
--
-- 'ecsClusters', 'describeEcsClustersResponse_ecsClusters' - A list of @EcsCluster@ objects containing the cluster descriptions.
--
-- 'httpStatus', 'describeEcsClustersResponse_httpStatus' - The response's http status code.
newDescribeEcsClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEcsClustersResponse
newDescribeEcsClustersResponse pHttpStatus_ =
  DescribeEcsClustersResponse'
    { nextToken =
        Prelude.Nothing,
      ecsClusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to retrieve the next set of results. If
-- the previous paginated request returned all of the remaining results,
-- this parameter is set to @null@.
describeEcsClustersResponse_nextToken :: Lens.Lens' DescribeEcsClustersResponse (Prelude.Maybe Prelude.Text)
describeEcsClustersResponse_nextToken = Lens.lens (\DescribeEcsClustersResponse' {nextToken} -> nextToken) (\s@DescribeEcsClustersResponse' {} a -> s {nextToken = a} :: DescribeEcsClustersResponse)

-- | A list of @EcsCluster@ objects containing the cluster descriptions.
describeEcsClustersResponse_ecsClusters :: Lens.Lens' DescribeEcsClustersResponse (Prelude.Maybe [EcsCluster])
describeEcsClustersResponse_ecsClusters = Lens.lens (\DescribeEcsClustersResponse' {ecsClusters} -> ecsClusters) (\s@DescribeEcsClustersResponse' {} a -> s {ecsClusters = a} :: DescribeEcsClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEcsClustersResponse_httpStatus :: Lens.Lens' DescribeEcsClustersResponse Prelude.Int
describeEcsClustersResponse_httpStatus = Lens.lens (\DescribeEcsClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeEcsClustersResponse' {} a -> s {httpStatus = a} :: DescribeEcsClustersResponse)

instance Prelude.NFData DescribeEcsClustersResponse where
  rnf DescribeEcsClustersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ecsClusters
      `Prelude.seq` Prelude.rnf httpStatus
