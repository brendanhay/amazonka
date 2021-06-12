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
-- Module      : Network.AWS.OpsWorks.DescribeEcsClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.DescribeEcsClusters
  ( -- * Creating a Request
    DescribeEcsClusters (..),
    newDescribeEcsClusters,

    -- * Request Lenses
    describeEcsClusters_nextToken,
    describeEcsClusters_maxResults,
    describeEcsClusters_stackId,
    describeEcsClusters_ecsClusterArns,

    -- * Destructuring the Response
    DescribeEcsClustersResponse (..),
    newDescribeEcsClustersResponse,

    -- * Response Lenses
    describeEcsClustersResponse_nextToken,
    describeEcsClustersResponse_ecsClusters,
    describeEcsClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEcsClusters' smart constructor.
data DescribeEcsClusters = DescribeEcsClusters'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s@NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @DescribeEcsClusters@
    -- again and assign that token to the request object\'s @NextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s @NextToken@ parameter is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | To receive a paginated response, use this parameter to specify the
    -- maximum number of results to be returned with a single call. If the
    -- number of available results exceeds this maximum, the response includes
    -- a @NextToken@ value that you can assign to the @NextToken@ request
    -- parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Int,
    -- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster
    -- that is registered with the stack.
    stackId :: Core.Maybe Core.Text,
    -- | A list of ARNs, one for each cluster to be described.
    ecsClusterArns :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEcsClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEcsClusters_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s@NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @DescribeEcsClusters@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
--
-- 'maxResults', 'describeEcsClusters_maxResults' - To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
--
-- 'stackId', 'describeEcsClusters_stackId' - A stack ID. @DescribeEcsClusters@ returns a description of the cluster
-- that is registered with the stack.
--
-- 'ecsClusterArns', 'describeEcsClusters_ecsClusterArns' - A list of ARNs, one for each cluster to be described.
newDescribeEcsClusters ::
  DescribeEcsClusters
newDescribeEcsClusters =
  DescribeEcsClusters'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      stackId = Core.Nothing,
      ecsClusterArns = Core.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s@NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @DescribeEcsClusters@
-- again and assign that token to the request object\'s @NextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s @NextToken@ parameter is set to @null@.
describeEcsClusters_nextToken :: Lens.Lens' DescribeEcsClusters (Core.Maybe Core.Text)
describeEcsClusters_nextToken = Lens.lens (\DescribeEcsClusters' {nextToken} -> nextToken) (\s@DescribeEcsClusters' {} a -> s {nextToken = a} :: DescribeEcsClusters)

-- | To receive a paginated response, use this parameter to specify the
-- maximum number of results to be returned with a single call. If the
-- number of available results exceeds this maximum, the response includes
-- a @NextToken@ value that you can assign to the @NextToken@ request
-- parameter to get the next set of results.
describeEcsClusters_maxResults :: Lens.Lens' DescribeEcsClusters (Core.Maybe Core.Int)
describeEcsClusters_maxResults = Lens.lens (\DescribeEcsClusters' {maxResults} -> maxResults) (\s@DescribeEcsClusters' {} a -> s {maxResults = a} :: DescribeEcsClusters)

-- | A stack ID. @DescribeEcsClusters@ returns a description of the cluster
-- that is registered with the stack.
describeEcsClusters_stackId :: Lens.Lens' DescribeEcsClusters (Core.Maybe Core.Text)
describeEcsClusters_stackId = Lens.lens (\DescribeEcsClusters' {stackId} -> stackId) (\s@DescribeEcsClusters' {} a -> s {stackId = a} :: DescribeEcsClusters)

-- | A list of ARNs, one for each cluster to be described.
describeEcsClusters_ecsClusterArns :: Lens.Lens' DescribeEcsClusters (Core.Maybe [Core.Text])
describeEcsClusters_ecsClusterArns = Lens.lens (\DescribeEcsClusters' {ecsClusterArns} -> ecsClusterArns) (\s@DescribeEcsClusters' {} a -> s {ecsClusterArns = a} :: DescribeEcsClusters) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeEcsClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEcsClustersResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEcsClustersResponse_ecsClusters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEcsClusters_nextToken
          Lens..~ rs
          Lens.^? describeEcsClustersResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeEcsClusters where
  type
    AWSResponse DescribeEcsClusters =
      DescribeEcsClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEcsClustersResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "EcsClusters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEcsClusters

instance Core.NFData DescribeEcsClusters

instance Core.ToHeaders DescribeEcsClusters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeEcsClusters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEcsClusters where
  toJSON DescribeEcsClusters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StackId" Core..=) Core.<$> stackId,
            ("EcsClusterArns" Core..=) Core.<$> ecsClusterArns
          ]
      )

instance Core.ToPath DescribeEcsClusters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEcsClusters where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeEcsClusters@ request.
--
-- /See:/ 'newDescribeEcsClustersResponse' smart constructor.
data DescribeEcsClustersResponse = DescribeEcsClustersResponse'
  { -- | If a paginated request does not return all of the remaining results,
    -- this parameter is set to a token that you can assign to the request
    -- object\'s @NextToken@ parameter to retrieve the next set of results. If
    -- the previous paginated request returned all of the remaining results,
    -- this parameter is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @EcsCluster@ objects containing the cluster descriptions.
    ecsClusters :: Core.Maybe [EcsCluster],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEcsClustersResponse
newDescribeEcsClustersResponse pHttpStatus_ =
  DescribeEcsClustersResponse'
    { nextToken =
        Core.Nothing,
      ecsClusters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a paginated request does not return all of the remaining results,
-- this parameter is set to a token that you can assign to the request
-- object\'s @NextToken@ parameter to retrieve the next set of results. If
-- the previous paginated request returned all of the remaining results,
-- this parameter is set to @null@.
describeEcsClustersResponse_nextToken :: Lens.Lens' DescribeEcsClustersResponse (Core.Maybe Core.Text)
describeEcsClustersResponse_nextToken = Lens.lens (\DescribeEcsClustersResponse' {nextToken} -> nextToken) (\s@DescribeEcsClustersResponse' {} a -> s {nextToken = a} :: DescribeEcsClustersResponse)

-- | A list of @EcsCluster@ objects containing the cluster descriptions.
describeEcsClustersResponse_ecsClusters :: Lens.Lens' DescribeEcsClustersResponse (Core.Maybe [EcsCluster])
describeEcsClustersResponse_ecsClusters = Lens.lens (\DescribeEcsClustersResponse' {ecsClusters} -> ecsClusters) (\s@DescribeEcsClustersResponse' {} a -> s {ecsClusters = a} :: DescribeEcsClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEcsClustersResponse_httpStatus :: Lens.Lens' DescribeEcsClustersResponse Core.Int
describeEcsClustersResponse_httpStatus = Lens.lens (\DescribeEcsClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeEcsClustersResponse' {} a -> s {httpStatus = a} :: DescribeEcsClustersResponse)

instance Core.NFData DescribeEcsClustersResponse
