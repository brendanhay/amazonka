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
-- Module      : Network.AWS.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of container instances in a specified cluster. You can
-- filter the results of a @ListContainerInstances@ operation with cluster
-- query language statements inside the @filter@ parameter. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListContainerInstances
  ( -- * Creating a Request
    ListContainerInstances (..),
    newListContainerInstances,

    -- * Request Lenses
    listContainerInstances_nextToken,
    listContainerInstances_status,
    listContainerInstances_maxResults,
    listContainerInstances_filter,
    listContainerInstances_cluster,

    -- * Destructuring the Response
    ListContainerInstancesResponse (..),
    newListContainerInstancesResponse,

    -- * Response Lenses
    listContainerInstancesResponse_nextToken,
    listContainerInstancesResponse_containerInstanceArns,
    listContainerInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListContainerInstances' smart constructor.
data ListContainerInstances = ListContainerInstances'
  { -- | The @nextToken@ value returned from a @ListContainerInstances@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it is
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the container instances by status. For example, if you specify
    -- the @DRAINING@ status, the results include only container instances that
    -- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
    -- do not specify this parameter, the default is to include container
    -- instances set to all states other than @INACTIVE@.
    status :: Core.Maybe ContainerInstanceStatus,
    -- | The maximum number of container instance results returned by
    -- @ListContainerInstances@ in paginated output. When this parameter is
    -- used, @ListContainerInstances@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @ListContainerInstances@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter is not used, then
    -- @ListContainerInstances@ returns up to 100 results and a @nextToken@
    -- value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | You can filter the results of a @ListContainerInstances@ operation with
    -- cluster query language statements. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    filter' :: Core.Maybe Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instances to list. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListContainerInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContainerInstances_nextToken' - The @nextToken@ value returned from a @ListContainerInstances@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'status', 'listContainerInstances_status' - Filters the container instances by status. For example, if you specify
-- the @DRAINING@ status, the results include only container instances that
-- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
-- do not specify this parameter, the default is to include container
-- instances set to all states other than @INACTIVE@.
--
-- 'maxResults', 'listContainerInstances_maxResults' - The maximum number of container instance results returned by
-- @ListContainerInstances@ in paginated output. When this parameter is
-- used, @ListContainerInstances@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListContainerInstances@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- @ListContainerInstances@ returns up to 100 results and a @nextToken@
-- value if applicable.
--
-- 'filter'', 'listContainerInstances_filter' - You can filter the results of a @ListContainerInstances@ operation with
-- cluster query language statements. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'cluster', 'listContainerInstances_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed.
newListContainerInstances ::
  ListContainerInstances
newListContainerInstances =
  ListContainerInstances'
    { nextToken = Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      cluster = Core.Nothing
    }

-- | The @nextToken@ value returned from a @ListContainerInstances@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listContainerInstances_nextToken :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
listContainerInstances_nextToken = Lens.lens (\ListContainerInstances' {nextToken} -> nextToken) (\s@ListContainerInstances' {} a -> s {nextToken = a} :: ListContainerInstances)

-- | Filters the container instances by status. For example, if you specify
-- the @DRAINING@ status, the results include only container instances that
-- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
-- do not specify this parameter, the default is to include container
-- instances set to all states other than @INACTIVE@.
listContainerInstances_status :: Lens.Lens' ListContainerInstances (Core.Maybe ContainerInstanceStatus)
listContainerInstances_status = Lens.lens (\ListContainerInstances' {status} -> status) (\s@ListContainerInstances' {} a -> s {status = a} :: ListContainerInstances)

-- | The maximum number of container instance results returned by
-- @ListContainerInstances@ in paginated output. When this parameter is
-- used, @ListContainerInstances@ only returns @maxResults@ results in a
-- single page along with a @nextToken@ response element. The remaining
-- results of the initial request can be seen by sending another
-- @ListContainerInstances@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter is not used, then
-- @ListContainerInstances@ returns up to 100 results and a @nextToken@
-- value if applicable.
listContainerInstances_maxResults :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Int)
listContainerInstances_maxResults = Lens.lens (\ListContainerInstances' {maxResults} -> maxResults) (\s@ListContainerInstances' {} a -> s {maxResults = a} :: ListContainerInstances)

-- | You can filter the results of a @ListContainerInstances@ operation with
-- cluster query language statements. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
listContainerInstances_filter :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
listContainerInstances_filter = Lens.lens (\ListContainerInstances' {filter'} -> filter') (\s@ListContainerInstances' {} a -> s {filter' = a} :: ListContainerInstances)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed.
listContainerInstances_cluster :: Lens.Lens' ListContainerInstances (Core.Maybe Core.Text)
listContainerInstances_cluster = Lens.lens (\ListContainerInstances' {cluster} -> cluster) (\s@ListContainerInstances' {} a -> s {cluster = a} :: ListContainerInstances)

instance Core.AWSPager ListContainerInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContainerInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listContainerInstancesResponse_containerInstanceArns
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listContainerInstances_nextToken
          Lens..~ rs
          Lens.^? listContainerInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListContainerInstances where
  type
    AWSResponse ListContainerInstances =
      ListContainerInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContainerInstancesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "containerInstanceArns"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListContainerInstances

instance Core.NFData ListContainerInstances

instance Core.ToHeaders ListContainerInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListContainerInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListContainerInstances where
  toJSON ListContainerInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.ToPath ListContainerInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListContainerInstances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListContainerInstancesResponse' smart constructor.
data ListContainerInstancesResponse = ListContainerInstancesResponse'
  { -- | The @nextToken@ value to include in a future @ListContainerInstances@
    -- request. When the results of a @ListContainerInstances@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of container instances with full ARN entries for each container
    -- instance associated with the specified cluster.
    containerInstanceArns :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListContainerInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContainerInstancesResponse_nextToken' - The @nextToken@ value to include in a future @ListContainerInstances@
-- request. When the results of a @ListContainerInstances@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'containerInstanceArns', 'listContainerInstancesResponse_containerInstanceArns' - The list of container instances with full ARN entries for each container
-- instance associated with the specified cluster.
--
-- 'httpStatus', 'listContainerInstancesResponse_httpStatus' - The response's http status code.
newListContainerInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListContainerInstancesResponse
newListContainerInstancesResponse pHttpStatus_ =
  ListContainerInstancesResponse'
    { nextToken =
        Core.Nothing,
      containerInstanceArns = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListContainerInstances@
-- request. When the results of a @ListContainerInstances@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listContainerInstancesResponse_nextToken :: Lens.Lens' ListContainerInstancesResponse (Core.Maybe Core.Text)
listContainerInstancesResponse_nextToken = Lens.lens (\ListContainerInstancesResponse' {nextToken} -> nextToken) (\s@ListContainerInstancesResponse' {} a -> s {nextToken = a} :: ListContainerInstancesResponse)

-- | The list of container instances with full ARN entries for each container
-- instance associated with the specified cluster.
listContainerInstancesResponse_containerInstanceArns :: Lens.Lens' ListContainerInstancesResponse (Core.Maybe [Core.Text])
listContainerInstancesResponse_containerInstanceArns = Lens.lens (\ListContainerInstancesResponse' {containerInstanceArns} -> containerInstanceArns) (\s@ListContainerInstancesResponse' {} a -> s {containerInstanceArns = a} :: ListContainerInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listContainerInstancesResponse_httpStatus :: Lens.Lens' ListContainerInstancesResponse Core.Int
listContainerInstancesResponse_httpStatus = Lens.lens (\ListContainerInstancesResponse' {httpStatus} -> httpStatus) (\s@ListContainerInstancesResponse' {} a -> s {httpStatus = a} :: ListContainerInstancesResponse)

instance Core.NFData ListContainerInstancesResponse
