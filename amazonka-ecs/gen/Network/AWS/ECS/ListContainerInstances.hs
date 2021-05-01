{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters the container instances by status. For example, if you specify
    -- the @DRAINING@ status, the results include only container instances that
    -- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
    -- do not specify this parameter, the default is to include container
    -- instances set to all states other than @INACTIVE@.
    status :: Prelude.Maybe ContainerInstanceStatus,
    -- | The maximum number of container instance results returned by
    -- @ListContainerInstances@ in paginated output. When this parameter is
    -- used, @ListContainerInstances@ only returns @maxResults@ results in a
    -- single page along with a @nextToken@ response element. The remaining
    -- results of the initial request can be seen by sending another
    -- @ListContainerInstances@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter is not used, then
    -- @ListContainerInstances@ returns up to 100 results and a @nextToken@
    -- value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | You can filter the results of a @ListContainerInstances@ operation with
    -- cluster query language statements. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    filter' :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instances to list. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing,
      cluster = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a @ListContainerInstances@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listContainerInstances_nextToken :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_nextToken = Lens.lens (\ListContainerInstances' {nextToken} -> nextToken) (\s@ListContainerInstances' {} a -> s {nextToken = a} :: ListContainerInstances)

-- | Filters the container instances by status. For example, if you specify
-- the @DRAINING@ status, the results include only container instances that
-- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
-- do not specify this parameter, the default is to include container
-- instances set to all states other than @INACTIVE@.
listContainerInstances_status :: Lens.Lens' ListContainerInstances (Prelude.Maybe ContainerInstanceStatus)
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
listContainerInstances_maxResults :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Int)
listContainerInstances_maxResults = Lens.lens (\ListContainerInstances' {maxResults} -> maxResults) (\s@ListContainerInstances' {} a -> s {maxResults = a} :: ListContainerInstances)

-- | You can filter the results of a @ListContainerInstances@ operation with
-- cluster query language statements. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
listContainerInstances_filter :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_filter = Lens.lens (\ListContainerInstances' {filter'} -> filter') (\s@ListContainerInstances' {} a -> s {filter' = a} :: ListContainerInstances)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed.
listContainerInstances_cluster :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_cluster = Lens.lens (\ListContainerInstances' {cluster} -> cluster) (\s@ListContainerInstances' {} a -> s {cluster = a} :: ListContainerInstances)

instance Pager.AWSPager ListContainerInstances where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listContainerInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listContainerInstancesResponse_containerInstanceArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listContainerInstances_nextToken
          Lens..~ rs
          Lens.^? listContainerInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListContainerInstances where
  type
    Rs ListContainerInstances =
      ListContainerInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContainerInstancesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "containerInstanceArns"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContainerInstances

instance Prelude.NFData ListContainerInstances

instance Prelude.ToHeaders ListContainerInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.ListContainerInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListContainerInstances where
  toJSON ListContainerInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("status" Prelude..=) Prelude.<$> status,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("filter" Prelude..=) Prelude.<$> filter',
            ("cluster" Prelude..=) Prelude.<$> cluster
          ]
      )

instance Prelude.ToPath ListContainerInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListContainerInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContainerInstancesResponse' smart constructor.
data ListContainerInstancesResponse = ListContainerInstancesResponse'
  { -- | The @nextToken@ value to include in a future @ListContainerInstances@
    -- request. When the results of a @ListContainerInstances@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of container instances with full ARN entries for each container
    -- instance associated with the specified cluster.
    containerInstanceArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListContainerInstancesResponse
newListContainerInstancesResponse pHttpStatus_ =
  ListContainerInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      containerInstanceArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListContainerInstances@
-- request. When the results of a @ListContainerInstances@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listContainerInstancesResponse_nextToken :: Lens.Lens' ListContainerInstancesResponse (Prelude.Maybe Prelude.Text)
listContainerInstancesResponse_nextToken = Lens.lens (\ListContainerInstancesResponse' {nextToken} -> nextToken) (\s@ListContainerInstancesResponse' {} a -> s {nextToken = a} :: ListContainerInstancesResponse)

-- | The list of container instances with full ARN entries for each container
-- instance associated with the specified cluster.
listContainerInstancesResponse_containerInstanceArns :: Lens.Lens' ListContainerInstancesResponse (Prelude.Maybe [Prelude.Text])
listContainerInstancesResponse_containerInstanceArns = Lens.lens (\ListContainerInstancesResponse' {containerInstanceArns} -> containerInstanceArns) (\s@ListContainerInstancesResponse' {} a -> s {containerInstanceArns = a} :: ListContainerInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listContainerInstancesResponse_httpStatus :: Lens.Lens' ListContainerInstancesResponse Prelude.Int
listContainerInstancesResponse_httpStatus = Lens.lens (\ListContainerInstancesResponse' {httpStatus} -> httpStatus) (\s@ListContainerInstancesResponse' {} a -> s {httpStatus = a} :: ListContainerInstancesResponse)

instance
  Prelude.NFData
    ListContainerInstancesResponse
