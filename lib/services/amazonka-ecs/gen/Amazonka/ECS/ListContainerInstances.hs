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
-- Module      : Amazonka.ECS.ListContainerInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ECS.ListContainerInstances
  ( -- * Creating a Request
    ListContainerInstances (..),
    newListContainerInstances,

    -- * Request Lenses
    listContainerInstances_nextToken,
    listContainerInstances_cluster,
    listContainerInstances_status,
    listContainerInstances_filter,
    listContainerInstances_maxResults,

    -- * Destructuring the Response
    ListContainerInstancesResponse (..),
    newListContainerInstancesResponse,

    -- * Response Lenses
    listContainerInstancesResponse_nextToken,
    listContainerInstancesResponse_containerInstanceArns,
    listContainerInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContainerInstances' smart constructor.
data ListContainerInstances = ListContainerInstances'
  { -- | The @nextToken@ value returned from a @ListContainerInstances@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls are needed. If @maxResults@ was provided, it\'s possible
    -- the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instances to list. If you do not specify a cluster,
    -- the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Filters the container instances by status. For example, if you specify
    -- the @DRAINING@ status, the results include only container instances that
    -- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
    -- don\'t specify this parameter, the default is to include container
    -- instances set to all states other than @INACTIVE@.
    status :: Prelude.Maybe ContainerInstanceStatus,
    -- | You can filter the results of a @ListContainerInstances@ operation with
    -- cluster query language statements. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    filter' :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of container instance results that
    -- @ListContainerInstances@ returned in paginated output. When this
    -- parameter is used, @ListContainerInstances@ only returns @maxResults@
    -- results in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @ListContainerInstances@ request with the returned @nextToken@ value.
    -- This value can be between 1 and 100. If this parameter isn\'t used, then
    -- @ListContainerInstances@ returns up to 100 results and a @nextToken@
    -- value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- further calls are needed. If @maxResults@ was provided, it\'s possible
-- the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'cluster', 'listContainerInstances_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed.
--
-- 'status', 'listContainerInstances_status' - Filters the container instances by status. For example, if you specify
-- the @DRAINING@ status, the results include only container instances that
-- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
-- don\'t specify this parameter, the default is to include container
-- instances set to all states other than @INACTIVE@.
--
-- 'filter'', 'listContainerInstances_filter' - You can filter the results of a @ListContainerInstances@ operation with
-- cluster query language statements. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'maxResults', 'listContainerInstances_maxResults' - The maximum number of container instance results that
-- @ListContainerInstances@ returned in paginated output. When this
-- parameter is used, @ListContainerInstances@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListContainerInstances@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListContainerInstances@ returns up to 100 results and a @nextToken@
-- value if applicable.
newListContainerInstances ::
  ListContainerInstances
newListContainerInstances =
  ListContainerInstances'
    { nextToken =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      status = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a @ListContainerInstances@ request
-- indicating that more results are available to fulfill the request and
-- further calls are needed. If @maxResults@ was provided, it\'s possible
-- the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listContainerInstances_nextToken :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_nextToken = Lens.lens (\ListContainerInstances' {nextToken} -> nextToken) (\s@ListContainerInstances' {} a -> s {nextToken = a} :: ListContainerInstances)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to list. If you do not specify a cluster,
-- the default cluster is assumed.
listContainerInstances_cluster :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_cluster = Lens.lens (\ListContainerInstances' {cluster} -> cluster) (\s@ListContainerInstances' {} a -> s {cluster = a} :: ListContainerInstances)

-- | Filters the container instances by status. For example, if you specify
-- the @DRAINING@ status, the results include only container instances that
-- have been set to @DRAINING@ using UpdateContainerInstancesState. If you
-- don\'t specify this parameter, the default is to include container
-- instances set to all states other than @INACTIVE@.
listContainerInstances_status :: Lens.Lens' ListContainerInstances (Prelude.Maybe ContainerInstanceStatus)
listContainerInstances_status = Lens.lens (\ListContainerInstances' {status} -> status) (\s@ListContainerInstances' {} a -> s {status = a} :: ListContainerInstances)

-- | You can filter the results of a @ListContainerInstances@ operation with
-- cluster query language statements. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language>
-- in the /Amazon Elastic Container Service Developer Guide/.
listContainerInstances_filter :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Text)
listContainerInstances_filter = Lens.lens (\ListContainerInstances' {filter'} -> filter') (\s@ListContainerInstances' {} a -> s {filter' = a} :: ListContainerInstances)

-- | The maximum number of container instance results that
-- @ListContainerInstances@ returned in paginated output. When this
-- parameter is used, @ListContainerInstances@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListContainerInstances@ request with the returned @nextToken@ value.
-- This value can be between 1 and 100. If this parameter isn\'t used, then
-- @ListContainerInstances@ returns up to 100 results and a @nextToken@
-- value if applicable.
listContainerInstances_maxResults :: Lens.Lens' ListContainerInstances (Prelude.Maybe Prelude.Int)
listContainerInstances_maxResults = Lens.lens (\ListContainerInstances' {maxResults} -> maxResults) (\s@ListContainerInstances' {} a -> s {maxResults = a} :: ListContainerInstances)

instance Core.AWSPager ListContainerInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContainerInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContainerInstancesResponse_containerInstanceArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listContainerInstances_nextToken
          Lens..~ rs
          Lens.^? listContainerInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListContainerInstances where
  type
    AWSResponse ListContainerInstances =
      ListContainerInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContainerInstancesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "containerInstanceArns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContainerInstances where
  hashWithSalt _salt ListContainerInstances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListContainerInstances where
  rnf ListContainerInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListContainerInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListContainerInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListContainerInstances where
  toJSON ListContainerInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("cluster" Core..=) Prelude.<$> cluster,
            ("status" Core..=) Prelude.<$> status,
            ("filter" Core..=) Prelude.<$> filter',
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListContainerInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery ListContainerInstances where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listContainerInstancesResponse_containerInstanceArns = Lens.lens (\ListContainerInstancesResponse' {containerInstanceArns} -> containerInstanceArns) (\s@ListContainerInstancesResponse' {} a -> s {containerInstanceArns = a} :: ListContainerInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listContainerInstancesResponse_httpStatus :: Lens.Lens' ListContainerInstancesResponse Prelude.Int
listContainerInstancesResponse_httpStatus = Lens.lens (\ListContainerInstancesResponse' {httpStatus} -> httpStatus) (\s@ListContainerInstancesResponse' {} a -> s {httpStatus = a} :: ListContainerInstancesResponse)

instance
  Prelude.NFData
    ListContainerInstancesResponse
  where
  rnf ListContainerInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf containerInstanceArns
      `Prelude.seq` Prelude.rnf httpStatus
