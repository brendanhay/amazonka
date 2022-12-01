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
-- Module      : Amazonka.ECS.ListServices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of services. You can filter the results by cluster,
-- launch type, and scheduling strategy.
--
-- This operation returns paginated results.
module Amazonka.ECS.ListServices
  ( -- * Creating a Request
    ListServices (..),
    newListServices,

    -- * Request Lenses
    listServices_nextToken,
    listServices_schedulingStrategy,
    listServices_cluster,
    listServices_maxResults,
    listServices_launchType,

    -- * Destructuring the Response
    ListServicesResponse (..),
    newListServicesResponse,

    -- * Response Lenses
    listServicesResponse_nextToken,
    listServicesResponse_serviceArns,
    listServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServices' smart constructor.
data ListServices = ListServices'
  { -- | The @nextToken@ value returned from a @ListServices@ request indicating
    -- that more results are available to fulfill the request and further calls
    -- will be needed. If @maxResults@ was provided, it is possible the number
    -- of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The scheduling strategy to use when filtering the @ListServices@
    -- results.
    schedulingStrategy :: Prelude.Maybe SchedulingStrategy,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to use
    -- when filtering the @ListServices@ results. If you do not specify a
    -- cluster, the default cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of service results that @ListServices@ returned in
    -- paginated output. When this parameter is used, @ListServices@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @ListServices@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter isn\'t used, then @ListServices@ returns up to 10 results and
    -- a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The launch type to use when filtering the @ListServices@ results.
    launchType :: Prelude.Maybe LaunchType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServices_nextToken' - The @nextToken@ value returned from a @ListServices@ request indicating
-- that more results are available to fulfill the request and further calls
-- will be needed. If @maxResults@ was provided, it is possible the number
-- of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'schedulingStrategy', 'listServices_schedulingStrategy' - The scheduling strategy to use when filtering the @ListServices@
-- results.
--
-- 'cluster', 'listServices_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to use
-- when filtering the @ListServices@ results. If you do not specify a
-- cluster, the default cluster is assumed.
--
-- 'maxResults', 'listServices_maxResults' - The maximum number of service results that @ListServices@ returned in
-- paginated output. When this parameter is used, @ListServices@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListServices@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @ListServices@ returns up to 10 results and
-- a @nextToken@ value if applicable.
--
-- 'launchType', 'listServices_launchType' - The launch type to use when filtering the @ListServices@ results.
newListServices ::
  ListServices
newListServices =
  ListServices'
    { nextToken = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      cluster = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      launchType = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a @ListServices@ request indicating
-- that more results are available to fulfill the request and further calls
-- will be needed. If @maxResults@ was provided, it is possible the number
-- of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listServices_nextToken :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Text)
listServices_nextToken = Lens.lens (\ListServices' {nextToken} -> nextToken) (\s@ListServices' {} a -> s {nextToken = a} :: ListServices)

-- | The scheduling strategy to use when filtering the @ListServices@
-- results.
listServices_schedulingStrategy :: Lens.Lens' ListServices (Prelude.Maybe SchedulingStrategy)
listServices_schedulingStrategy = Lens.lens (\ListServices' {schedulingStrategy} -> schedulingStrategy) (\s@ListServices' {} a -> s {schedulingStrategy = a} :: ListServices)

-- | The short name or full Amazon Resource Name (ARN) of the cluster to use
-- when filtering the @ListServices@ results. If you do not specify a
-- cluster, the default cluster is assumed.
listServices_cluster :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Text)
listServices_cluster = Lens.lens (\ListServices' {cluster} -> cluster) (\s@ListServices' {} a -> s {cluster = a} :: ListServices)

-- | The maximum number of service results that @ListServices@ returned in
-- paginated output. When this parameter is used, @ListServices@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListServices@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @ListServices@ returns up to 10 results and
-- a @nextToken@ value if applicable.
listServices_maxResults :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Int)
listServices_maxResults = Lens.lens (\ListServices' {maxResults} -> maxResults) (\s@ListServices' {} a -> s {maxResults = a} :: ListServices)

-- | The launch type to use when filtering the @ListServices@ results.
listServices_launchType :: Lens.Lens' ListServices (Prelude.Maybe LaunchType)
listServices_launchType = Lens.lens (\ListServices' {launchType} -> launchType) (\s@ListServices' {} a -> s {launchType = a} :: ListServices)

instance Core.AWSPager ListServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_serviceArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServices_nextToken
          Lens..~ rs
          Lens.^? listServicesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListServices where
  type AWSResponse ListServices = ListServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "serviceArns" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServices where
  hashWithSalt _salt ListServices' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schedulingStrategy
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` launchType

instance Prelude.NFData ListServices where
  rnf ListServices' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schedulingStrategy
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf launchType

instance Core.ToHeaders ListServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListServices where
  toJSON ListServices' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("schedulingStrategy" Core..=)
              Prelude.<$> schedulingStrategy,
            ("cluster" Core..=) Prelude.<$> cluster,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("launchType" Core..=) Prelude.<$> launchType
          ]
      )

instance Core.ToPath ListServices where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | The @nextToken@ value to include in a future @ListServices@ request.
    -- When the results of a @ListServices@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of full ARN entries for each service that\'s associated with
    -- the specified cluster.
    serviceArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicesResponse_nextToken' - The @nextToken@ value to include in a future @ListServices@ request.
-- When the results of a @ListServices@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'serviceArns', 'listServicesResponse_serviceArns' - The list of full ARN entries for each service that\'s associated with
-- the specified cluster.
--
-- 'httpStatus', 'listServicesResponse_httpStatus' - The response's http status code.
newListServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServicesResponse
newListServicesResponse pHttpStatus_ =
  ListServicesResponse'
    { nextToken = Prelude.Nothing,
      serviceArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListServices@ request.
-- When the results of a @ListServices@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listServicesResponse_nextToken :: Lens.Lens' ListServicesResponse (Prelude.Maybe Prelude.Text)
listServicesResponse_nextToken = Lens.lens (\ListServicesResponse' {nextToken} -> nextToken) (\s@ListServicesResponse' {} a -> s {nextToken = a} :: ListServicesResponse)

-- | The list of full ARN entries for each service that\'s associated with
-- the specified cluster.
listServicesResponse_serviceArns :: Lens.Lens' ListServicesResponse (Prelude.Maybe [Prelude.Text])
listServicesResponse_serviceArns = Lens.lens (\ListServicesResponse' {serviceArns} -> serviceArns) (\s@ListServicesResponse' {} a -> s {serviceArns = a} :: ListServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServicesResponse_httpStatus :: Lens.Lens' ListServicesResponse Prelude.Int
listServicesResponse_httpStatus = Lens.lens (\ListServicesResponse' {httpStatus} -> httpStatus) (\s@ListServicesResponse' {} a -> s {httpStatus = a} :: ListServicesResponse)

instance Prelude.NFData ListServicesResponse where
  rnf ListServicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceArns
      `Prelude.seq` Prelude.rnf httpStatus
