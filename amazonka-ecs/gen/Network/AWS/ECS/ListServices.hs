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
-- Module      : Network.AWS.ECS.ListServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the services that are running in a specified cluster.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListServices
  ( -- * Creating a Request
    ListServices (..),
    newListServices,

    -- * Request Lenses
    listServices_nextToken,
    listServices_maxResults,
    listServices_launchType,
    listServices_schedulingStrategy,
    listServices_cluster,

    -- * Destructuring the Response
    ListServicesResponse (..),
    newListServicesResponse,

    -- * Response Lenses
    listServicesResponse_nextToken,
    listServicesResponse_serviceArns,
    listServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of service results returned by @ListServices@ in
    -- paginated output. When this parameter is used, @ListServices@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @ListServices@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListServices@ returns up to 10 results and
    -- a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | The launch type for the services to list.
    launchType :: Core.Maybe LaunchType,
    -- | The scheduling strategy for services to list.
    schedulingStrategy :: Core.Maybe SchedulingStrategy,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the services to list. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listServices_maxResults' - The maximum number of service results returned by @ListServices@ in
-- paginated output. When this parameter is used, @ListServices@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListServices@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListServices@ returns up to 10 results and
-- a @nextToken@ value if applicable.
--
-- 'launchType', 'listServices_launchType' - The launch type for the services to list.
--
-- 'schedulingStrategy', 'listServices_schedulingStrategy' - The scheduling strategy for services to list.
--
-- 'cluster', 'listServices_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the services to list. If you do not specify a cluster, the default
-- cluster is assumed.
newListServices ::
  ListServices
newListServices =
  ListServices'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      launchType = Core.Nothing,
      schedulingStrategy = Core.Nothing,
      cluster = Core.Nothing
    }

-- | The @nextToken@ value returned from a @ListServices@ request indicating
-- that more results are available to fulfill the request and further calls
-- will be needed. If @maxResults@ was provided, it is possible the number
-- of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listServices_nextToken :: Lens.Lens' ListServices (Core.Maybe Core.Text)
listServices_nextToken = Lens.lens (\ListServices' {nextToken} -> nextToken) (\s@ListServices' {} a -> s {nextToken = a} :: ListServices)

-- | The maximum number of service results returned by @ListServices@ in
-- paginated output. When this parameter is used, @ListServices@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListServices@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListServices@ returns up to 10 results and
-- a @nextToken@ value if applicable.
listServices_maxResults :: Lens.Lens' ListServices (Core.Maybe Core.Int)
listServices_maxResults = Lens.lens (\ListServices' {maxResults} -> maxResults) (\s@ListServices' {} a -> s {maxResults = a} :: ListServices)

-- | The launch type for the services to list.
listServices_launchType :: Lens.Lens' ListServices (Core.Maybe LaunchType)
listServices_launchType = Lens.lens (\ListServices' {launchType} -> launchType) (\s@ListServices' {} a -> s {launchType = a} :: ListServices)

-- | The scheduling strategy for services to list.
listServices_schedulingStrategy :: Lens.Lens' ListServices (Core.Maybe SchedulingStrategy)
listServices_schedulingStrategy = Lens.lens (\ListServices' {schedulingStrategy} -> schedulingStrategy) (\s@ListServices' {} a -> s {schedulingStrategy = a} :: ListServices)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the services to list. If you do not specify a cluster, the default
-- cluster is assumed.
listServices_cluster :: Lens.Lens' ListServices (Core.Maybe Core.Text)
listServices_cluster = Lens.lens (\ListServices' {cluster} -> cluster) (\s@ListServices' {} a -> s {cluster = a} :: ListServices)

instance Core.AWSPager ListServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_serviceArns Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listServices_nextToken
          Lens..~ rs
          Lens.^? listServicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListServices where
  type AWSResponse ListServices = ListServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "serviceArns" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListServices

instance Core.NFData ListServices

instance Core.ToHeaders ListServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListServices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListServices where
  toJSON ListServices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("launchType" Core..=) Core.<$> launchType,
            ("schedulingStrategy" Core..=)
              Core.<$> schedulingStrategy,
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.ToPath ListServices where
  toPath = Core.const "/"

instance Core.ToQuery ListServices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | The @nextToken@ value to include in a future @ListServices@ request.
    -- When the results of a @ListServices@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of full ARN entries for each service associated with the
    -- specified cluster.
    serviceArns :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'serviceArns', 'listServicesResponse_serviceArns' - The list of full ARN entries for each service associated with the
-- specified cluster.
--
-- 'httpStatus', 'listServicesResponse_httpStatus' - The response's http status code.
newListServicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListServicesResponse
newListServicesResponse pHttpStatus_ =
  ListServicesResponse'
    { nextToken = Core.Nothing,
      serviceArns = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListServices@ request.
-- When the results of a @ListServices@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listServicesResponse_nextToken :: Lens.Lens' ListServicesResponse (Core.Maybe Core.Text)
listServicesResponse_nextToken = Lens.lens (\ListServicesResponse' {nextToken} -> nextToken) (\s@ListServicesResponse' {} a -> s {nextToken = a} :: ListServicesResponse)

-- | The list of full ARN entries for each service associated with the
-- specified cluster.
listServicesResponse_serviceArns :: Lens.Lens' ListServicesResponse (Core.Maybe [Core.Text])
listServicesResponse_serviceArns = Lens.lens (\ListServicesResponse' {serviceArns} -> serviceArns) (\s@ListServicesResponse' {} a -> s {serviceArns = a} :: ListServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listServicesResponse_httpStatus :: Lens.Lens' ListServicesResponse Core.Int
listServicesResponse_httpStatus = Lens.lens (\ListServicesResponse' {httpStatus} -> httpStatus) (\s@ListServicesResponse' {} a -> s {httpStatus = a} :: ListServicesResponse)

instance Core.NFData ListServicesResponse
