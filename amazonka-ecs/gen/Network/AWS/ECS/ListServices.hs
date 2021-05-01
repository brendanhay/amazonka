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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of service results returned by @ListServices@ in
    -- paginated output. When this parameter is used, @ListServices@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @ListServices@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListServices@ returns up to 10 results and
    -- a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The launch type for the services to list.
    launchType :: Prelude.Maybe LaunchType,
    -- | The scheduling strategy for services to list.
    schedulingStrategy :: Prelude.Maybe SchedulingStrategy,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the services to list. If you do not specify a cluster, the default
    -- cluster is assumed.
    cluster :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      launchType = Prelude.Nothing,
      schedulingStrategy = Prelude.Nothing,
      cluster = Prelude.Nothing
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

-- | The maximum number of service results returned by @ListServices@ in
-- paginated output. When this parameter is used, @ListServices@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListServices@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListServices@ returns up to 10 results and
-- a @nextToken@ value if applicable.
listServices_maxResults :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Int)
listServices_maxResults = Lens.lens (\ListServices' {maxResults} -> maxResults) (\s@ListServices' {} a -> s {maxResults = a} :: ListServices)

-- | The launch type for the services to list.
listServices_launchType :: Lens.Lens' ListServices (Prelude.Maybe LaunchType)
listServices_launchType = Lens.lens (\ListServices' {launchType} -> launchType) (\s@ListServices' {} a -> s {launchType = a} :: ListServices)

-- | The scheduling strategy for services to list.
listServices_schedulingStrategy :: Lens.Lens' ListServices (Prelude.Maybe SchedulingStrategy)
listServices_schedulingStrategy = Lens.lens (\ListServices' {schedulingStrategy} -> schedulingStrategy) (\s@ListServices' {} a -> s {schedulingStrategy = a} :: ListServices)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the services to list. If you do not specify a cluster, the default
-- cluster is assumed.
listServices_cluster :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Text)
listServices_cluster = Lens.lens (\ListServices' {cluster} -> cluster) (\s@ListServices' {} a -> s {cluster = a} :: ListServices)

instance Pager.AWSPager ListServices where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listServicesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listServicesResponse_serviceArns
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listServices_nextToken
          Lens..~ rs
          Lens.^? listServicesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListServices where
  type Rs ListServices = ListServicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "serviceArns"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServices

instance Prelude.NFData ListServices

instance Prelude.ToHeaders ListServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.ListServices" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListServices where
  toJSON ListServices' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("launchType" Prelude..=) Prelude.<$> launchType,
            ("schedulingStrategy" Prelude..=)
              Prelude.<$> schedulingStrategy,
            ("cluster" Prelude..=) Prelude.<$> cluster
          ]
      )

instance Prelude.ToPath ListServices where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | The @nextToken@ value to include in a future @ListServices@ request.
    -- When the results of a @ListServices@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of full ARN entries for each service associated with the
    -- specified cluster.
    serviceArns :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The list of full ARN entries for each service associated with the
-- specified cluster.
listServicesResponse_serviceArns :: Lens.Lens' ListServicesResponse (Prelude.Maybe [Prelude.Text])
listServicesResponse_serviceArns = Lens.lens (\ListServicesResponse' {serviceArns} -> serviceArns) (\s@ListServicesResponse' {} a -> s {serviceArns = a} :: ListServicesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listServicesResponse_httpStatus :: Lens.Lens' ListServicesResponse Prelude.Int
listServicesResponse_httpStatus = Lens.lens (\ListServicesResponse' {httpStatus} -> httpStatus) (\s@ListServicesResponse' {} a -> s {httpStatus = a} :: ListServicesResponse)

instance Prelude.NFData ListServicesResponse
