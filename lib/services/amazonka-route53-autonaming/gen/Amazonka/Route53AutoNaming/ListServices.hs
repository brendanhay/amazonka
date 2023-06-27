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
-- Module      : Amazonka.Route53AutoNaming.ListServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information for all the services that are associated with
-- one or more specified namespaces.
--
-- This operation returns paginated results.
module Amazonka.Route53AutoNaming.ListServices
  ( -- * Creating a Request
    ListServices (..),
    newListServices,

    -- * Request Lenses
    listServices_filters,
    listServices_maxResults,
    listServices_nextToken,

    -- * Destructuring the Response
    ListServicesResponse (..),
    newListServicesResponse,

    -- * Response Lenses
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newListServices' smart constructor.
data ListServices = ListServices'
  { -- | A complex type that contains specifications for the namespaces that you
    -- want to list services for.
    --
    -- If you specify more than one filter, an operation must match all filters
    -- to be returned by @ListServices@.
    filters :: Prelude.Maybe [ServiceFilter],
    -- | The maximum number of services that you want Cloud Map to return in the
    -- response to a @ListServices@ request. If you don\'t specify a value for
    -- @MaxResults@, Cloud Map returns up to 100 services.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListServices@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListServices@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ services and then filters them based on the
    -- specified criteria. It\'s possible that no services in the first
    -- @MaxResults@ services matched the specified criteria but that subsequent
    -- groups of @MaxResults@ services do contain services that match the
    -- criteria.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'filters', 'listServices_filters' - A complex type that contains specifications for the namespaces that you
-- want to list services for.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListServices@.
--
-- 'maxResults', 'listServices_maxResults' - The maximum number of services that you want Cloud Map to return in the
-- response to a @ListServices@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 services.
--
-- 'nextToken', 'listServices_nextToken' - For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ services and then filters them based on the
-- specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
newListServices ::
  ListServices
newListServices =
  ListServices'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A complex type that contains specifications for the namespaces that you
-- want to list services for.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListServices@.
listServices_filters :: Lens.Lens' ListServices (Prelude.Maybe [ServiceFilter])
listServices_filters = Lens.lens (\ListServices' {filters} -> filters) (\s@ListServices' {} a -> s {filters = a} :: ListServices) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of services that you want Cloud Map to return in the
-- response to a @ListServices@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 services.
listServices_maxResults :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Natural)
listServices_maxResults = Lens.lens (\ListServices' {maxResults} -> maxResults) (\s@ListServices' {} a -> s {maxResults = a} :: ListServices)

-- | For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ services and then filters them based on the
-- specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
listServices_nextToken :: Lens.Lens' ListServices (Prelude.Maybe Prelude.Text)
listServices_nextToken = Lens.lens (\ListServices' {nextToken} -> nextToken) (\s@ListServices' {} a -> s {nextToken = a} :: ListServices)

instance Core.AWSPager ListServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_services
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listServices_nextToken
          Lens..~ rs
          Lens.^? listServicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListServices where
  type AWSResponse ListServices = ListServicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Services" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServices where
  hashWithSalt _salt ListServices' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListServices where
  rnf ListServices' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.ListServices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServices where
  toJSON ListServices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListServices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | If the response contains @NextToken@, submit another @ListServices@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- Cloud Map gets @MaxResults@ services and then filters them based on the
    -- specified criteria. It\'s possible that no services in the first
    -- @MaxResults@ services matched the specified criteria but that subsequent
    -- groups of @MaxResults@ services do contain services that match the
    -- criteria.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that contains one @ServiceSummary@ object for each service that
    -- matches the specified filter criteria.
    services :: Prelude.Maybe [ServiceSummary],
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
-- 'nextToken', 'listServicesResponse_nextToken' - If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ services and then filters them based on the
-- specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
--
-- 'services', 'listServicesResponse_services' - An array that contains one @ServiceSummary@ object for each service that
-- matches the specified filter criteria.
--
-- 'httpStatus', 'listServicesResponse_httpStatus' - The response's http status code.
newListServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServicesResponse
newListServicesResponse pHttpStatus_ =
  ListServicesResponse'
    { nextToken = Prelude.Nothing,
      services = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- Cloud Map gets @MaxResults@ services and then filters them based on the
-- specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
listServicesResponse_nextToken :: Lens.Lens' ListServicesResponse (Prelude.Maybe Prelude.Text)
listServicesResponse_nextToken = Lens.lens (\ListServicesResponse' {nextToken} -> nextToken) (\s@ListServicesResponse' {} a -> s {nextToken = a} :: ListServicesResponse)

-- | An array that contains one @ServiceSummary@ object for each service that
-- matches the specified filter criteria.
listServicesResponse_services :: Lens.Lens' ListServicesResponse (Prelude.Maybe [ServiceSummary])
listServicesResponse_services = Lens.lens (\ListServicesResponse' {services} -> services) (\s@ListServicesResponse' {} a -> s {services = a} :: ListServicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServicesResponse_httpStatus :: Lens.Lens' ListServicesResponse Prelude.Int
listServicesResponse_httpStatus = Lens.lens (\ListServicesResponse' {httpStatus} -> httpStatus) (\s@ListServicesResponse' {} a -> s {httpStatus = a} :: ListServicesResponse)

instance Prelude.NFData ListServicesResponse where
  rnf ListServicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf httpStatus
