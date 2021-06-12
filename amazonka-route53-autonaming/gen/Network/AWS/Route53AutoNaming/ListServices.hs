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
-- Module      : Network.AWS.Route53AutoNaming.ListServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information for all the services that are associated with
-- one or more specified namespaces.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListServices
  ( -- * Creating a Request
    ListServices (..),
    newListServices,

    -- * Request Lenses
    listServices_nextToken,
    listServices_maxResults,
    listServices_filters,

    -- * Destructuring the Response
    ListServicesResponse (..),
    newListServicesResponse,

    -- * Response Lenses
    listServicesResponse_nextToken,
    listServicesResponse_services,
    listServicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newListServices' smart constructor.
data ListServices = ListServices'
  { -- | For the first @ListServices@ request, omit this value.
    --
    -- If the response contains @NextToken@, submit another @ListServices@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ services and then filters them based on
    -- the specified criteria. It\'s possible that no services in the first
    -- @MaxResults@ services matched the specified criteria but that subsequent
    -- groups of @MaxResults@ services do contain services that match the
    -- criteria.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of services that you want AWS Cloud Map to return in
    -- the response to a @ListServices@ request. If you don\'t specify a value
    -- for @MaxResults@, AWS Cloud Map returns up to 100 services.
    maxResults :: Core.Maybe Core.Natural,
    -- | A complex type that contains specifications for the namespaces that you
    -- want to list services for.
    --
    -- If you specify more than one filter, an operation must match all filters
    -- to be returned by @ListServices@.
    filters :: Core.Maybe [ServiceFilter]
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
-- 'nextToken', 'listServices_nextToken' - For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ services and then filters them based on
-- the specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
--
-- 'maxResults', 'listServices_maxResults' - The maximum number of services that you want AWS Cloud Map to return in
-- the response to a @ListServices@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 services.
--
-- 'filters', 'listServices_filters' - A complex type that contains specifications for the namespaces that you
-- want to list services for.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListServices@.
newListServices ::
  ListServices
newListServices =
  ListServices'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ services and then filters them based on
-- the specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
listServices_nextToken :: Lens.Lens' ListServices (Core.Maybe Core.Text)
listServices_nextToken = Lens.lens (\ListServices' {nextToken} -> nextToken) (\s@ListServices' {} a -> s {nextToken = a} :: ListServices)

-- | The maximum number of services that you want AWS Cloud Map to return in
-- the response to a @ListServices@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 services.
listServices_maxResults :: Lens.Lens' ListServices (Core.Maybe Core.Natural)
listServices_maxResults = Lens.lens (\ListServices' {maxResults} -> maxResults) (\s@ListServices' {} a -> s {maxResults = a} :: ListServices)

-- | A complex type that contains specifications for the namespaces that you
-- want to list services for.
--
-- If you specify more than one filter, an operation must match all filters
-- to be returned by @ListServices@.
listServices_filters :: Lens.Lens' ListServices (Core.Maybe [ServiceFilter])
listServices_filters = Lens.lens (\ListServices' {filters} -> filters) (\s@ListServices' {} a -> s {filters = a} :: ListServices) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listServicesResponse_services Core.. Lens._Just
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Services" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListServices

instance Core.NFData ListServices

instance Core.ToHeaders ListServices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListServices" ::
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
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListServices where
  toPath = Core.const "/"

instance Core.ToQuery ListServices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | If the response contains @NextToken@, submit another @ListServices@
    -- request to get the next group of results. Specify the value of
    -- @NextToken@ from the previous response in the next request.
    --
    -- AWS Cloud Map gets @MaxResults@ services and then filters them based on
    -- the specified criteria. It\'s possible that no services in the first
    -- @MaxResults@ services matched the specified criteria but that subsequent
    -- groups of @MaxResults@ services do contain services that match the
    -- criteria.
    nextToken :: Core.Maybe Core.Text,
    -- | An array that contains one @ServiceSummary@ object for each service that
    -- matches the specified filter criteria.
    services :: Core.Maybe [ServiceSummary],
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
-- 'nextToken', 'listServicesResponse_nextToken' - If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ services and then filters them based on
-- the specified criteria. It\'s possible that no services in the first
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
  Core.Int ->
  ListServicesResponse
newListServicesResponse pHttpStatus_ =
  ListServicesResponse'
    { nextToken = Core.Nothing,
      services = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response contains @NextToken@, submit another @ListServices@
-- request to get the next group of results. Specify the value of
-- @NextToken@ from the previous response in the next request.
--
-- AWS Cloud Map gets @MaxResults@ services and then filters them based on
-- the specified criteria. It\'s possible that no services in the first
-- @MaxResults@ services matched the specified criteria but that subsequent
-- groups of @MaxResults@ services do contain services that match the
-- criteria.
listServicesResponse_nextToken :: Lens.Lens' ListServicesResponse (Core.Maybe Core.Text)
listServicesResponse_nextToken = Lens.lens (\ListServicesResponse' {nextToken} -> nextToken) (\s@ListServicesResponse' {} a -> s {nextToken = a} :: ListServicesResponse)

-- | An array that contains one @ServiceSummary@ object for each service that
-- matches the specified filter criteria.
listServicesResponse_services :: Lens.Lens' ListServicesResponse (Core.Maybe [ServiceSummary])
listServicesResponse_services = Lens.lens (\ListServicesResponse' {services} -> services) (\s@ListServicesResponse' {} a -> s {services = a} :: ListServicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listServicesResponse_httpStatus :: Lens.Lens' ListServicesResponse Core.Int
listServicesResponse_httpStatus = Lens.lens (\ListServicesResponse' {httpStatus} -> httpStatus) (\s@ListServicesResponse' {} a -> s {httpStatus = a} :: ListServicesResponse)

instance Core.NFData ListServicesResponse
