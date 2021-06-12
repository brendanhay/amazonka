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
-- Module      : Network.AWS.Discovery.ListConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration items as specified by the value passed
-- to the required parameter @configurationType@. Optional filtering may be
-- applied to refine search results.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.ListConfigurations
  ( -- * Creating a Request
    ListConfigurations (..),
    newListConfigurations,

    -- * Request Lenses
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurations_orderBy,
    listConfigurations_filters,
    listConfigurations_configurationType,

    -- * Destructuring the Response
    ListConfigurationsResponse (..),
    newListConfigurationsResponse,

    -- * Response Lenses
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { -- | Token to retrieve the next set of results. For example, if a previous
    -- call to ListConfigurations returned 100 items, but you set
    -- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
    -- results along with a token. Use that token in this query to get the next
    -- set of 10.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return. The maximum value is 100.
    maxResults :: Core.Maybe Core.Int,
    -- | Certain filter criteria return output that can be sorted in ascending or
    -- descending order. For a list of output characteristics for each filter,
    -- see
    -- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
    -- in the /AWS Application Discovery Service User Guide/.
    orderBy :: Core.Maybe [OrderByElement],
    -- | You can filter the request using various logical operators and a
    -- /key/-/value/ format. For example:
    --
    -- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
    --
    -- For a complete list of filter options and guidance about using them with
    -- this action, see
    -- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
    -- in the /AWS Application Discovery Service User Guide/.
    filters :: Core.Maybe [Filter],
    -- | A valid configuration identified by Application Discovery Service.
    configurationType :: ConfigurationItemType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurations_nextToken' - Token to retrieve the next set of results. For example, if a previous
-- call to ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
--
-- 'maxResults', 'listConfigurations_maxResults' - The total number of items to return. The maximum value is 100.
--
-- 'orderBy', 'listConfigurations_orderBy' - Certain filter criteria return output that can be sorted in ascending or
-- descending order. For a list of output characteristics for each filter,
-- see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
--
-- 'filters', 'listConfigurations_filters' - You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
--
-- For a complete list of filter options and guidance about using them with
-- this action, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
--
-- 'configurationType', 'listConfigurations_configurationType' - A valid configuration identified by Application Discovery Service.
newListConfigurations ::
  -- | 'configurationType'
  ConfigurationItemType ->
  ListConfigurations
newListConfigurations pConfigurationType_ =
  ListConfigurations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      orderBy = Core.Nothing,
      filters = Core.Nothing,
      configurationType = pConfigurationType_
    }

-- | Token to retrieve the next set of results. For example, if a previous
-- call to ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with a token. Use that token in this query to get the next
-- set of 10.
listConfigurations_nextToken :: Lens.Lens' ListConfigurations (Core.Maybe Core.Text)
listConfigurations_nextToken = Lens.lens (\ListConfigurations' {nextToken} -> nextToken) (\s@ListConfigurations' {} a -> s {nextToken = a} :: ListConfigurations)

-- | The total number of items to return. The maximum value is 100.
listConfigurations_maxResults :: Lens.Lens' ListConfigurations (Core.Maybe Core.Int)
listConfigurations_maxResults = Lens.lens (\ListConfigurations' {maxResults} -> maxResults) (\s@ListConfigurations' {} a -> s {maxResults = a} :: ListConfigurations)

-- | Certain filter criteria return output that can be sorted in ascending or
-- descending order. For a list of output characteristics for each filter,
-- see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
listConfigurations_orderBy :: Lens.Lens' ListConfigurations (Core.Maybe [OrderByElement])
listConfigurations_orderBy = Lens.lens (\ListConfigurations' {orderBy} -> orderBy) (\s@ListConfigurations' {} a -> s {orderBy = a} :: ListConfigurations) Core.. Lens.mapping Lens._Coerce

-- | You can filter the request using various logical operators and a
-- /key/-/value/ format. For example:
--
-- @{\"key\": \"serverType\", \"value\": \"webServer\"}@
--
-- For a complete list of filter options and guidance about using them with
-- this action, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#ListConfigurations Using the ListConfigurations Action>
-- in the /AWS Application Discovery Service User Guide/.
listConfigurations_filters :: Lens.Lens' ListConfigurations (Core.Maybe [Filter])
listConfigurations_filters = Lens.lens (\ListConfigurations' {filters} -> filters) (\s@ListConfigurations' {} a -> s {filters = a} :: ListConfigurations) Core.. Lens.mapping Lens._Coerce

-- | A valid configuration identified by Application Discovery Service.
listConfigurations_configurationType :: Lens.Lens' ListConfigurations ConfigurationItemType
listConfigurations_configurationType = Lens.lens (\ListConfigurations' {configurationType} -> configurationType) (\s@ListConfigurations' {} a -> s {configurationType = a} :: ListConfigurations)

instance Core.AWSPager ListConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listConfigurationsResponse_configurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listConfigurations_nextToken
          Lens..~ rs
          Lens.^? listConfigurationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListConfigurations where
  type
    AWSResponse ListConfigurations =
      ListConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConfigurationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "configurations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListConfigurations

instance Core.NFData ListConfigurations

instance Core.ToHeaders ListConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.ListConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListConfigurations where
  toJSON ListConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("orderBy" Core..=) Core.<$> orderBy,
            ("filters" Core..=) Core.<$> filters,
            Core.Just
              ("configurationType" Core..= configurationType)
          ]
      )

instance Core.ToPath ListConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery ListConfigurations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { -- | Token to retrieve the next set of results. For example, if your call to
    -- ListConfigurations returned 100 items, but you set
    -- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
    -- results along with this token. Use this token in the next query to
    -- retrieve the next set of 10.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns configuration details, including the configuration ID, attribute
    -- names, and attribute values.
    configurations :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConfigurationsResponse_nextToken' - Token to retrieve the next set of results. For example, if your call to
-- ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
--
-- 'configurations', 'listConfigurationsResponse_configurations' - Returns configuration details, including the configuration ID, attribute
-- names, and attribute values.
--
-- 'httpStatus', 'listConfigurationsResponse_httpStatus' - The response's http status code.
newListConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListConfigurationsResponse
newListConfigurationsResponse pHttpStatus_ =
  ListConfigurationsResponse'
    { nextToken =
        Core.Nothing,
      configurations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token to retrieve the next set of results. For example, if your call to
-- ListConfigurations returned 100 items, but you set
-- @ListConfigurationsRequest$maxResults@ to 10, you received a set of 10
-- results along with this token. Use this token in the next query to
-- retrieve the next set of 10.
listConfigurationsResponse_nextToken :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Core.Text)
listConfigurationsResponse_nextToken = Lens.lens (\ListConfigurationsResponse' {nextToken} -> nextToken) (\s@ListConfigurationsResponse' {} a -> s {nextToken = a} :: ListConfigurationsResponse)

-- | Returns configuration details, including the configuration ID, attribute
-- names, and attribute values.
listConfigurationsResponse_configurations :: Lens.Lens' ListConfigurationsResponse (Core.Maybe [Core.HashMap Core.Text Core.Text])
listConfigurationsResponse_configurations = Lens.lens (\ListConfigurationsResponse' {configurations} -> configurations) (\s@ListConfigurationsResponse' {} a -> s {configurations = a} :: ListConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listConfigurationsResponse_httpStatus :: Lens.Lens' ListConfigurationsResponse Core.Int
listConfigurationsResponse_httpStatus = Lens.lens (\ListConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListConfigurationsResponse' {} a -> s {httpStatus = a} :: ListConfigurationsResponse)

instance Core.NFData ListConfigurationsResponse
