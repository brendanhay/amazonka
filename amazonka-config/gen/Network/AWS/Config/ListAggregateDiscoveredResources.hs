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
-- Module      : Network.AWS.Config.ListAggregateDiscoveredResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers that
-- are aggregated for a specific resource type across accounts and regions.
-- A resource identifier includes the resource type, ID, (if available) the
-- custom resource name, source account, and source region. You can narrow
-- the results to include only resources that have specific resource IDs,
-- or a resource name, or source account ID, or source region.
--
-- For example, if the input consists of accountID 12345678910 and the
-- region is us-east-1 for resource type @AWS::EC2::Instance@ then the API
-- returns all the EC2 instance identifiers of accountID 12345678910 and
-- region us-east-1.
--
-- This operation returns paginated results.
module Network.AWS.Config.ListAggregateDiscoveredResources
  ( -- * Creating a Request
    ListAggregateDiscoveredResources (..),
    newListAggregateDiscoveredResources,

    -- * Request Lenses
    listAggregateDiscoveredResources_nextToken,
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_configurationAggregatorName,
    listAggregateDiscoveredResources_resourceType,

    -- * Destructuring the Response
    ListAggregateDiscoveredResourcesResponse (..),
    newListAggregateDiscoveredResourcesResponse,

    -- * Response Lenses
    listAggregateDiscoveredResourcesResponse_nextToken,
    listAggregateDiscoveredResourcesResponse_resourceIdentifiers,
    listAggregateDiscoveredResourcesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAggregateDiscoveredResources' smart constructor.
data ListAggregateDiscoveredResources = ListAggregateDiscoveredResources'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Filters the results based on the @ResourceFilters@ object.
    filters :: Core.Maybe ResourceFilters,
    -- | The maximum number of resource identifiers returned on each page. You
    -- cannot specify a number greater than 100. If you specify 0, AWS Config
    -- uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text,
    -- | The type of resources that you want AWS Config to list in the response.
    resourceType :: ResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAggregateDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAggregateDiscoveredResources_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'filters', 'listAggregateDiscoveredResources_filters' - Filters the results based on the @ResourceFilters@ object.
--
-- 'limit', 'listAggregateDiscoveredResources_limit' - The maximum number of resource identifiers returned on each page. You
-- cannot specify a number greater than 100. If you specify 0, AWS Config
-- uses the default.
--
-- 'configurationAggregatorName', 'listAggregateDiscoveredResources_configurationAggregatorName' - The name of the configuration aggregator.
--
-- 'resourceType', 'listAggregateDiscoveredResources_resourceType' - The type of resources that you want AWS Config to list in the response.
newListAggregateDiscoveredResources ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  -- | 'resourceType'
  ResourceType ->
  ListAggregateDiscoveredResources
newListAggregateDiscoveredResources
  pConfigurationAggregatorName_
  pResourceType_ =
    ListAggregateDiscoveredResources'
      { nextToken =
          Core.Nothing,
        filters = Core.Nothing,
        limit = Core.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceType = pResourceType_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listAggregateDiscoveredResources_nextToken :: Lens.Lens' ListAggregateDiscoveredResources (Core.Maybe Core.Text)
listAggregateDiscoveredResources_nextToken = Lens.lens (\ListAggregateDiscoveredResources' {nextToken} -> nextToken) (\s@ListAggregateDiscoveredResources' {} a -> s {nextToken = a} :: ListAggregateDiscoveredResources)

-- | Filters the results based on the @ResourceFilters@ object.
listAggregateDiscoveredResources_filters :: Lens.Lens' ListAggregateDiscoveredResources (Core.Maybe ResourceFilters)
listAggregateDiscoveredResources_filters = Lens.lens (\ListAggregateDiscoveredResources' {filters} -> filters) (\s@ListAggregateDiscoveredResources' {} a -> s {filters = a} :: ListAggregateDiscoveredResources)

-- | The maximum number of resource identifiers returned on each page. You
-- cannot specify a number greater than 100. If you specify 0, AWS Config
-- uses the default.
listAggregateDiscoveredResources_limit :: Lens.Lens' ListAggregateDiscoveredResources (Core.Maybe Core.Natural)
listAggregateDiscoveredResources_limit = Lens.lens (\ListAggregateDiscoveredResources' {limit} -> limit) (\s@ListAggregateDiscoveredResources' {} a -> s {limit = a} :: ListAggregateDiscoveredResources)

-- | The name of the configuration aggregator.
listAggregateDiscoveredResources_configurationAggregatorName :: Lens.Lens' ListAggregateDiscoveredResources Core.Text
listAggregateDiscoveredResources_configurationAggregatorName = Lens.lens (\ListAggregateDiscoveredResources' {configurationAggregatorName} -> configurationAggregatorName) (\s@ListAggregateDiscoveredResources' {} a -> s {configurationAggregatorName = a} :: ListAggregateDiscoveredResources)

-- | The type of resources that you want AWS Config to list in the response.
listAggregateDiscoveredResources_resourceType :: Lens.Lens' ListAggregateDiscoveredResources ResourceType
listAggregateDiscoveredResources_resourceType = Lens.lens (\ListAggregateDiscoveredResources' {resourceType} -> resourceType) (\s@ListAggregateDiscoveredResources' {} a -> s {resourceType = a} :: ListAggregateDiscoveredResources)

instance
  Core.AWSPager
    ListAggregateDiscoveredResources
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAggregateDiscoveredResourcesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAggregateDiscoveredResourcesResponse_resourceIdentifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAggregateDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listAggregateDiscoveredResourcesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListAggregateDiscoveredResources
  where
  type
    AWSResponse ListAggregateDiscoveredResources =
      ListAggregateDiscoveredResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAggregateDiscoveredResourcesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ResourceIdentifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListAggregateDiscoveredResources

instance Core.NFData ListAggregateDiscoveredResources

instance
  Core.ToHeaders
    ListAggregateDiscoveredResources
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.ListAggregateDiscoveredResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAggregateDiscoveredResources where
  toJSON ListAggregateDiscoveredResources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Filters" Core..=) Core.<$> filters,
            ("Limit" Core..=) Core.<$> limit,
            Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath ListAggregateDiscoveredResources where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListAggregateDiscoveredResources
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAggregateDiscoveredResourcesResponse' smart constructor.
data ListAggregateDiscoveredResourcesResponse = ListAggregateDiscoveredResourcesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a list of @ResourceIdentifiers@ objects.
    resourceIdentifiers :: Core.Maybe [AggregateResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAggregateDiscoveredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAggregateDiscoveredResourcesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceIdentifiers', 'listAggregateDiscoveredResourcesResponse_resourceIdentifiers' - Returns a list of @ResourceIdentifiers@ objects.
--
-- 'httpStatus', 'listAggregateDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListAggregateDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAggregateDiscoveredResourcesResponse
newListAggregateDiscoveredResourcesResponse
  pHttpStatus_ =
    ListAggregateDiscoveredResourcesResponse'
      { nextToken =
          Core.Nothing,
        resourceIdentifiers =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listAggregateDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Core.Maybe Core.Text)
listAggregateDiscoveredResourcesResponse_nextToken = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListAggregateDiscoveredResourcesResponse)

-- | Returns a list of @ResourceIdentifiers@ objects.
listAggregateDiscoveredResourcesResponse_resourceIdentifiers :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Core.Maybe [AggregateResourceIdentifier])
listAggregateDiscoveredResourcesResponse_resourceIdentifiers = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {resourceIdentifiers = a} :: ListAggregateDiscoveredResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAggregateDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListAggregateDiscoveredResourcesResponse Core.Int
listAggregateDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListAggregateDiscoveredResourcesResponse)

instance
  Core.NFData
    ListAggregateDiscoveredResourcesResponse
