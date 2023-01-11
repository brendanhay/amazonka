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
-- Module      : Amazonka.Config.ListAggregateDiscoveredResources
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Config.ListAggregateDiscoveredResources
  ( -- * Creating a Request
    ListAggregateDiscoveredResources (..),
    newListAggregateDiscoveredResources,

    -- * Request Lenses
    listAggregateDiscoveredResources_filters,
    listAggregateDiscoveredResources_limit,
    listAggregateDiscoveredResources_nextToken,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAggregateDiscoveredResources' smart constructor.
data ListAggregateDiscoveredResources = ListAggregateDiscoveredResources'
  { -- | Filters the results based on the @ResourceFilters@ object.
    filters :: Prelude.Maybe ResourceFilters,
    -- | The maximum number of resource identifiers returned on each page. You
    -- cannot specify a number greater than 100. If you specify 0, Config uses
    -- the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text,
    -- | The type of resources that you want Config to list in the response.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAggregateDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listAggregateDiscoveredResources_filters' - Filters the results based on the @ResourceFilters@ object.
--
-- 'limit', 'listAggregateDiscoveredResources_limit' - The maximum number of resource identifiers returned on each page. You
-- cannot specify a number greater than 100. If you specify 0, Config uses
-- the default.
--
-- 'nextToken', 'listAggregateDiscoveredResources_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configurationAggregatorName', 'listAggregateDiscoveredResources_configurationAggregatorName' - The name of the configuration aggregator.
--
-- 'resourceType', 'listAggregateDiscoveredResources_resourceType' - The type of resources that you want Config to list in the response.
newListAggregateDiscoveredResources ::
  -- | 'configurationAggregatorName'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  ListAggregateDiscoveredResources
newListAggregateDiscoveredResources
  pConfigurationAggregatorName_
  pResourceType_ =
    ListAggregateDiscoveredResources'
      { filters =
          Prelude.Nothing,
        limit = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceType = pResourceType_
      }

-- | Filters the results based on the @ResourceFilters@ object.
listAggregateDiscoveredResources_filters :: Lens.Lens' ListAggregateDiscoveredResources (Prelude.Maybe ResourceFilters)
listAggregateDiscoveredResources_filters = Lens.lens (\ListAggregateDiscoveredResources' {filters} -> filters) (\s@ListAggregateDiscoveredResources' {} a -> s {filters = a} :: ListAggregateDiscoveredResources)

-- | The maximum number of resource identifiers returned on each page. You
-- cannot specify a number greater than 100. If you specify 0, Config uses
-- the default.
listAggregateDiscoveredResources_limit :: Lens.Lens' ListAggregateDiscoveredResources (Prelude.Maybe Prelude.Natural)
listAggregateDiscoveredResources_limit = Lens.lens (\ListAggregateDiscoveredResources' {limit} -> limit) (\s@ListAggregateDiscoveredResources' {} a -> s {limit = a} :: ListAggregateDiscoveredResources)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listAggregateDiscoveredResources_nextToken :: Lens.Lens' ListAggregateDiscoveredResources (Prelude.Maybe Prelude.Text)
listAggregateDiscoveredResources_nextToken = Lens.lens (\ListAggregateDiscoveredResources' {nextToken} -> nextToken) (\s@ListAggregateDiscoveredResources' {} a -> s {nextToken = a} :: ListAggregateDiscoveredResources)

-- | The name of the configuration aggregator.
listAggregateDiscoveredResources_configurationAggregatorName :: Lens.Lens' ListAggregateDiscoveredResources Prelude.Text
listAggregateDiscoveredResources_configurationAggregatorName = Lens.lens (\ListAggregateDiscoveredResources' {configurationAggregatorName} -> configurationAggregatorName) (\s@ListAggregateDiscoveredResources' {} a -> s {configurationAggregatorName = a} :: ListAggregateDiscoveredResources)

-- | The type of resources that you want Config to list in the response.
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
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAggregateDiscoveredResourcesResponse_resourceIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAggregateDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listAggregateDiscoveredResourcesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListAggregateDiscoveredResources
  where
  type
    AWSResponse ListAggregateDiscoveredResources =
      ListAggregateDiscoveredResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAggregateDiscoveredResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ResourceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAggregateDiscoveredResources
  where
  hashWithSalt
    _salt
    ListAggregateDiscoveredResources' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` configurationAggregatorName
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    ListAggregateDiscoveredResources
  where
  rnf ListAggregateDiscoveredResources' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configurationAggregatorName
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToHeaders
    ListAggregateDiscoveredResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.ListAggregateDiscoveredResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAggregateDiscoveredResources where
  toJSON ListAggregateDiscoveredResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "ConfigurationAggregatorName"
                  Data..= configurationAggregatorName
              ),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath ListAggregateDiscoveredResources where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListAggregateDiscoveredResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAggregateDiscoveredResourcesResponse' smart constructor.
data ListAggregateDiscoveredResourcesResponse = ListAggregateDiscoveredResourcesResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of @ResourceIdentifiers@ objects.
    resourceIdentifiers :: Prelude.Maybe [AggregateResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAggregateDiscoveredResourcesResponse
newListAggregateDiscoveredResourcesResponse
  pHttpStatus_ =
    ListAggregateDiscoveredResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        resourceIdentifiers =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listAggregateDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Prelude.Maybe Prelude.Text)
listAggregateDiscoveredResourcesResponse_nextToken = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListAggregateDiscoveredResourcesResponse)

-- | Returns a list of @ResourceIdentifiers@ objects.
listAggregateDiscoveredResourcesResponse_resourceIdentifiers :: Lens.Lens' ListAggregateDiscoveredResourcesResponse (Prelude.Maybe [AggregateResourceIdentifier])
listAggregateDiscoveredResourcesResponse_resourceIdentifiers = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {resourceIdentifiers = a} :: ListAggregateDiscoveredResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAggregateDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListAggregateDiscoveredResourcesResponse Prelude.Int
listAggregateDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListAggregateDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListAggregateDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListAggregateDiscoveredResourcesResponse)

instance
  Prelude.NFData
    ListAggregateDiscoveredResourcesResponse
  where
  rnf ListAggregateDiscoveredResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIdentifiers
      `Prelude.seq` Prelude.rnf httpStatus
