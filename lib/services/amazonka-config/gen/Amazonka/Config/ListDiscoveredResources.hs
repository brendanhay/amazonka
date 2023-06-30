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
-- Module      : Amazonka.Config.ListDiscoveredResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers for
-- the resources of that type. A resource identifier includes the resource
-- type, ID, and (if available) the custom resource name. The results
-- consist of resources that Config has discovered, including those that
-- Config is not currently recording. You can narrow the results to include
-- only resources that have specific resource IDs or a resource name.
--
-- You can specify either resource IDs or a resource name, but not both, in
-- the same request.
--
-- The response is paginated. By default, Config lists 100 resource
-- identifiers on each page. You can customize this number with the @limit@
-- parameter. The response includes a @nextToken@ string. To get the next
-- page of results, run the request again and specify the string for the
-- @nextToken@ parameter.
--
-- This operation returns paginated results.
module Amazonka.Config.ListDiscoveredResources
  ( -- * Creating a Request
    ListDiscoveredResources (..),
    newListDiscoveredResources,

    -- * Request Lenses
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_limit,
    listDiscoveredResources_nextToken,
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_resourceType,

    -- * Destructuring the Response
    ListDiscoveredResourcesResponse (..),
    newListDiscoveredResourcesResponse,

    -- * Response Lenses
    listDiscoveredResourcesResponse_nextToken,
    listDiscoveredResourcesResponse_resourceIdentifiers,
    listDiscoveredResourcesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | Specifies whether Config includes deleted resources in the results. By
    -- default, deleted resources are not included.
    includeDeletedResources :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of resource identifiers returned on each page. The
    -- default is 100. You cannot specify a number greater than 100. If you
    -- specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of only those resources that you want Config to list in the
    -- response. If you do not specify this parameter, Config lists all
    -- resources of the specified type that it has discovered.
    resourceIds :: Prelude.Maybe [Prelude.Text],
    -- | The custom name of only those resources that you want Config to list in
    -- the response. If you do not specify this parameter, Config lists all
    -- resources of the specified type that it has discovered.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resources that you want Config to list in the response.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeDeletedResources', 'listDiscoveredResources_includeDeletedResources' - Specifies whether Config includes deleted resources in the results. By
-- default, deleted resources are not included.
--
-- 'limit', 'listDiscoveredResources_limit' - The maximum number of resource identifiers returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
--
-- 'nextToken', 'listDiscoveredResources_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceIds', 'listDiscoveredResources_resourceIds' - The IDs of only those resources that you want Config to list in the
-- response. If you do not specify this parameter, Config lists all
-- resources of the specified type that it has discovered.
--
-- 'resourceName', 'listDiscoveredResources_resourceName' - The custom name of only those resources that you want Config to list in
-- the response. If you do not specify this parameter, Config lists all
-- resources of the specified type that it has discovered.
--
-- 'resourceType', 'listDiscoveredResources_resourceType' - The type of resources that you want Config to list in the response.
newListDiscoveredResources ::
  -- | 'resourceType'
  ResourceType ->
  ListDiscoveredResources
newListDiscoveredResources pResourceType_ =
  ListDiscoveredResources'
    { includeDeletedResources =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = pResourceType_
    }

-- | Specifies whether Config includes deleted resources in the results. By
-- default, deleted resources are not included.
listDiscoveredResources_includeDeletedResources :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Bool)
listDiscoveredResources_includeDeletedResources = Lens.lens (\ListDiscoveredResources' {includeDeletedResources} -> includeDeletedResources) (\s@ListDiscoveredResources' {} a -> s {includeDeletedResources = a} :: ListDiscoveredResources)

-- | The maximum number of resource identifiers returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
listDiscoveredResources_limit :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Natural)
listDiscoveredResources_limit = Lens.lens (\ListDiscoveredResources' {limit} -> limit) (\s@ListDiscoveredResources' {} a -> s {limit = a} :: ListDiscoveredResources)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The IDs of only those resources that you want Config to list in the
-- response. If you do not specify this parameter, Config lists all
-- resources of the specified type that it has discovered.
listDiscoveredResources_resourceIds :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe [Prelude.Text])
listDiscoveredResources_resourceIds = Lens.lens (\ListDiscoveredResources' {resourceIds} -> resourceIds) (\s@ListDiscoveredResources' {} a -> s {resourceIds = a} :: ListDiscoveredResources) Prelude.. Lens.mapping Lens.coerced

-- | The custom name of only those resources that you want Config to list in
-- the response. If you do not specify this parameter, Config lists all
-- resources of the specified type that it has discovered.
listDiscoveredResources_resourceName :: Lens.Lens' ListDiscoveredResources (Prelude.Maybe Prelude.Text)
listDiscoveredResources_resourceName = Lens.lens (\ListDiscoveredResources' {resourceName} -> resourceName) (\s@ListDiscoveredResources' {} a -> s {resourceName = a} :: ListDiscoveredResources)

-- | The type of resources that you want Config to list in the response.
listDiscoveredResources_resourceType :: Lens.Lens' ListDiscoveredResources ResourceType
listDiscoveredResources_resourceType = Lens.lens (\ListDiscoveredResources' {resourceType} -> resourceType) (\s@ListDiscoveredResources' {} a -> s {resourceType = a} :: ListDiscoveredResources)

instance Core.AWSPager ListDiscoveredResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_resourceIdentifiers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listDiscoveredResourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDiscoveredResources where
  type
    AWSResponse ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "resourceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoveredResources where
  hashWithSalt _salt ListDiscoveredResources' {..} =
    _salt
      `Prelude.hashWithSalt` includeDeletedResources
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListDiscoveredResources where
  rnf ListDiscoveredResources' {..} =
    Prelude.rnf includeDeletedResources
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders ListDiscoveredResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.ListDiscoveredResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("includeDeletedResources" Data..=)
              Prelude.<$> includeDeletedResources,
            ("limit" Data..=) Prelude.<$> limit,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("resourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just ("resourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath ListDiscoveredResources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDiscoveredResources where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The details that identify a resource that is discovered by Config,
    -- including the resource type, ID, and (if available) the custom resource
    -- name.
    resourceIdentifiers :: Prelude.Maybe [ResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoveredResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDiscoveredResourcesResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'resourceIdentifiers', 'listDiscoveredResourcesResponse_resourceIdentifiers' - The details that identify a resource that is discovered by Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
--
-- 'httpStatus', 'listDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      resourceIdentifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe Prelude.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | The details that identify a resource that is discovered by Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
listDiscoveredResourcesResponse_resourceIdentifiers :: Lens.Lens' ListDiscoveredResourcesResponse (Prelude.Maybe [ResourceIdentifier])
listDiscoveredResourcesResponse_resourceIdentifiers = Lens.lens (\ListDiscoveredResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@ListDiscoveredResourcesResponse' {} a -> s {resourceIdentifiers = a} :: ListDiscoveredResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Prelude.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance
  Prelude.NFData
    ListDiscoveredResourcesResponse
  where
  rnf ListDiscoveredResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIdentifiers
      `Prelude.seq` Prelude.rnf httpStatus
