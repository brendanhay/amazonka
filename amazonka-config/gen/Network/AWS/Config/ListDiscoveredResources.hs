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
-- Module      : Network.AWS.Config.ListDiscoveredResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers for
-- the resources of that type. A resource identifier includes the resource
-- type, ID, and (if available) the custom resource name. The results
-- consist of resources that AWS Config has discovered, including those
-- that AWS Config is not currently recording. You can narrow the results
-- to include only resources that have specific resource IDs or a resource
-- name.
--
-- You can specify either resource IDs or a resource name, but not both, in
-- the same request.
--
-- The response is paginated. By default, AWS Config lists 100 resource
-- identifiers on each page. You can customize this number with the @limit@
-- parameter. The response includes a @nextToken@ string. To get the next
-- page of results, run the request again and specify the string for the
-- @nextToken@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.Config.ListDiscoveredResources
  ( -- * Creating a Request
    ListDiscoveredResources (..),
    newListDiscoveredResources,

    -- * Request Lenses
    listDiscoveredResources_nextToken,
    listDiscoveredResources_resourceIds,
    listDiscoveredResources_includeDeletedResources,
    listDiscoveredResources_resourceName,
    listDiscoveredResources_limit,
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

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The IDs of only those resources that you want AWS Config to list in the
    -- response. If you do not specify this parameter, AWS Config lists all
    -- resources of the specified type that it has discovered.
    resourceIds :: Core.Maybe [Core.Text],
    -- | Specifies whether AWS Config includes deleted resources in the results.
    -- By default, deleted resources are not included.
    includeDeletedResources :: Core.Maybe Core.Bool,
    -- | The custom name of only those resources that you want AWS Config to list
    -- in the response. If you do not specify this parameter, AWS Config lists
    -- all resources of the specified type that it has discovered.
    resourceName :: Core.Maybe Core.Text,
    -- | The maximum number of resource identifiers returned on each page. The
    -- default is 100. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The type of resources that you want AWS Config to list in the response.
    resourceType :: ResourceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDiscoveredResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDiscoveredResources_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceIds', 'listDiscoveredResources_resourceIds' - The IDs of only those resources that you want AWS Config to list in the
-- response. If you do not specify this parameter, AWS Config lists all
-- resources of the specified type that it has discovered.
--
-- 'includeDeletedResources', 'listDiscoveredResources_includeDeletedResources' - Specifies whether AWS Config includes deleted resources in the results.
-- By default, deleted resources are not included.
--
-- 'resourceName', 'listDiscoveredResources_resourceName' - The custom name of only those resources that you want AWS Config to list
-- in the response. If you do not specify this parameter, AWS Config lists
-- all resources of the specified type that it has discovered.
--
-- 'limit', 'listDiscoveredResources_limit' - The maximum number of resource identifiers returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
--
-- 'resourceType', 'listDiscoveredResources_resourceType' - The type of resources that you want AWS Config to list in the response.
newListDiscoveredResources ::
  -- | 'resourceType'
  ResourceType ->
  ListDiscoveredResources
newListDiscoveredResources pResourceType_ =
  ListDiscoveredResources'
    { nextToken = Core.Nothing,
      resourceIds = Core.Nothing,
      includeDeletedResources = Core.Nothing,
      resourceName = Core.Nothing,
      limit = Core.Nothing,
      resourceType = pResourceType_
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
listDiscoveredResources_nextToken :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Text)
listDiscoveredResources_nextToken = Lens.lens (\ListDiscoveredResources' {nextToken} -> nextToken) (\s@ListDiscoveredResources' {} a -> s {nextToken = a} :: ListDiscoveredResources)

-- | The IDs of only those resources that you want AWS Config to list in the
-- response. If you do not specify this parameter, AWS Config lists all
-- resources of the specified type that it has discovered.
listDiscoveredResources_resourceIds :: Lens.Lens' ListDiscoveredResources (Core.Maybe [Core.Text])
listDiscoveredResources_resourceIds = Lens.lens (\ListDiscoveredResources' {resourceIds} -> resourceIds) (\s@ListDiscoveredResources' {} a -> s {resourceIds = a} :: ListDiscoveredResources) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether AWS Config includes deleted resources in the results.
-- By default, deleted resources are not included.
listDiscoveredResources_includeDeletedResources :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Bool)
listDiscoveredResources_includeDeletedResources = Lens.lens (\ListDiscoveredResources' {includeDeletedResources} -> includeDeletedResources) (\s@ListDiscoveredResources' {} a -> s {includeDeletedResources = a} :: ListDiscoveredResources)

-- | The custom name of only those resources that you want AWS Config to list
-- in the response. If you do not specify this parameter, AWS Config lists
-- all resources of the specified type that it has discovered.
listDiscoveredResources_resourceName :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Text)
listDiscoveredResources_resourceName = Lens.lens (\ListDiscoveredResources' {resourceName} -> resourceName) (\s@ListDiscoveredResources' {} a -> s {resourceName = a} :: ListDiscoveredResources)

-- | The maximum number of resource identifiers returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
listDiscoveredResources_limit :: Lens.Lens' ListDiscoveredResources (Core.Maybe Core.Natural)
listDiscoveredResources_limit = Lens.lens (\ListDiscoveredResources' {limit} -> limit) (\s@ListDiscoveredResources' {} a -> s {limit = a} :: ListDiscoveredResources)

-- | The type of resources that you want AWS Config to list in the response.
listDiscoveredResources_resourceType :: Lens.Lens' ListDiscoveredResources ResourceType
listDiscoveredResources_resourceType = Lens.lens (\ListDiscoveredResources' {resourceType} -> resourceType) (\s@ListDiscoveredResources' {} a -> s {resourceType = a} :: ListDiscoveredResources)

instance Core.AWSPager ListDiscoveredResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoveredResourcesResponse_resourceIdentifiers
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDiscoveredResources_nextToken
          Lens..~ rs
          Lens.^? listDiscoveredResourcesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDiscoveredResources where
  type
    AWSResponse ListDiscoveredResources =
      ListDiscoveredResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "resourceIdentifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDiscoveredResources

instance Core.NFData ListDiscoveredResources

instance Core.ToHeaders ListDiscoveredResources where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.ListDiscoveredResources" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("resourceIds" Core..=) Core.<$> resourceIds,
            ("includeDeletedResources" Core..=)
              Core.<$> includeDeletedResources,
            ("resourceName" Core..=) Core.<$> resourceName,
            ("limit" Core..=) Core.<$> limit,
            Core.Just ("resourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath ListDiscoveredResources where
  toPath = Core.const "/"

instance Core.ToQuery ListDiscoveredResources where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The details that identify a resource that is discovered by AWS Config,
    -- including the resource type, ID, and (if available) the custom resource
    -- name.
    resourceIdentifiers :: Core.Maybe [ResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'resourceIdentifiers', 'listDiscoveredResourcesResponse_resourceIdentifiers' - The details that identify a resource that is discovered by AWS Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
--
-- 'httpStatus', 'listDiscoveredResourcesResponse_httpStatus' - The response's http status code.
newListDiscoveredResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDiscoveredResourcesResponse
newListDiscoveredResourcesResponse pHttpStatus_ =
  ListDiscoveredResourcesResponse'
    { nextToken =
        Core.Nothing,
      resourceIdentifiers = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
listDiscoveredResourcesResponse_nextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe Core.Text)
listDiscoveredResourcesResponse_nextToken = Lens.lens (\ListDiscoveredResourcesResponse' {nextToken} -> nextToken) (\s@ListDiscoveredResourcesResponse' {} a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)

-- | The details that identify a resource that is discovered by AWS Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
listDiscoveredResourcesResponse_resourceIdentifiers :: Lens.Lens' ListDiscoveredResourcesResponse (Core.Maybe [ResourceIdentifier])
listDiscoveredResourcesResponse_resourceIdentifiers = Lens.lens (\ListDiscoveredResourcesResponse' {resourceIdentifiers} -> resourceIdentifiers) (\s@ListDiscoveredResourcesResponse' {} a -> s {resourceIdentifiers = a} :: ListDiscoveredResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDiscoveredResourcesResponse_httpStatus :: Lens.Lens' ListDiscoveredResourcesResponse Core.Int
listDiscoveredResourcesResponse_httpStatus = Lens.lens (\ListDiscoveredResourcesResponse' {httpStatus} -> httpStatus) (\s@ListDiscoveredResourcesResponse' {} a -> s {httpStatus = a} :: ListDiscoveredResourcesResponse)

instance Core.NFData ListDiscoveredResourcesResponse
