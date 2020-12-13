{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.ListDiscoveredResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.
--
-- The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the @limit@ parameter. The response includes a @nextToken@ string. To get the next page of results, run the request again and specify the string for the @nextToken@ parameter.
--
-- This operation returns paginated results.
module Network.AWS.Config.ListDiscoveredResources
  ( -- * Creating a request
    ListDiscoveredResources (..),
    mkListDiscoveredResources,

    -- ** Request lenses
    ldrResourceIds,
    ldrResourceType,
    ldrResourceName,
    ldrIncludeDeletedResources,
    ldrNextToken,
    ldrLimit,

    -- * Destructuring the response
    ListDiscoveredResourcesResponse (..),
    mkListDiscoveredResourcesResponse,

    -- ** Response lenses
    ldrrsNextToken,
    ldrrsResourceIdentifiers,
    ldrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkListDiscoveredResources' smart constructor.
data ListDiscoveredResources = ListDiscoveredResources'
  { -- | The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
    resourceIds :: Lude.Maybe [Lude.Text],
    -- | The type of resources that you want AWS Config to list in the response.
    resourceType :: ResourceType,
    -- | The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
    resourceName :: Lude.Maybe Lude.Text,
    -- | Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
    includeDeletedResources :: Lude.Maybe Lude.Bool,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDiscoveredResources' with the minimum fields required to make a request.
--
-- * 'resourceIds' - The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
-- * 'resourceType' - The type of resources that you want AWS Config to list in the response.
-- * 'resourceName' - The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
-- * 'includeDeletedResources' - Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
-- * 'nextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
-- * 'limit' - The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
mkListDiscoveredResources ::
  -- | 'resourceType'
  ResourceType ->
  ListDiscoveredResources
mkListDiscoveredResources pResourceType_ =
  ListDiscoveredResources'
    { resourceIds = Lude.Nothing,
      resourceType = pResourceType_,
      resourceName = Lude.Nothing,
      includeDeletedResources = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The IDs of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceIds :: Lens.Lens' ListDiscoveredResources (Lude.Maybe [Lude.Text])
ldrResourceIds = Lens.lens (resourceIds :: ListDiscoveredResources -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceIds = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrResourceIds "Use generic-lens or generic-optics with 'resourceIds' instead." #-}

-- | The type of resources that you want AWS Config to list in the response.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceType :: Lens.Lens' ListDiscoveredResources ResourceType
ldrResourceType = Lens.lens (resourceType :: ListDiscoveredResources -> ResourceType) (\s a -> s {resourceType = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The custom name of only those resources that you want AWS Config to list in the response. If you do not specify this parameter, AWS Config lists all resources of the specified type that it has discovered.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrResourceName :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Text)
ldrResourceName = Lens.lens (resourceName :: ListDiscoveredResources -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | Specifies whether AWS Config includes deleted resources in the results. By default, deleted resources are not included.
--
-- /Note:/ Consider using 'includeDeletedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrIncludeDeletedResources :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Bool)
ldrIncludeDeletedResources = Lens.lens (includeDeletedResources :: ListDiscoveredResources -> Lude.Maybe Lude.Bool) (\s a -> s {includeDeletedResources = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrIncludeDeletedResources "Use generic-lens or generic-optics with 'includeDeletedResources' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrNextToken :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Text)
ldrNextToken = Lens.lens (nextToken :: ListDiscoveredResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of resource identifiers returned on each page. The default is 100. You cannot specify a number greater than 100. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrLimit :: Lens.Lens' ListDiscoveredResources (Lude.Maybe Lude.Natural)
ldrLimit = Lens.lens (limit :: ListDiscoveredResources -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListDiscoveredResources)
{-# DEPRECATED ldrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListDiscoveredResources where
  page rq rs
    | Page.stop (rs Lens.^. ldrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrrsResourceIdentifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldrNextToken Lens..~ rs Lens.^. ldrrsNextToken

instance Lude.AWSRequest ListDiscoveredResources where
  type Rs ListDiscoveredResources = ListDiscoveredResourcesResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDiscoveredResourcesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "resourceIdentifiers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDiscoveredResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StarlingDoveService.ListDiscoveredResources" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDiscoveredResources where
  toJSON ListDiscoveredResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("resourceIds" Lude..=) Lude.<$> resourceIds,
            Lude.Just ("resourceType" Lude..= resourceType),
            ("resourceName" Lude..=) Lude.<$> resourceName,
            ("includeDeletedResources" Lude..=)
              Lude.<$> includeDeletedResources,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListDiscoveredResources where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDiscoveredResources where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkListDiscoveredResourcesResponse' smart constructor.
data ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse'
  { -- | The string that you use in a subsequent request to get the next page of results in a paginated response.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
    resourceIdentifiers :: Lude.Maybe [ResourceIdentifier],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDiscoveredResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
-- * 'resourceIdentifiers' - The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
-- * 'responseStatus' - The response status code.
mkListDiscoveredResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDiscoveredResourcesResponse
mkListDiscoveredResourcesResponse pResponseStatus_ =
  ListDiscoveredResourcesResponse'
    { nextToken = Lude.Nothing,
      resourceIdentifiers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDiscoveredResourcesResponse (Lude.Maybe Lude.Text)
ldrrsNextToken = Lens.lens (nextToken :: ListDiscoveredResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.
--
-- /Note:/ Consider using 'resourceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResourceIdentifiers :: Lens.Lens' ListDiscoveredResourcesResponse (Lude.Maybe [ResourceIdentifier])
ldrrsResourceIdentifiers = Lens.lens (resourceIdentifiers :: ListDiscoveredResourcesResponse -> Lude.Maybe [ResourceIdentifier]) (\s a -> s {resourceIdentifiers = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsResourceIdentifiers "Use generic-lens or generic-optics with 'resourceIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDiscoveredResourcesResponse Lude.Int
ldrrsResponseStatus = Lens.lens (responseStatus :: ListDiscoveredResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDiscoveredResourcesResponse)
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
