{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstanceStorageConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of storage configs for the identified instance and resource type.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceStorageConfigs
  ( -- * Creating a request
    ListInstanceStorageConfigs (..),
    mkListInstanceStorageConfigs,

    -- ** Request lenses
    liscInstanceId,
    liscResourceType,
    liscMaxResults,
    liscNextToken,

    -- * Destructuring the response
    ListInstanceStorageConfigsResponse (..),
    mkListInstanceStorageConfigsResponse,

    -- ** Response lenses
    liscrrsNextToken,
    liscrrsStorageConfigs,
    liscrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceStorageConfigs' smart constructor.
data ListInstanceStorageConfigs = ListInstanceStorageConfigs'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | A valid resource type.
    resourceType :: Types.InstanceStorageResourceType,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceStorageConfigs' value with any optional fields omitted.
mkListInstanceStorageConfigs ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'resourceType'
  Types.InstanceStorageResourceType ->
  ListInstanceStorageConfigs
mkListInstanceStorageConfigs instanceId resourceType =
  ListInstanceStorageConfigs'
    { instanceId,
      resourceType,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscInstanceId :: Lens.Lens' ListInstanceStorageConfigs Types.InstanceId
liscInstanceId = Lens.field @"instanceId"
{-# DEPRECATED liscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A valid resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscResourceType :: Lens.Lens' ListInstanceStorageConfigs Types.InstanceStorageResourceType
liscResourceType = Lens.field @"resourceType"
{-# DEPRECATED liscResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscMaxResults :: Lens.Lens' ListInstanceStorageConfigs (Core.Maybe Core.Natural)
liscMaxResults = Lens.field @"maxResults"
{-# DEPRECATED liscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscNextToken :: Lens.Lens' ListInstanceStorageConfigs (Core.Maybe Types.NextToken)
liscNextToken = Lens.field @"nextToken"
{-# DEPRECATED liscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListInstanceStorageConfigs where
  type
    Rs ListInstanceStorageConfigs =
      ListInstanceStorageConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/storage-configs")
            ),
        Core._rqQuery =
          Core.toQueryValue "resourceType" resourceType
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceStorageConfigsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "StorageConfigs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListInstanceStorageConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"storageConfigs" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListInstanceStorageConfigsResponse' smart constructor.
data ListInstanceStorageConfigsResponse = ListInstanceStorageConfigsResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A valid storage type.
    storageConfigs :: Core.Maybe [Types.InstanceStorageConfig],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceStorageConfigsResponse' value with any optional fields omitted.
mkListInstanceStorageConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListInstanceStorageConfigsResponse
mkListInstanceStorageConfigsResponse responseStatus =
  ListInstanceStorageConfigsResponse'
    { nextToken = Core.Nothing,
      storageConfigs = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrrsNextToken :: Lens.Lens' ListInstanceStorageConfigsResponse (Core.Maybe Types.NextToken)
liscrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED liscrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A valid storage type.
--
-- /Note:/ Consider using 'storageConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrrsStorageConfigs :: Lens.Lens' ListInstanceStorageConfigsResponse (Core.Maybe [Types.InstanceStorageConfig])
liscrrsStorageConfigs = Lens.field @"storageConfigs"
{-# DEPRECATED liscrrsStorageConfigs "Use generic-lens or generic-optics with 'storageConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liscrrsResponseStatus :: Lens.Lens' ListInstanceStorageConfigsResponse Core.Int
liscrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED liscrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
