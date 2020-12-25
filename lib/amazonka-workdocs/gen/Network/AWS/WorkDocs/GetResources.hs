{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of resources, including folders and documents. The only @CollectionType@ supported is @SHARED_WITH_ME@ .
module Network.AWS.WorkDocs.GetResources
  ( -- * Creating a request
    GetResources (..),
    mkGetResources,

    -- ** Request lenses
    grAuthenticationToken,
    grCollectionType,
    grLimit,
    grMarker,
    grUserId,

    -- * Destructuring the response
    GetResourcesResponse (..),
    mkGetResourcesResponse,

    -- ** Response lenses
    grrrsDocuments,
    grrrsFolders,
    grrrsMarker,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { -- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The collection type.
    collectionType :: Core.Maybe Types.ResourceCollectionType,
    -- | The maximum number of resources to return.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results. This marker was received from a previous call.
    marker :: Core.Maybe Types.Marker,
    -- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
    userId :: Core.Maybe Types.IdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResources' value with any optional fields omitted.
mkGetResources ::
  GetResources
mkGetResources =
  GetResources'
    { authenticationToken = Core.Nothing,
      collectionType = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      userId = Core.Nothing
    }

-- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grAuthenticationToken :: Lens.Lens' GetResources (Core.Maybe Types.AuthenticationHeaderType)
grAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED grAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The collection type.
--
-- /Note:/ Consider using 'collectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grCollectionType :: Lens.Lens' GetResources (Core.Maybe Types.ResourceCollectionType)
grCollectionType = Lens.field @"collectionType"
{-# DEPRECATED grCollectionType "Use generic-lens or generic-optics with 'collectionType' instead." #-}

-- | The maximum number of resources to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grLimit :: Lens.Lens' GetResources (Core.Maybe Core.Natural)
grLimit = Lens.field @"limit"
{-# DEPRECATED grLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grMarker :: Lens.Lens' GetResources (Core.Maybe Types.Marker)
grMarker = Lens.field @"marker"
{-# DEPRECATED grMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grUserId :: Lens.Lens' GetResources (Core.Maybe Types.IdType)
grUserId = Lens.field @"userId"
{-# DEPRECATED grUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.AWSRequest GetResources where
  type Rs GetResources = GetResourcesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/api/v1/resources",
        Core._rqQuery =
          Core.toQueryValue "collectionType" Core.<$> collectionType
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "userId" Core.<$> userId),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcesResponse'
            Core.<$> (x Core..:? "Documents")
            Core.<*> (x Core..:? "Folders")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { -- | The documents in the specified collection.
    documents :: Core.Maybe [Types.DocumentMetadata],
    -- | The folders in the specified folder.
    folders :: Core.Maybe [Types.FolderMetadata],
    -- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
    marker :: Core.Maybe Types.PageMarkerType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetResourcesResponse' value with any optional fields omitted.
mkGetResourcesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourcesResponse
mkGetResourcesResponse responseStatus =
  GetResourcesResponse'
    { documents = Core.Nothing,
      folders = Core.Nothing,
      marker = Core.Nothing,
      responseStatus
    }

-- | The documents in the specified collection.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsDocuments :: Lens.Lens' GetResourcesResponse (Core.Maybe [Types.DocumentMetadata])
grrrsDocuments = Lens.field @"documents"
{-# DEPRECATED grrrsDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The folders in the specified folder.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsFolders :: Lens.Lens' GetResourcesResponse (Core.Maybe [Types.FolderMetadata])
grrrsFolders = Lens.field @"folders"
{-# DEPRECATED grrrsFolders "Use generic-lens or generic-optics with 'folders' instead." #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsMarker :: Lens.Lens' GetResourcesResponse (Core.Maybe Types.PageMarkerType)
grrrsMarker = Lens.field @"marker"
{-# DEPRECATED grrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetResourcesResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
