{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetResources (..)
    , mkGetResources
    -- ** Request lenses
    , grAuthenticationToken
    , grCollectionType
    , grLimit
    , grMarker
    , grUserId

    -- * Destructuring the response
    , GetResourcesResponse (..)
    , mkGetResourcesResponse
    -- ** Response lenses
    , grrrsDocuments
    , grrrsFolders
    , grrrsMarker
    , grrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetResources' smart constructor.
data GetResources = GetResources'
  { authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , collectionType :: Core.Maybe Types.ResourceCollectionType
    -- ^ The collection type.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of resources to return.
  , marker :: Core.Maybe Types.Marker
    -- ^ The marker for the next set of results. This marker was received from a previous call.
  , userId :: Core.Maybe Types.IdType
    -- ^ The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResources' value with any optional fields omitted.
mkGetResources
    :: GetResources
mkGetResources
  = GetResources'{authenticationToken = Core.Nothing,
                  collectionType = Core.Nothing, limit = Core.Nothing,
                  marker = Core.Nothing, userId = Core.Nothing}

-- | The Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grAuthenticationToken :: Lens.Lens' GetResources (Core.Maybe Types.AuthenticationHeaderType)
grAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE grAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The collection type.
--
-- /Note:/ Consider using 'collectionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grCollectionType :: Lens.Lens' GetResources (Core.Maybe Types.ResourceCollectionType)
grCollectionType = Lens.field @"collectionType"
{-# INLINEABLE grCollectionType #-}
{-# DEPRECATED collectionType "Use generic-lens or generic-optics with 'collectionType' instead"  #-}

-- | The maximum number of resources to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grLimit :: Lens.Lens' GetResources (Core.Maybe Core.Natural)
grLimit = Lens.field @"limit"
{-# INLINEABLE grLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. This marker was received from a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grMarker :: Lens.Lens' GetResources (Core.Maybe Types.Marker)
grMarker = Lens.field @"marker"
{-# INLINEABLE grMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The user ID for the resource collection. This is a required field for accessing the API operation using IAM credentials.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grUserId :: Lens.Lens' GetResources (Core.Maybe Types.IdType)
grUserId = Lens.field @"userId"
{-# INLINEABLE grUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery GetResources where
        toQuery GetResources{..}
          = Core.maybe Core.mempty (Core.toQueryPair "collectionType")
              collectionType
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "marker") marker
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "userId") userId

instance Core.ToHeaders GetResources where
        toHeaders GetResources{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetResources where
        type Rs GetResources = GetResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/api/v1/resources",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourcesResponse' Core.<$>
                   (x Core..:? "Documents") Core.<*> x Core..:? "Folders" Core.<*>
                     x Core..:? "Marker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetResourcesResponse' smart constructor.
data GetResourcesResponse = GetResourcesResponse'
  { documents :: Core.Maybe [Types.DocumentMetadata]
    -- ^ The documents in the specified collection.
  , folders :: Core.Maybe [Types.FolderMetadata]
    -- ^ The folders in the specified folder.
  , marker :: Core.Maybe Types.PageMarkerType
    -- ^ The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetResourcesResponse' value with any optional fields omitted.
mkGetResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourcesResponse
mkGetResourcesResponse responseStatus
  = GetResourcesResponse'{documents = Core.Nothing,
                          folders = Core.Nothing, marker = Core.Nothing, responseStatus}

-- | The documents in the specified collection.
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsDocuments :: Lens.Lens' GetResourcesResponse (Core.Maybe [Types.DocumentMetadata])
grrrsDocuments = Lens.field @"documents"
{-# INLINEABLE grrrsDocuments #-}
{-# DEPRECATED documents "Use generic-lens or generic-optics with 'documents' instead"  #-}

-- | The folders in the specified folder.
--
-- /Note:/ Consider using 'folders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsFolders :: Lens.Lens' GetResourcesResponse (Core.Maybe [Types.FolderMetadata])
grrrsFolders = Lens.field @"folders"
{-# INLINEABLE grrrsFolders #-}
{-# DEPRECATED folders "Use generic-lens or generic-optics with 'folders' instead"  #-}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsMarker :: Lens.Lens' GetResourcesResponse (Core.Maybe Types.PageMarkerType)
grrrsMarker = Lens.field @"marker"
{-# INLINEABLE grrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetResourcesResponse Core.Int
grrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
