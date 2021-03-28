{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified list of labels from a resource.
module Network.AWS.WorkDocs.DeleteLabels
    (
    -- * Creating a request
      DeleteLabels (..)
    , mkDeleteLabels
    -- ** Request lenses
    , dlResourceId
    , dlAuthenticationToken
    , dlDeleteAll
    , dlLabels

    -- * Destructuring the response
    , DeleteLabelsResponse (..)
    , mkDeleteLabelsResponse
    -- ** Response lenses
    , dlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { resourceId :: Types.ResourceIdType
    -- ^ The ID of the resource.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , deleteAll :: Core.Maybe Core.Bool
    -- ^ Flag to request removal of all labels from the specified resource.
  , labels :: Core.Maybe [Types.SharedLabel]
    -- ^ List of labels to delete from the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLabels' value with any optional fields omitted.
mkDeleteLabels
    :: Types.ResourceIdType -- ^ 'resourceId'
    -> DeleteLabels
mkDeleteLabels resourceId
  = DeleteLabels'{resourceId, authenticationToken = Core.Nothing,
                  deleteAll = Core.Nothing, labels = Core.Nothing}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlResourceId :: Lens.Lens' DeleteLabels Types.ResourceIdType
dlResourceId = Lens.field @"resourceId"
{-# INLINEABLE dlResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlAuthenticationToken :: Lens.Lens' DeleteLabels (Core.Maybe Types.AuthenticationHeaderType)
dlAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE dlAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | Flag to request removal of all labels from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlDeleteAll :: Lens.Lens' DeleteLabels (Core.Maybe Core.Bool)
dlDeleteAll = Lens.field @"deleteAll"
{-# INLINEABLE dlDeleteAll #-}
{-# DEPRECATED deleteAll "Use generic-lens or generic-optics with 'deleteAll' instead"  #-}

-- | List of labels to delete from the resource.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLabels :: Lens.Lens' DeleteLabels (Core.Maybe [Types.SharedLabel])
dlLabels = Lens.field @"labels"
{-# INLINEABLE dlLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

instance Core.ToQuery DeleteLabels where
        toQuery DeleteLabels{..}
          = Core.maybe Core.mempty (Core.toQueryPair "deleteAll") deleteAll
              Core.<>
              Core.toQueryPair "labels"
                (Core.maybe Core.mempty (Core.toQueryList "member") labels)

instance Core.ToHeaders DeleteLabels where
        toHeaders DeleteLabels{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteLabels where
        type Rs DeleteLabels = DeleteLabelsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/api/v1/resources/" Core.<> Core.toText resourceId Core.<>
                             "/labels",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteLabelsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLabelsResponse' smart constructor.
newtype DeleteLabelsResponse = DeleteLabelsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLabelsResponse' value with any optional fields omitted.
mkDeleteLabelsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLabelsResponse
mkDeleteLabelsResponse responseStatus
  = DeleteLabelsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DeleteLabelsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
