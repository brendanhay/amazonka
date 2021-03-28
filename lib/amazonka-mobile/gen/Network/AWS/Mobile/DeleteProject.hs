{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delets a project in AWS Mobile Hub. 
module Network.AWS.Mobile.DeleteProject
    (
    -- * Creating a request
      DeleteProject (..)
    , mkDeleteProject
    -- ** Request lenses
    , dpProjectId

    -- * Destructuring the response
    , DeleteProjectResponse (..)
    , mkDeleteProjectResponse
    -- ** Response lenses
    , dprrsDeletedResources
    , dprrsOrphanedResources
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request a project be deleted. 
--
-- /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { projectId :: Types.ProjectId
    -- ^ Unique project identifier. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProject' value with any optional fields omitted.
mkDeleteProject
    :: Types.ProjectId -- ^ 'projectId'
    -> DeleteProject
mkDeleteProject projectId = DeleteProject'{projectId}

-- | Unique project identifier. 
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProjectId :: Lens.Lens' DeleteProject Types.ProjectId
dpProjectId = Lens.field @"projectId"
{-# INLINEABLE dpProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

instance Core.ToQuery DeleteProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProject where
        toHeaders DeleteProject{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/projects/" Core.<> Core.toText projectId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteProjectResponse' Core.<$>
                   (x Core..:? "deletedResources") Core.<*>
                     x Core..:? "orphanedResources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result structure used in response to request to delete a project. 
--
-- /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { deletedResources :: Core.Maybe [Types.Resource]
    -- ^ Resources which were deleted. 
  , orphanedResources :: Core.Maybe [Types.Resource]
    -- ^ Resources which were not deleted, due to a risk of losing potentially important data or files. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectResponse' value with any optional fields omitted.
mkDeleteProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProjectResponse
mkDeleteProjectResponse responseStatus
  = DeleteProjectResponse'{deletedResources = Core.Nothing,
                           orphanedResources = Core.Nothing, responseStatus}

-- | Resources which were deleted. 
--
-- /Note:/ Consider using 'deletedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsDeletedResources :: Lens.Lens' DeleteProjectResponse (Core.Maybe [Types.Resource])
dprrsDeletedResources = Lens.field @"deletedResources"
{-# INLINEABLE dprrsDeletedResources #-}
{-# DEPRECATED deletedResources "Use generic-lens or generic-optics with 'deletedResources' instead"  #-}

-- | Resources which were not deleted, due to a risk of losing potentially important data or files. 
--
-- /Note:/ Consider using 'orphanedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsOrphanedResources :: Lens.Lens' DeleteProjectResponse (Core.Maybe [Types.Resource])
dprrsOrphanedResources = Lens.field @"orphanedResources"
{-# INLINEABLE dprrsOrphanedResources #-}
{-# DEPRECATED orphanedResources "Use generic-lens or generic-optics with 'orphanedResources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProjectResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
