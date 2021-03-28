{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels project. To delete a project you must first delete all models associated with the project. To delete a model, see 'DeleteProjectVersion' .
--
-- This operation requires permissions to perform the @rekognition:DeleteProject@ action. 
module Network.AWS.Rekognition.DeleteProject
    (
    -- * Creating a request
      DeleteProject (..)
    , mkDeleteProject
    -- ** Request lenses
    , dpProjectArn

    -- * Destructuring the response
    , DeleteProjectResponse (..)
    , mkDeleteProjectResponse
    -- ** Response lenses
    , dprrsStatus
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { projectArn :: Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the project that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProject' value with any optional fields omitted.
mkDeleteProject
    :: Types.ProjectArn -- ^ 'projectArn'
    -> DeleteProject
mkDeleteProject projectArn = DeleteProject'{projectArn}

-- | The Amazon Resource Name (ARN) of the project that you want to delete.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProjectArn :: Lens.Lens' DeleteProject Types.ProjectArn
dpProjectArn = Lens.field @"projectArn"
{-# INLINEABLE dpProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

instance Core.ToQuery DeleteProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProject where
        toHeaders DeleteProject{..}
          = Core.pure ("X-Amz-Target", "RekognitionService.DeleteProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProject where
        toJSON DeleteProject{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ProjectArn" Core..= projectArn)])

instance Core.AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteProjectResponse' Core.<$>
                   (x Core..:? "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { status :: Core.Maybe Types.ProjectStatus
    -- ^ The current status of the delete project operation.
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
  = DeleteProjectResponse'{status = Core.Nothing, responseStatus}

-- | The current status of the delete project operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsStatus :: Lens.Lens' DeleteProjectResponse (Core.Maybe Types.ProjectStatus)
dprrsStatus = Lens.field @"status"
{-# INLINEABLE dprrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProjectResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
