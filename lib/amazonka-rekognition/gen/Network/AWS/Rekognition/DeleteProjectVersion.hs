{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels model. 
--
-- You can't delete a model if it is running or if it is training. To check the status of a model, use the @Status@ field returned from 'DescribeProjectVersions' . To stop a running model call 'StopProjectVersion' . If the model is training, wait until it finishes.
-- This operation requires permissions to perform the @rekognition:DeleteProjectVersion@ action. 
module Network.AWS.Rekognition.DeleteProjectVersion
    (
    -- * Creating a request
      DeleteProjectVersion (..)
    , mkDeleteProjectVersion
    -- ** Request lenses
    , dpvProjectVersionArn

    -- * Destructuring the response
    , DeleteProjectVersionResponse (..)
    , mkDeleteProjectVersionResponse
    -- ** Response lenses
    , dpvrfrsStatus
    , dpvrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProjectVersion' smart constructor.
newtype DeleteProjectVersion = DeleteProjectVersion'
  { projectVersionArn :: Types.ProjectVersionArn
    -- ^ The Amazon Resource Name (ARN) of the model version that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectVersion' value with any optional fields omitted.
mkDeleteProjectVersion
    :: Types.ProjectVersionArn -- ^ 'projectVersionArn'
    -> DeleteProjectVersion
mkDeleteProjectVersion projectVersionArn
  = DeleteProjectVersion'{projectVersionArn}

-- | The Amazon Resource Name (ARN) of the model version that you want to delete.
--
-- /Note:/ Consider using 'projectVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvProjectVersionArn :: Lens.Lens' DeleteProjectVersion Types.ProjectVersionArn
dpvProjectVersionArn = Lens.field @"projectVersionArn"
{-# INLINEABLE dpvProjectVersionArn #-}
{-# DEPRECATED projectVersionArn "Use generic-lens or generic-optics with 'projectVersionArn' instead"  #-}

instance Core.ToQuery DeleteProjectVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProjectVersion where
        toHeaders DeleteProjectVersion{..}
          = Core.pure
              ("X-Amz-Target", "RekognitionService.DeleteProjectVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProjectVersion where
        toJSON DeleteProjectVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProjectVersionArn" Core..= projectVersionArn)])

instance Core.AWSRequest DeleteProjectVersion where
        type Rs DeleteProjectVersion = DeleteProjectVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteProjectVersionResponse' Core.<$>
                   (x Core..:? "Status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProjectVersionResponse' smart constructor.
data DeleteProjectVersionResponse = DeleteProjectVersionResponse'
  { status :: Core.Maybe Types.ProjectVersionStatus
    -- ^ The status of the deletion operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectVersionResponse' value with any optional fields omitted.
mkDeleteProjectVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProjectVersionResponse
mkDeleteProjectVersionResponse responseStatus
  = DeleteProjectVersionResponse'{status = Core.Nothing,
                                  responseStatus}

-- | The status of the deletion operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrfrsStatus :: Lens.Lens' DeleteProjectVersionResponse (Core.Maybe Types.ProjectVersionStatus)
dpvrfrsStatus = Lens.field @"status"
{-# INLINEABLE dpvrfrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrfrsResponseStatus :: Lens.Lens' DeleteProjectVersionResponse Core.Int
dpvrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpvrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
