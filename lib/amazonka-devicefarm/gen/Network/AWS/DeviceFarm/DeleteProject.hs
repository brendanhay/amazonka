{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Device Farm project, given the project ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteProject
    (
    -- * Creating a request
      DeleteProject (..)
    , mkDeleteProject
    -- ** Request lenses
    , dpfArn

    -- * Destructuring the response
    , DeleteProjectResponse (..)
    , mkDeleteProjectResponse
    -- ** Response lenses
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the delete project operation.
--
-- /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { arn :: Types.Arn
    -- ^ Represents the Amazon Resource Name (ARN) of the Device Farm project to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProject' value with any optional fields omitted.
mkDeleteProject
    :: Types.Arn -- ^ 'arn'
    -> DeleteProject
mkDeleteProject arn = DeleteProject'{arn}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm project to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfArn :: Lens.Lens' DeleteProject Types.Arn
dpfArn = Lens.field @"arn"
{-# INLINEABLE dpfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery DeleteProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProject where
        toHeaders DeleteProject{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.DeleteProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProject where
        toJSON DeleteProject{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteProjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of a delete project request.
--
-- /See:/ 'mkDeleteProjectResponse' smart constructor.
newtype DeleteProjectResponse = DeleteProjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProjectResponse' value with any optional fields omitted.
mkDeleteProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProjectResponse
mkDeleteProjectResponse responseStatus
  = DeleteProjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProjectResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
