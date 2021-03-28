{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change details of a project.
module Network.AWS.DeviceFarm.UpdateTestGridProject
    (
    -- * Creating a request
      UpdateTestGridProject (..)
    , mkUpdateTestGridProject
    -- ** Request lenses
    , utgpProjectArn
    , utgpDescription
    , utgpName

    -- * Destructuring the response
    , UpdateTestGridProjectResponse (..)
    , mkUpdateTestGridProjectResponse
    -- ** Response lenses
    , utgprrsTestGridProject
    , utgprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTestGridProject' smart constructor.
data UpdateTestGridProject = UpdateTestGridProject'
  { projectArn :: Types.DeviceFarmArn
    -- ^ ARN of the project to update.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ Human-readable description for the project.
  , name :: Core.Maybe Types.ResourceName
    -- ^ Human-readable name for the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTestGridProject' value with any optional fields omitted.
mkUpdateTestGridProject
    :: Types.DeviceFarmArn -- ^ 'projectArn'
    -> UpdateTestGridProject
mkUpdateTestGridProject projectArn
  = UpdateTestGridProject'{projectArn, description = Core.Nothing,
                           name = Core.Nothing}

-- | ARN of the project to update.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpProjectArn :: Lens.Lens' UpdateTestGridProject Types.DeviceFarmArn
utgpProjectArn = Lens.field @"projectArn"
{-# INLINEABLE utgpProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | Human-readable description for the project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpDescription :: Lens.Lens' UpdateTestGridProject (Core.Maybe Types.ResourceDescription)
utgpDescription = Lens.field @"description"
{-# INLINEABLE utgpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Human-readable name for the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgpName :: Lens.Lens' UpdateTestGridProject (Core.Maybe Types.ResourceName)
utgpName = Lens.field @"name"
{-# INLINEABLE utgpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateTestGridProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTestGridProject where
        toHeaders UpdateTestGridProject{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.UpdateTestGridProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTestGridProject where
        toJSON UpdateTestGridProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectArn" Core..= projectArn),
                  ("description" Core..=) Core.<$> description,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateTestGridProject where
        type Rs UpdateTestGridProject = UpdateTestGridProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTestGridProjectResponse' Core.<$>
                   (x Core..:? "testGridProject") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTestGridProjectResponse' smart constructor.
data UpdateTestGridProjectResponse = UpdateTestGridProjectResponse'
  { testGridProject :: Core.Maybe Types.TestGridProject
    -- ^ The project, including updated information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateTestGridProjectResponse' value with any optional fields omitted.
mkUpdateTestGridProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTestGridProjectResponse
mkUpdateTestGridProjectResponse responseStatus
  = UpdateTestGridProjectResponse'{testGridProject = Core.Nothing,
                                   responseStatus}

-- | The project, including updated information.
--
-- /Note:/ Consider using 'testGridProject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgprrsTestGridProject :: Lens.Lens' UpdateTestGridProjectResponse (Core.Maybe Types.TestGridProject)
utgprrsTestGridProject = Lens.field @"testGridProject"
{-# INLINEABLE utgprrsTestGridProject #-}
{-# DEPRECATED testGridProject "Use generic-lens or generic-optics with 'testGridProject' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utgprrsResponseStatus :: Lens.Lens' UpdateTestGridProjectResponse Core.Int
utgprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utgprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
