{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified project name, given the project ARN and a new name.
module Network.AWS.DeviceFarm.UpdateProject
    (
    -- * Creating a request
      UpdateProject (..)
    , mkUpdateProject
    -- ** Request lenses
    , upArn
    , upDefaultJobTimeoutMinutes
    , upName

    -- * Destructuring the response
    , UpdateProjectResponse (..)
    , mkUpdateProjectResponse
    -- ** Response lenses
    , uprrsProject
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the update project operation.
--
-- /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the project whose name to update.
  , defaultJobTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ The number of minutes a test run in the project executes before it times out.
  , name :: Core.Maybe Types.Name
    -- ^ A string that represents the new name of the project that you are updating.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject
    :: Types.Arn -- ^ 'arn'
    -> UpdateProject
mkUpdateProject arn
  = UpdateProject'{arn, defaultJobTimeoutMinutes = Core.Nothing,
                   name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project whose name to update.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upArn :: Lens.Lens' UpdateProject Types.Arn
upArn = Lens.field @"arn"
{-# INLINEABLE upArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The number of minutes a test run in the project executes before it times out.
--
-- /Note:/ Consider using 'defaultJobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDefaultJobTimeoutMinutes :: Lens.Lens' UpdateProject (Core.Maybe Core.Int)
upDefaultJobTimeoutMinutes = Lens.field @"defaultJobTimeoutMinutes"
{-# INLINEABLE upDefaultJobTimeoutMinutes #-}
{-# DEPRECATED defaultJobTimeoutMinutes "Use generic-lens or generic-optics with 'defaultJobTimeoutMinutes' instead"  #-}

-- | A string that represents the new name of the project that you are updating.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject (Core.Maybe Types.Name)
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProject where
        toHeaders UpdateProject{..}
          = Core.pure ("X-Amz-Target", "DeviceFarm_20150623.UpdateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateProject where
        toJSON UpdateProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("arn" Core..= arn),
                  ("defaultJobTimeoutMinutes" Core..=) Core.<$>
                    defaultJobTimeoutMinutes,
                  ("name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateProjectResponse' Core.<$>
                   (x Core..:? "project") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the result of an update project request.
--
-- /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { project :: Core.Maybe Types.Project
    -- ^ The project to update.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProjectResponse
mkUpdateProjectResponse responseStatus
  = UpdateProjectResponse'{project = Core.Nothing, responseStatus}

-- | The project to update.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsProject :: Lens.Lens' UpdateProjectResponse (Core.Maybe Types.Project)
uprrsProject = Lens.field @"project"
{-# INLINEABLE uprrsProject #-}
{-# DEPRECATED project "Use generic-lens or generic-optics with 'project' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
