{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetPatchBaselineForPatchGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the patch baseline that should be used for the specified patch group.
module Network.AWS.SSM.GetPatchBaselineForPatchGroup
    (
    -- * Creating a request
      GetPatchBaselineForPatchGroup (..)
    , mkGetPatchBaselineForPatchGroup
    -- ** Request lenses
    , gpbfpgPatchGroup
    , gpbfpgOperatingSystem

    -- * Destructuring the response
    , GetPatchBaselineForPatchGroupResponse (..)
    , mkGetPatchBaselineForPatchGroupResponse
    -- ** Response lenses
    , gpbfpgrrsBaselineId
    , gpbfpgrrsOperatingSystem
    , gpbfpgrrsPatchGroup
    , gpbfpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetPatchBaselineForPatchGroup' smart constructor.
data GetPatchBaselineForPatchGroup = GetPatchBaselineForPatchGroup'
  { patchGroup :: Types.PatchGroup
    -- ^ The name of the patch group whose patch baseline should be retrieved.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ Returns he operating system rule specified for patch groups using the patch baseline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaselineForPatchGroup' value with any optional fields omitted.
mkGetPatchBaselineForPatchGroup
    :: Types.PatchGroup -- ^ 'patchGroup'
    -> GetPatchBaselineForPatchGroup
mkGetPatchBaselineForPatchGroup patchGroup
  = GetPatchBaselineForPatchGroup'{patchGroup,
                                   operatingSystem = Core.Nothing}

-- | The name of the patch group whose patch baseline should be retrieved.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroup Types.PatchGroup
gpbfpgPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE gpbfpgPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

-- | Returns he operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroup (Core.Maybe Types.OperatingSystem)
gpbfpgOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE gpbfpgOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

instance Core.ToQuery GetPatchBaselineForPatchGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPatchBaselineForPatchGroup where
        toHeaders GetPatchBaselineForPatchGroup{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.GetPatchBaselineForPatchGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPatchBaselineForPatchGroup where
        toJSON GetPatchBaselineForPatchGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PatchGroup" Core..= patchGroup),
                  ("OperatingSystem" Core..=) Core.<$> operatingSystem])

instance Core.AWSRequest GetPatchBaselineForPatchGroup where
        type Rs GetPatchBaselineForPatchGroup =
             GetPatchBaselineForPatchGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPatchBaselineForPatchGroupResponse' Core.<$>
                   (x Core..:? "BaselineId") Core.<*> x Core..:? "OperatingSystem"
                     Core.<*> x Core..:? "PatchGroup"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPatchBaselineForPatchGroupResponse' smart constructor.
data GetPatchBaselineForPatchGroupResponse = GetPatchBaselineForPatchGroupResponse'
  { baselineId :: Core.Maybe Types.BaselineId
    -- ^ The ID of the patch baseline that should be used for the patch group.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ The operating system rule specified for patch groups using the patch baseline.
  , patchGroup :: Core.Maybe Types.PatchGroup
    -- ^ The name of the patch group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaselineForPatchGroupResponse' value with any optional fields omitted.
mkGetPatchBaselineForPatchGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPatchBaselineForPatchGroupResponse
mkGetPatchBaselineForPatchGroupResponse responseStatus
  = GetPatchBaselineForPatchGroupResponse'{baselineId = Core.Nothing,
                                           operatingSystem = Core.Nothing,
                                           patchGroup = Core.Nothing, responseStatus}

-- | The ID of the patch baseline that should be used for the patch group.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsBaselineId :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.BaselineId)
gpbfpgrrsBaselineId = Lens.field @"baselineId"
{-# INLINEABLE gpbfpgrrsBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The operating system rule specified for patch groups using the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsOperatingSystem :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.OperatingSystem)
gpbfpgrrsOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE gpbfpgrrsOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

-- | The name of the patch group.
--
-- /Note:/ Consider using 'patchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsPatchGroup :: Lens.Lens' GetPatchBaselineForPatchGroupResponse (Core.Maybe Types.PatchGroup)
gpbfpgrrsPatchGroup = Lens.field @"patchGroup"
{-# INLINEABLE gpbfpgrrsPatchGroup #-}
{-# DEPRECATED patchGroup "Use generic-lens or generic-optics with 'patchGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbfpgrrsResponseStatus :: Lens.Lens' GetPatchBaselineForPatchGroupResponse Core.Int
gpbfpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpbfpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
