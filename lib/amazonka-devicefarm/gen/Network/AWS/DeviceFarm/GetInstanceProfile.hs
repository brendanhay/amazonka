{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified instance profile.
module Network.AWS.DeviceFarm.GetInstanceProfile
    (
    -- * Creating a request
      GetInstanceProfile (..)
    , mkGetInstanceProfile
    -- ** Request lenses
    , gipArn

    -- * Destructuring the response
    , GetInstanceProfileResponse (..)
    , mkGetInstanceProfileResponse
    -- ** Response lenses
    , giprrsInstanceProfile
    , giprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstanceProfile' smart constructor.
newtype GetInstanceProfile = GetInstanceProfile'
  { arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of an instance profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceProfile' value with any optional fields omitted.
mkGetInstanceProfile
    :: Types.Arn -- ^ 'arn'
    -> GetInstanceProfile
mkGetInstanceProfile arn = GetInstanceProfile'{arn}

-- | The Amazon Resource Name (ARN) of an instance profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipArn :: Lens.Lens' GetInstanceProfile Types.Arn
gipArn = Lens.field @"arn"
{-# INLINEABLE gipArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetInstanceProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstanceProfile where
        toHeaders GetInstanceProfile{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetInstanceProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstanceProfile where
        toJSON GetInstanceProfile{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetInstanceProfile where
        type Rs GetInstanceProfile = GetInstanceProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceProfileResponse' Core.<$>
                   (x Core..:? "instanceProfile") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { instanceProfile :: Core.Maybe Types.InstanceProfile
    -- ^ An object that contains information about an instance profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceProfileResponse' value with any optional fields omitted.
mkGetInstanceProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceProfileResponse
mkGetInstanceProfileResponse responseStatus
  = GetInstanceProfileResponse'{instanceProfile = Core.Nothing,
                                responseStatus}

-- | An object that contains information about an instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsInstanceProfile :: Lens.Lens' GetInstanceProfileResponse (Core.Maybe Types.InstanceProfile)
giprrsInstanceProfile = Lens.field @"instanceProfile"
{-# INLINEABLE giprrsInstanceProfile #-}
{-# DEPRECATED instanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprrsResponseStatus :: Lens.Lens' GetInstanceProfileResponse Core.Int
giprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
