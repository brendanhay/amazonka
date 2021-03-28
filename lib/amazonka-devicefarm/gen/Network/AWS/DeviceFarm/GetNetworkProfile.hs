{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a network profile.
module Network.AWS.DeviceFarm.GetNetworkProfile
    (
    -- * Creating a request
      GetNetworkProfile (..)
    , mkGetNetworkProfile
    -- ** Request lenses
    , gnpArn

    -- * Destructuring the response
    , GetNetworkProfileResponse (..)
    , mkGetNetworkProfileResponse
    -- ** Response lenses
    , gnprrsNetworkProfile
    , gnprrsResponseStatus
    ) where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetNetworkProfile' smart constructor.
newtype GetNetworkProfile = GetNetworkProfile'
  { arn :: Types.Arn
    -- ^ The ARN of the network profile to return information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetNetworkProfile' value with any optional fields omitted.
mkGetNetworkProfile
    :: Types.Arn -- ^ 'arn'
    -> GetNetworkProfile
mkGetNetworkProfile arn = GetNetworkProfile'{arn}

-- | The ARN of the network profile to return information about.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnpArn :: Lens.Lens' GetNetworkProfile Types.Arn
gnpArn = Lens.field @"arn"
{-# INLINEABLE gnpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetNetworkProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetNetworkProfile where
        toHeaders GetNetworkProfile{..}
          = Core.pure
              ("X-Amz-Target", "DeviceFarm_20150623.GetNetworkProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetNetworkProfile where
        toJSON GetNetworkProfile{..}
          = Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetNetworkProfile where
        type Rs GetNetworkProfile = GetNetworkProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetNetworkProfileResponse' Core.<$>
                   (x Core..:? "networkProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { networkProfile :: Core.Maybe Types.NetworkProfile
    -- ^ The network profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetNetworkProfileResponse' value with any optional fields omitted.
mkGetNetworkProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetNetworkProfileResponse
mkGetNetworkProfileResponse responseStatus
  = GetNetworkProfileResponse'{networkProfile = Core.Nothing,
                               responseStatus}

-- | The network profile.
--
-- /Note:/ Consider using 'networkProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprrsNetworkProfile :: Lens.Lens' GetNetworkProfileResponse (Core.Maybe Types.NetworkProfile)
gnprrsNetworkProfile = Lens.field @"networkProfile"
{-# INLINEABLE gnprrsNetworkProfile #-}
{-# DEPRECATED networkProfile "Use generic-lens or generic-optics with 'networkProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnprrsResponseStatus :: Lens.Lens' GetNetworkProfileResponse Core.Int
gnprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gnprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
