{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Network.AWS.AlexaBusiness.GetGatewayGroup
    (
    -- * Creating a request
      GetGatewayGroup (..)
    , mkGetGatewayGroup
    -- ** Request lenses
    , gggGatewayGroupArn

    -- * Destructuring the response
    , GetGatewayGroupResponse (..)
    , mkGetGatewayGroupResponse
    -- ** Response lenses
    , gggrrsGatewayGroup
    , gggrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGatewayGroup' smart constructor.
newtype GetGatewayGroup = GetGatewayGroup'
  { gatewayGroupArn :: Types.Arn
    -- ^ The ARN of the gateway group to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayGroup' value with any optional fields omitted.
mkGetGatewayGroup
    :: Types.Arn -- ^ 'gatewayGroupArn'
    -> GetGatewayGroup
mkGetGatewayGroup gatewayGroupArn
  = GetGatewayGroup'{gatewayGroupArn}

-- | The ARN of the gateway group to get.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggGatewayGroupArn :: Lens.Lens' GetGatewayGroup Types.Arn
gggGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# INLINEABLE gggGatewayGroupArn #-}
{-# DEPRECATED gatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead"  #-}

instance Core.ToQuery GetGatewayGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetGatewayGroup where
        toHeaders GetGatewayGroup{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.GetGatewayGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetGatewayGroup where
        toJSON GetGatewayGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayGroupArn" Core..= gatewayGroupArn)])

instance Core.AWSRequest GetGatewayGroup where
        type Rs GetGatewayGroup = GetGatewayGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetGatewayGroupResponse' Core.<$>
                   (x Core..:? "GatewayGroup") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { gatewayGroup :: Core.Maybe Types.GatewayGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGatewayGroupResponse' value with any optional fields omitted.
mkGetGatewayGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGatewayGroupResponse
mkGetGatewayGroupResponse responseStatus
  = GetGatewayGroupResponse'{gatewayGroup = Core.Nothing,
                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrrsGatewayGroup :: Lens.Lens' GetGatewayGroupResponse (Core.Maybe Types.GatewayGroup)
gggrrsGatewayGroup = Lens.field @"gatewayGroup"
{-# INLINEABLE gggrrsGatewayGroup #-}
{-# DEPRECATED gatewayGroup "Use generic-lens or generic-optics with 'gatewayGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gggrrsResponseStatus :: Lens.Lens' GetGatewayGroupResponse Core.Int
gggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
