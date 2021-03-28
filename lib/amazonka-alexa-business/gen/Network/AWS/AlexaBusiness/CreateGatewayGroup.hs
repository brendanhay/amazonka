{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a gateway group with the specified details.
module Network.AWS.AlexaBusiness.CreateGatewayGroup
    (
    -- * Creating a request
      CreateGatewayGroup (..)
    , mkCreateGatewayGroup
    -- ** Request lenses
    , cggName
    , cggClientRequestToken
    , cggDescription

    -- * Destructuring the response
    , CreateGatewayGroupResponse (..)
    , mkCreateGatewayGroupResponse
    -- ** Response lenses
    , cggrrsGatewayGroupArn
    , cggrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGatewayGroup' smart constructor.
data CreateGatewayGroup = CreateGatewayGroup'
  { name :: Types.Name
    -- ^ The name of the gateway group.
  , clientRequestToken :: Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for the request that ensures idempotency.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the gateway group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGatewayGroup' value with any optional fields omitted.
mkCreateGatewayGroup
    :: Types.Name -- ^ 'name'
    -> Types.ClientRequestToken -- ^ 'clientRequestToken'
    -> CreateGatewayGroup
mkCreateGatewayGroup name clientRequestToken
  = CreateGatewayGroup'{name, clientRequestToken,
                        description = Core.Nothing}

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggName :: Lens.Lens' CreateGatewayGroup Types.Name
cggName = Lens.field @"name"
{-# INLINEABLE cggName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique, user-specified identifier for the request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggClientRequestToken :: Lens.Lens' CreateGatewayGroup Types.ClientRequestToken
cggClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cggClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggDescription :: Lens.Lens' CreateGatewayGroup (Core.Maybe Types.Description)
cggDescription = Lens.field @"description"
{-# INLINEABLE cggDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreateGatewayGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateGatewayGroup where
        toHeaders CreateGatewayGroup{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateGatewayGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateGatewayGroup where
        toJSON CreateGatewayGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ClientRequestToken" Core..= clientRequestToken),
                  ("Description" Core..=) Core.<$> description])

instance Core.AWSRequest CreateGatewayGroup where
        type Rs CreateGatewayGroup = CreateGatewayGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateGatewayGroupResponse' Core.<$>
                   (x Core..:? "GatewayGroupArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateGatewayGroupResponse' smart constructor.
data CreateGatewayGroupResponse = CreateGatewayGroupResponse'
  { gatewayGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the created gateway group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGatewayGroupResponse' value with any optional fields omitted.
mkCreateGatewayGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateGatewayGroupResponse
mkCreateGatewayGroupResponse responseStatus
  = CreateGatewayGroupResponse'{gatewayGroupArn = Core.Nothing,
                                responseStatus}

-- | The ARN of the created gateway group.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrrsGatewayGroupArn :: Lens.Lens' CreateGatewayGroupResponse (Core.Maybe Types.Arn)
cggrrsGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# INLINEABLE cggrrsGatewayGroupArn #-}
{-# DEPRECATED gatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cggrrsResponseStatus :: Lens.Lens' CreateGatewayGroupResponse Core.Int
cggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
