{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGatewayGroup
    (
    -- * Creating a request
      UpdateGatewayGroup (..)
    , mkUpdateGatewayGroup
    -- ** Request lenses
    , uggGatewayGroupArn
    , uggDescription
    , uggName

    -- * Destructuring the response
    , UpdateGatewayGroupResponse (..)
    , mkUpdateGatewayGroupResponse
    -- ** Response lenses
    , uggrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { gatewayGroupArn :: Types.GatewayGroupArn
    -- ^ The ARN of the gateway group to update.
  , description :: Core.Maybe Types.Description
    -- ^ The updated description of the gateway group.
  , name :: Core.Maybe Types.Name
    -- ^ The updated name of the gateway group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayGroup' value with any optional fields omitted.
mkUpdateGatewayGroup
    :: Types.GatewayGroupArn -- ^ 'gatewayGroupArn'
    -> UpdateGatewayGroup
mkUpdateGatewayGroup gatewayGroupArn
  = UpdateGatewayGroup'{gatewayGroupArn, description = Core.Nothing,
                        name = Core.Nothing}

-- | The ARN of the gateway group to update.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggGatewayGroupArn :: Lens.Lens' UpdateGatewayGroup Types.GatewayGroupArn
uggGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# INLINEABLE uggGatewayGroupArn #-}
{-# DEPRECATED gatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead"  #-}

-- | The updated description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggDescription :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Types.Description)
uggDescription = Lens.field @"description"
{-# INLINEABLE uggDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggName :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Types.Name)
uggName = Lens.field @"name"
{-# INLINEABLE uggName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateGatewayGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGatewayGroup where
        toHeaders UpdateGatewayGroup{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateGatewayGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGatewayGroup where
        toJSON UpdateGatewayGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayGroupArn" Core..= gatewayGroupArn),
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateGatewayGroup where
        type Rs UpdateGatewayGroup = UpdateGatewayGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateGatewayGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGatewayGroupResponse' smart constructor.
newtype UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayGroupResponse' value with any optional fields omitted.
mkUpdateGatewayGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGatewayGroupResponse
mkUpdateGatewayGroupResponse responseStatus
  = UpdateGatewayGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uggrrsResponseStatus :: Lens.Lens' UpdateGatewayGroupResponse Core.Int
uggrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uggrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
