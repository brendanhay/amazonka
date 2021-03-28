{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway. If any optional field is not provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGateway
    (
    -- * Creating a request
      UpdateGateway (..)
    , mkUpdateGateway
    -- ** Request lenses
    , ugGatewayArn
    , ugDescription
    , ugName
    , ugSoftwareVersion

    -- * Destructuring the response
    , UpdateGatewayResponse (..)
    , mkUpdateGatewayResponse
    -- ** Response lenses
    , ugrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateGateway' smart constructor.
data UpdateGateway = UpdateGateway'
  { gatewayArn :: Types.Arn
    -- ^ The ARN of the gateway to update.
  , description :: Core.Maybe Types.Description
    -- ^ The updated description of the gateway.
  , name :: Core.Maybe Types.Name
    -- ^ The updated name of the gateway.
  , softwareVersion :: Core.Maybe Types.SoftwareVersion
    -- ^ The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGateway' value with any optional fields omitted.
mkUpdateGateway
    :: Types.Arn -- ^ 'gatewayArn'
    -> UpdateGateway
mkUpdateGateway gatewayArn
  = UpdateGateway'{gatewayArn, description = Core.Nothing,
                   name = Core.Nothing, softwareVersion = Core.Nothing}

-- | The ARN of the gateway to update.
--
-- /Note:/ Consider using 'gatewayArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGatewayArn :: Lens.Lens' UpdateGateway Types.Arn
ugGatewayArn = Lens.field @"gatewayArn"
{-# INLINEABLE ugGatewayArn #-}
{-# DEPRECATED gatewayArn "Use generic-lens or generic-optics with 'gatewayArn' instead"  #-}

-- | The updated description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGateway (Core.Maybe Types.Description)
ugDescription = Lens.field @"description"
{-# INLINEABLE ugDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugName :: Lens.Lens' UpdateGateway (Core.Maybe Types.Name)
ugName = Lens.field @"name"
{-# INLINEABLE ugName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The updated software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugSoftwareVersion :: Lens.Lens' UpdateGateway (Core.Maybe Types.SoftwareVersion)
ugSoftwareVersion = Lens.field @"softwareVersion"
{-# INLINEABLE ugSoftwareVersion #-}
{-# DEPRECATED softwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead"  #-}

instance Core.ToQuery UpdateGateway where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateGateway where
        toHeaders UpdateGateway{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateGateway")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateGateway where
        toJSON UpdateGateway{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GatewayArn" Core..= gatewayArn),
                  ("Description" Core..=) Core.<$> description,
                  ("Name" Core..=) Core.<$> name,
                  ("SoftwareVersion" Core..=) Core.<$> softwareVersion])

instance Core.AWSRequest UpdateGateway where
        type Rs UpdateGateway = UpdateGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateGatewayResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateGatewayResponse' smart constructor.
newtype UpdateGatewayResponse = UpdateGatewayResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGatewayResponse' value with any optional fields omitted.
mkUpdateGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateGatewayResponse
mkUpdateGatewayResponse responseStatus
  = UpdateGatewayResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrrsResponseStatus :: Lens.Lens' UpdateGatewayResponse Core.Int
ugrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ugrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
