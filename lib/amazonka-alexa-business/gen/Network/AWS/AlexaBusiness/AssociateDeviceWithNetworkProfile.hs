{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with the specified network profile.
module Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
    (
    -- * Creating a request
      AssociateDeviceWithNetworkProfile (..)
    , mkAssociateDeviceWithNetworkProfile
    -- ** Request lenses
    , adwnpDeviceArn
    , adwnpNetworkProfileArn

    -- * Destructuring the response
    , AssociateDeviceWithNetworkProfileResponse (..)
    , mkAssociateDeviceWithNetworkProfileResponse
    -- ** Response lenses
    , adwnprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDeviceWithNetworkProfile' smart constructor.
data AssociateDeviceWithNetworkProfile = AssociateDeviceWithNetworkProfile'
  { deviceArn :: Types.Arn
    -- ^ The device ARN.
  , networkProfileArn :: Types.Arn
    -- ^ The ARN of the network profile to associate with a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithNetworkProfile' value with any optional fields omitted.
mkAssociateDeviceWithNetworkProfile
    :: Types.Arn -- ^ 'deviceArn'
    -> Types.Arn -- ^ 'networkProfileArn'
    -> AssociateDeviceWithNetworkProfile
mkAssociateDeviceWithNetworkProfile deviceArn networkProfileArn
  = AssociateDeviceWithNetworkProfile'{deviceArn, networkProfileArn}

-- | The device ARN.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpDeviceArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Types.Arn
adwnpDeviceArn = Lens.field @"deviceArn"
{-# INLINEABLE adwnpDeviceArn #-}
{-# DEPRECATED deviceArn "Use generic-lens or generic-optics with 'deviceArn' instead"  #-}

-- | The ARN of the network profile to associate with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpNetworkProfileArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Types.Arn
adwnpNetworkProfileArn = Lens.field @"networkProfileArn"
{-# INLINEABLE adwnpNetworkProfileArn #-}
{-# DEPRECATED networkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead"  #-}

instance Core.ToQuery AssociateDeviceWithNetworkProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateDeviceWithNetworkProfile where
        toHeaders AssociateDeviceWithNetworkProfile{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.AssociateDeviceWithNetworkProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateDeviceWithNetworkProfile where
        toJSON AssociateDeviceWithNetworkProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeviceArn" Core..= deviceArn),
                  Core.Just ("NetworkProfileArn" Core..= networkProfileArn)])

instance Core.AWSRequest AssociateDeviceWithNetworkProfile where
        type Rs AssociateDeviceWithNetworkProfile =
             AssociateDeviceWithNetworkProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateDeviceWithNetworkProfileResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateDeviceWithNetworkProfileResponse' smart constructor.
newtype AssociateDeviceWithNetworkProfileResponse = AssociateDeviceWithNetworkProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithNetworkProfileResponse' value with any optional fields omitted.
mkAssociateDeviceWithNetworkProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateDeviceWithNetworkProfileResponse
mkAssociateDeviceWithNetworkProfileResponse responseStatus
  = AssociateDeviceWithNetworkProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnprrsResponseStatus :: Lens.Lens' AssociateDeviceWithNetworkProfileResponse Core.Int
adwnprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE adwnprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
