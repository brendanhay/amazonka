{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AssociateDeviceWithNetworkProfile (..),
    mkAssociateDeviceWithNetworkProfile,

    -- ** Request lenses
    adwnpDeviceArn,
    adwnpNetworkProfileArn,

    -- * Destructuring the response
    AssociateDeviceWithNetworkProfileResponse (..),
    mkAssociateDeviceWithNetworkProfileResponse,

    -- ** Response lenses
    adwnprrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateDeviceWithNetworkProfile' smart constructor.
data AssociateDeviceWithNetworkProfile = AssociateDeviceWithNetworkProfile'
  { -- | The device ARN.
    deviceArn :: Types.Arn,
    -- | The ARN of the network profile to associate with a device.
    networkProfileArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithNetworkProfile' value with any optional fields omitted.
mkAssociateDeviceWithNetworkProfile ::
  -- | 'deviceArn'
  Types.Arn ->
  -- | 'networkProfileArn'
  Types.Arn ->
  AssociateDeviceWithNetworkProfile
mkAssociateDeviceWithNetworkProfile deviceArn networkProfileArn =
  AssociateDeviceWithNetworkProfile' {deviceArn, networkProfileArn}

-- | The device ARN.
--
-- /Note:/ Consider using 'deviceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpDeviceArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Types.Arn
adwnpDeviceArn = Lens.field @"deviceArn"
{-# DEPRECATED adwnpDeviceArn "Use generic-lens or generic-optics with 'deviceArn' instead." #-}

-- | The ARN of the network profile to associate with a device.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnpNetworkProfileArn :: Lens.Lens' AssociateDeviceWithNetworkProfile Types.Arn
adwnpNetworkProfileArn = Lens.field @"networkProfileArn"
{-# DEPRECATED adwnpNetworkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead." #-}

instance Core.FromJSON AssociateDeviceWithNetworkProfile where
  toJSON AssociateDeviceWithNetworkProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DeviceArn" Core..= deviceArn),
            Core.Just ("NetworkProfileArn" Core..= networkProfileArn)
          ]
      )

instance Core.AWSRequest AssociateDeviceWithNetworkProfile where
  type
    Rs AssociateDeviceWithNetworkProfile =
      AssociateDeviceWithNetworkProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AlexaForBusiness.AssociateDeviceWithNetworkProfile"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithNetworkProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateDeviceWithNetworkProfileResponse' smart constructor.
newtype AssociateDeviceWithNetworkProfileResponse = AssociateDeviceWithNetworkProfileResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDeviceWithNetworkProfileResponse' value with any optional fields omitted.
mkAssociateDeviceWithNetworkProfileResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateDeviceWithNetworkProfileResponse
mkAssociateDeviceWithNetworkProfileResponse responseStatus =
  AssociateDeviceWithNetworkProfileResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adwnprrsResponseStatus :: Lens.Lens' AssociateDeviceWithNetworkProfileResponse Core.Int
adwnprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED adwnprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
