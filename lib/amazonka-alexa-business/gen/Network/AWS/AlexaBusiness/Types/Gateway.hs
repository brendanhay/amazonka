{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Gateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Gateway
  ( Gateway (..),

    -- * Smart constructor
    mkGateway,

    -- * Lenses
    gArn,
    gDescription,
    gGatewayGroupArn,
    gName,
    gSoftwareVersion,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.Name as Types
import qualified Network.AWS.AlexaBusiness.Types.SoftwareVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the gateway.
--
-- /See:/ 'mkGateway' smart constructor.
data Gateway = Gateway'
  { -- | The ARN of the gateway.
    arn :: Core.Maybe Types.Arn,
    -- | The description of the gateway.
    description :: Core.Maybe Types.Description,
    -- | The ARN of the gateway group that the gateway is associated to.
    gatewayGroupArn :: Core.Maybe Types.Arn,
    -- | The name of the gateway.
    name :: Core.Maybe Types.Name,
    -- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
    softwareVersion :: Core.Maybe Types.SoftwareVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Gateway' value with any optional fields omitted.
mkGateway ::
  Gateway
mkGateway =
  Gateway'
    { arn = Core.Nothing,
      description = Core.Nothing,
      gatewayGroupArn = Core.Nothing,
      name = Core.Nothing,
      softwareVersion = Core.Nothing
    }

-- | The ARN of the gateway.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gArn :: Lens.Lens' Gateway (Core.Maybe Types.Arn)
gArn = Lens.field @"arn"
{-# DEPRECATED gArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDescription :: Lens.Lens' Gateway (Core.Maybe Types.Description)
gDescription = Lens.field @"description"
{-# DEPRECATED gDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the gateway group that the gateway is associated to.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGatewayGroupArn :: Lens.Lens' Gateway (Core.Maybe Types.Arn)
gGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED gGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Gateway (Core.Maybe Types.Name)
gName = Lens.field @"name"
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gSoftwareVersion :: Lens.Lens' Gateway (Core.Maybe Types.SoftwareVersion)
gSoftwareVersion = Lens.field @"softwareVersion"
{-# DEPRECATED gSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

instance Core.FromJSON Gateway where
  parseJSON =
    Core.withObject "Gateway" Core.$
      \x ->
        Gateway'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "GatewayGroupArn")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "SoftwareVersion")
