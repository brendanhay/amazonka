{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.GatewaySummary
  ( GatewaySummary (..)
  -- * Smart constructor
  , mkGatewaySummary
  -- * Lenses
  , gsArn
  , gsDescription
  , gsGatewayGroupArn
  , gsName
  , gsSoftwareVersion
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.GatewayGroupArn as Types
import qualified Network.AWS.AlexaBusiness.Types.Name as Types
import qualified Network.AWS.AlexaBusiness.Types.SoftwareVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of a gateway.
--
-- /See:/ 'mkGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the gateway.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the gateway.
  , gatewayGroupArn :: Core.Maybe Types.GatewayGroupArn
    -- ^ The ARN of the gateway group that the gateway is associated to.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the gateway.
  , softwareVersion :: Core.Maybe Types.SoftwareVersion
    -- ^ The software version of the gateway. The gateway automatically updates its software version during normal operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GatewaySummary' value with any optional fields omitted.
mkGatewaySummary
    :: GatewaySummary
mkGatewaySummary
  = GatewaySummary'{arn = Core.Nothing, description = Core.Nothing,
                    gatewayGroupArn = Core.Nothing, name = Core.Nothing,
                    softwareVersion = Core.Nothing}

-- | The ARN of the gateway.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsArn :: Lens.Lens' GatewaySummary (Core.Maybe Types.Arn)
gsArn = Lens.field @"arn"
{-# INLINEABLE gsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsDescription :: Lens.Lens' GatewaySummary (Core.Maybe Types.Description)
gsDescription = Lens.field @"description"
{-# INLINEABLE gsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ARN of the gateway group that the gateway is associated to.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGatewayGroupArn :: Lens.Lens' GatewaySummary (Core.Maybe Types.GatewayGroupArn)
gsGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# INLINEABLE gsGatewayGroupArn #-}
{-# DEPRECATED gatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead"  #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsName :: Lens.Lens' GatewaySummary (Core.Maybe Types.Name)
gsName = Lens.field @"name"
{-# INLINEABLE gsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSoftwareVersion :: Lens.Lens' GatewaySummary (Core.Maybe Types.SoftwareVersion)
gsSoftwareVersion = Lens.field @"softwareVersion"
{-# INLINEABLE gsSoftwareVersion #-}
{-# DEPRECATED softwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead"  #-}

instance Core.FromJSON GatewaySummary where
        parseJSON
          = Core.withObject "GatewaySummary" Core.$
              \ x ->
                GatewaySummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Description" Core.<*>
                    x Core..:? "GatewayGroupArn"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "SoftwareVersion"
