{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.GatewayInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.GatewayInfo
  ( GatewayInfo (..),

    -- * Smart constructor
    mkGatewayInfo,

    -- * Lenses
    giEc2InstanceId,
    giEc2InstanceRegion,
    giGatewayARN,
    giGatewayId,
    giGatewayName,
    giGatewayOperationalState,
    giGatewayType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.Ec2InstanceId as Types
import qualified Network.AWS.StorageGateway.Types.Ec2InstanceRegion as Types
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.GatewayId as Types
import qualified Network.AWS.StorageGateway.Types.GatewayOperationalState as Types
import qualified Network.AWS.StorageGateway.Types.GatewayType as Types
import qualified Network.AWS.StorageGateway.Types.String as Types

-- | Describes a gateway object.
--
-- /See:/ 'mkGatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
  { -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Core.Maybe Types.Ec2InstanceId,
    -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Core.Maybe Types.Ec2InstanceRegion,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
    gatewayId :: Core.Maybe Types.GatewayId,
    -- | The name of the gateway.
    gatewayName :: Core.Maybe Types.String,
    -- | The state of the gateway.
    --
    -- Valid Values: @DISABLED@ | @ACTIVE@
    gatewayOperationalState :: Core.Maybe Types.GatewayOperationalState,
    -- | The type of the gateway.
    gatewayType :: Core.Maybe Types.GatewayType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GatewayInfo' value with any optional fields omitted.
mkGatewayInfo ::
  GatewayInfo
mkGatewayInfo =
  GatewayInfo'
    { ec2InstanceId = Core.Nothing,
      ec2InstanceRegion = Core.Nothing,
      gatewayARN = Core.Nothing,
      gatewayId = Core.Nothing,
      gatewayName = Core.Nothing,
      gatewayOperationalState = Core.Nothing,
      gatewayType = Core.Nothing
    }

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giEc2InstanceId :: Lens.Lens' GatewayInfo (Core.Maybe Types.Ec2InstanceId)
giEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED giEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The AWS Region where the Amazon EC2 instance is located.
--
-- /Note:/ Consider using 'ec2InstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giEc2InstanceRegion :: Lens.Lens' GatewayInfo (Core.Maybe Types.Ec2InstanceRegion)
giEc2InstanceRegion = Lens.field @"ec2InstanceRegion"
{-# DEPRECATED giEc2InstanceRegion "Use generic-lens or generic-optics with 'ec2InstanceRegion' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayARN :: Lens.Lens' GatewayInfo (Core.Maybe Types.GatewayARN)
giGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED giGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayId :: Lens.Lens' GatewayInfo (Core.Maybe Types.GatewayId)
giGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED giGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayName :: Lens.Lens' GatewayInfo (Core.Maybe Types.String)
giGatewayName = Lens.field @"gatewayName"
{-# DEPRECATED giGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
--
-- /Note:/ Consider using 'gatewayOperationalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayOperationalState :: Lens.Lens' GatewayInfo (Core.Maybe Types.GatewayOperationalState)
giGatewayOperationalState = Lens.field @"gatewayOperationalState"
{-# DEPRECATED giGatewayOperationalState "Use generic-lens or generic-optics with 'gatewayOperationalState' instead." #-}

-- | The type of the gateway.
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayType :: Lens.Lens' GatewayInfo (Core.Maybe Types.GatewayType)
giGatewayType = Lens.field @"gatewayType"
{-# DEPRECATED giGatewayType "Use generic-lens or generic-optics with 'gatewayType' instead." #-}

instance Core.FromJSON GatewayInfo where
  parseJSON =
    Core.withObject "GatewayInfo" Core.$
      \x ->
        GatewayInfo'
          Core.<$> (x Core..:? "Ec2InstanceId")
          Core.<*> (x Core..:? "Ec2InstanceRegion")
          Core.<*> (x Core..:? "GatewayARN")
          Core.<*> (x Core..:? "GatewayId")
          Core.<*> (x Core..:? "GatewayName")
          Core.<*> (x Core..:? "GatewayOperationalState")
          Core.<*> (x Core..:? "GatewayType")
