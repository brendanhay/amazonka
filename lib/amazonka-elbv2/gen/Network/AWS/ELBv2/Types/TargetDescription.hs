{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetDescription
  ( TargetDescription (..),

    -- * Smart constructor
    mkTargetDescription,

    -- * Lenses
    tdId,
    tdAvailabilityZone,
    tdPort,
  )
where

import qualified Network.AWS.ELBv2.Types.TargetId as Types
import qualified Network.AWS.ELBv2.Types.ZoneName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a target.
--
-- /See:/ 'mkTargetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { -- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
    id :: Types.TargetId,
    -- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer.
    --
    -- This parameter is not supported if the target type of the target group is @instance@ .
    -- If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required.
    -- With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ .
    -- If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
    availabilityZone :: Core.Maybe Types.ZoneName,
    -- | The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
    port :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetDescription' value with any optional fields omitted.
mkTargetDescription ::
  -- | 'id'
  Types.TargetId ->
  TargetDescription
mkTargetDescription id =
  TargetDescription'
    { id,
      availabilityZone = Core.Nothing,
      port = Core.Nothing
    }

-- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdId :: Lens.Lens' TargetDescription Types.TargetId
tdId = Lens.field @"id"
{-# DEPRECATED tdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer.
--
-- This parameter is not supported if the target type of the target group is @instance@ .
-- If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required.
-- With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ .
-- If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAvailabilityZone :: Lens.Lens' TargetDescription (Core.Maybe Types.ZoneName)
tdAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED tdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPort :: Lens.Lens' TargetDescription (Core.Maybe Core.Natural)
tdPort = Lens.field @"port"
{-# DEPRECATED tdPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Core.FromXML TargetDescription where
  parseXML x =
    TargetDescription'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@? "AvailabilityZone")
      Core.<*> (x Core..@? "Port")
