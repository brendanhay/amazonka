{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacement
  ( LaunchTemplatePlacement (..),

    -- * Smart constructor
    mkLaunchTemplatePlacement,

    -- * Lenses
    ltpAffinity,
    ltpAvailabilityZone,
    ltpGroupName,
    ltpHostId,
    ltpHostResourceGroupArn,
    ltpPartitionNumber,
    ltpSpreadDomain,
    ltpTenancy,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement of an instance.
--
-- /See:/ 'mkLaunchTemplatePlacement' smart constructor.
data LaunchTemplatePlacement = LaunchTemplatePlacement'
  { -- | The affinity setting for the instance on the Dedicated Host.
    affinity :: Core.Maybe Types.String,
    -- | The Availability Zone of the instance.
    availabilityZone :: Core.Maybe Types.String,
    -- | The name of the placement group for the instance.
    groupName :: Core.Maybe Types.String,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Core.Maybe Types.String,
    -- | The ARN of the host resource group in which to launch the instances.
    hostResourceGroupArn :: Core.Maybe Types.String,
    -- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
    partitionNumber :: Core.Maybe Core.Int,
    -- | Reserved for future use.
    spreadDomain :: Core.Maybe Types.String,
    -- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    tenancy :: Core.Maybe Types.Tenancy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplatePlacement' value with any optional fields omitted.
mkLaunchTemplatePlacement ::
  LaunchTemplatePlacement
mkLaunchTemplatePlacement =
  LaunchTemplatePlacement'
    { affinity = Core.Nothing,
      availabilityZone = Core.Nothing,
      groupName = Core.Nothing,
      hostId = Core.Nothing,
      hostResourceGroupArn = Core.Nothing,
      partitionNumber = Core.Nothing,
      spreadDomain = Core.Nothing,
      tenancy = Core.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpAffinity :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpAffinity = Lens.field @"affinity"
{-# DEPRECATED ltpAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The Availability Zone of the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpAvailabilityZone :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ltpAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the placement group for the instance.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpGroupName :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpGroupName = Lens.field @"groupName"
{-# DEPRECATED ltpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ID of the Dedicated Host for the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpHostId :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpHostId = Lens.field @"hostId"
{-# DEPRECATED ltpHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The ARN of the host resource group in which to launch the instances.
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpHostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# DEPRECATED ltpHostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead." #-}

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpPartitionNumber :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Core.Int)
ltpPartitionNumber = Lens.field @"partitionNumber"
{-# DEPRECATED ltpPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpSpreadDomain :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.String)
ltpSpreadDomain = Lens.field @"spreadDomain"
{-# DEPRECATED ltpSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpTenancy :: Lens.Lens' LaunchTemplatePlacement (Core.Maybe Types.Tenancy)
ltpTenancy = Lens.field @"tenancy"
{-# DEPRECATED ltpTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

instance Core.FromXML LaunchTemplatePlacement where
  parseXML x =
    LaunchTemplatePlacement'
      Core.<$> (x Core..@? "affinity")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "groupName")
      Core.<*> (x Core..@? "hostId")
      Core.<*> (x Core..@? "hostResourceGroupArn")
      Core.<*> (x Core..@? "partitionNumber")
      Core.<*> (x Core..@? "spreadDomain")
      Core.<*> (x Core..@? "tenancy")
