{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatePlacementRequest
  ( LaunchTemplatePlacementRequest (..),

    -- * Smart constructor
    mkLaunchTemplatePlacementRequest,

    -- * Lenses
    ltprAffinity,
    ltprAvailabilityZone,
    ltprGroupName,
    ltprHostId,
    ltprHostResourceGroupArn,
    ltprPartitionNumber,
    ltprSpreadDomain,
    ltprTenancy,
  )
where

import qualified Network.AWS.EC2.Types.GroupName as Types
import qualified Network.AWS.EC2.Types.HostId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement of an instance.
--
-- /See:/ 'mkLaunchTemplatePlacementRequest' smart constructor.
data LaunchTemplatePlacementRequest = LaunchTemplatePlacementRequest'
  { -- | The affinity setting for an instance on a Dedicated Host.
    affinity :: Core.Maybe Types.String,
    -- | The Availability Zone for the instance.
    availabilityZone :: Core.Maybe Types.String,
    -- | The name of the placement group for the instance.
    groupName :: Core.Maybe Types.GroupName,
    -- | The ID of the Dedicated Host for the instance.
    hostId :: Core.Maybe Types.HostId,
    -- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
    hostResourceGroupArn :: Core.Maybe Types.String,
    -- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
    partitionNumber :: Core.Maybe Core.Int,
    -- | Reserved for future use.
    spreadDomain :: Core.Maybe Types.String,
    -- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
    tenancy :: Core.Maybe Types.Tenancy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplatePlacementRequest' value with any optional fields omitted.
mkLaunchTemplatePlacementRequest ::
  LaunchTemplatePlacementRequest
mkLaunchTemplatePlacementRequest =
  LaunchTemplatePlacementRequest'
    { affinity = Core.Nothing,
      availabilityZone = Core.Nothing,
      groupName = Core.Nothing,
      hostId = Core.Nothing,
      hostResourceGroupArn = Core.Nothing,
      partitionNumber = Core.Nothing,
      spreadDomain = Core.Nothing,
      tenancy = Core.Nothing
    }

-- | The affinity setting for an instance on a Dedicated Host.
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprAffinity :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.String)
ltprAffinity = Lens.field @"affinity"
{-# DEPRECATED ltprAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprAvailabilityZone :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.String)
ltprAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ltprAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the placement group for the instance.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprGroupName :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.GroupName)
ltprGroupName = Lens.field @"groupName"
{-# DEPRECATED ltprGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ID of the Dedicated Host for the instance.
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprHostId :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.HostId)
ltprHostId = Lens.field @"hostId"
{-# DEPRECATED ltprHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprHostResourceGroupArn :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.String)
ltprHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# DEPRECATED ltprHostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead." #-}

-- | The number of the partition the instance should launch in. Valid only if the placement group strategy is set to @partition@ .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprPartitionNumber :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Core.Int)
ltprPartitionNumber = Lens.field @"partitionNumber"
{-# DEPRECATED ltprPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprSpreadDomain :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.String)
ltprSpreadDomain = Lens.field @"spreadDomain"
{-# DEPRECATED ltprSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of dedicated runs on single-tenant hardware.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprTenancy :: Lens.Lens' LaunchTemplatePlacementRequest (Core.Maybe Types.Tenancy)
ltprTenancy = Lens.field @"tenancy"
{-# DEPRECATED ltprTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}
