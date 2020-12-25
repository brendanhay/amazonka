{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Placement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Placement
  ( Placement (..),

    -- * Smart constructor
    mkPlacement,

    -- * Lenses
    pAffinity,
    pAvailabilityZone,
    pGroupName,
    pHostId,
    pHostResourceGroupArn,
    pPartitionNumber,
    pSpreadDomain,
    pTenancy,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement of an instance.
--
-- /See:/ 'mkPlacement' smart constructor.
data Placement = Placement'
  { -- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    affinity :: Core.Maybe Types.String,
    -- | The Availability Zone of the instance.
    --
    -- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    availabilityZone :: Core.Maybe Types.String,
    -- | The name of the placement group the instance is in.
    groupName :: Core.Maybe Types.String,
    -- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    hostId :: Core.Maybe Types.String,
    -- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    hostResourceGroupArn :: Core.Maybe Types.String,
    -- | The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    partitionNumber :: Core.Maybe Core.Int,
    -- | Reserved for future use.
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    spreadDomain :: Core.Maybe Types.String,
    -- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
    --
    -- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
    tenancy :: Core.Maybe Types.Tenancy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Placement' value with any optional fields omitted.
mkPlacement ::
  Placement
mkPlacement =
  Placement'
    { affinity = Core.Nothing,
      availabilityZone = Core.Nothing,
      groupName = Core.Nothing,
      hostId = Core.Nothing,
      hostResourceGroupArn = Core.Nothing,
      partitionNumber = Core.Nothing,
      spreadDomain = Core.Nothing,
      tenancy = Core.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAffinity :: Lens.Lens' Placement (Core.Maybe Types.String)
pAffinity = Lens.field @"affinity"
{-# DEPRECATED pAffinity "Use generic-lens or generic-optics with 'affinity' instead." #-}

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAvailabilityZone :: Lens.Lens' Placement (Core.Maybe Types.String)
pAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED pAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The name of the placement group the instance is in.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGroupName :: Lens.Lens' Placement (Core.Maybe Types.String)
pGroupName = Lens.field @"groupName"
{-# DEPRECATED pGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostId :: Lens.Lens' Placement (Core.Maybe Types.String)
pHostId = Lens.field @"hostId"
{-# DEPRECATED pHostId "Use generic-lens or generic-optics with 'hostId' instead." #-}

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostResourceGroupArn :: Lens.Lens' Placement (Core.Maybe Types.String)
pHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# DEPRECATED pHostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead." #-}

-- | The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPartitionNumber :: Lens.Lens' Placement (Core.Maybe Core.Int)
pPartitionNumber = Lens.field @"partitionNumber"
{-# DEPRECATED pPartitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead." #-}

-- | Reserved for future use.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSpreadDomain :: Lens.Lens' Placement (Core.Maybe Types.String)
pSpreadDomain = Lens.field @"spreadDomain"
{-# DEPRECATED pSpreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTenancy :: Lens.Lens' Placement (Core.Maybe Types.Tenancy)
pTenancy = Lens.field @"tenancy"
{-# DEPRECATED pTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

instance Core.FromXML Placement where
  parseXML x =
    Placement'
      Core.<$> (x Core..@? "affinity")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "groupName")
      Core.<*> (x Core..@? "hostId")
      Core.<*> (x Core..@? "hostResourceGroupArn")
      Core.<*> (x Core..@? "partitionNumber")
      Core.<*> (x Core..@? "spreadDomain")
      Core.<*> (x Core..@? "tenancy")
