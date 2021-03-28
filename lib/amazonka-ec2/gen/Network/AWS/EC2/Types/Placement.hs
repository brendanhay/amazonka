{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Placement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Placement
  ( Placement (..)
  -- * Smart constructor
  , mkPlacement
  -- * Lenses
  , pAffinity
  , pAvailabilityZone
  , pGroupName
  , pHostId
  , pHostResourceGroupArn
  , pPartitionNumber
  , pSpreadDomain
  , pTenancy
  ) where

import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the placement of an instance.
--
-- /See:/ 'mkPlacement' smart constructor.
data Placement = Placement'
  { affinity :: Core.Maybe Core.Text
    -- ^ The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , groupName :: Core.Maybe Core.Text
    -- ^ The name of the placement group the instance is in.
  , hostId :: Core.Maybe Core.Text
    -- ^ The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , hostResourceGroupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , partitionNumber :: Core.Maybe Core.Int
    -- ^ The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , spreadDomain :: Core.Maybe Core.Text
    -- ^ Reserved for future use.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  , tenancy :: Core.Maybe Types.Tenancy
    -- ^ The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Placement' value with any optional fields omitted.
mkPlacement
    :: Placement
mkPlacement
  = Placement'{affinity = Core.Nothing,
               availabilityZone = Core.Nothing, groupName = Core.Nothing,
               hostId = Core.Nothing, hostResourceGroupArn = Core.Nothing,
               partitionNumber = Core.Nothing, spreadDomain = Core.Nothing,
               tenancy = Core.Nothing}

-- | The affinity setting for the instance on the Dedicated Host. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'affinity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAffinity :: Lens.Lens' Placement (Core.Maybe Core.Text)
pAffinity = Lens.field @"affinity"
{-# INLINEABLE pAffinity #-}
{-# DEPRECATED affinity "Use generic-lens or generic-optics with 'affinity' instead"  #-}

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for you based on the load balancing criteria for the Region.
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pAvailabilityZone :: Lens.Lens' Placement (Core.Maybe Core.Text)
pAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE pAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The name of the placement group the instance is in.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGroupName :: Lens.Lens' Placement (Core.Maybe Core.Text)
pGroupName = Lens.field @"groupName"
{-# INLINEABLE pGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The ID of the Dedicated Host on which the instance resides. This parameter is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostId :: Lens.Lens' Placement (Core.Maybe Core.Text)
pHostId = Lens.field @"hostId"
{-# INLINEABLE pHostId #-}
{-# DEPRECATED hostId "Use generic-lens or generic-optics with 'hostId' instead"  #-}

-- | The ARN of the host resource group in which to launch the instances. If you specify a host resource group ARN, omit the __Tenancy__ parameter or set it to @host@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'hostResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostResourceGroupArn :: Lens.Lens' Placement (Core.Maybe Core.Text)
pHostResourceGroupArn = Lens.field @"hostResourceGroupArn"
{-# INLINEABLE pHostResourceGroupArn #-}
{-# DEPRECATED hostResourceGroupArn "Use generic-lens or generic-optics with 'hostResourceGroupArn' instead"  #-}

-- | The number of the partition the instance is in. Valid only if the placement group strategy is set to @partition@ .
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'partitionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPartitionNumber :: Lens.Lens' Placement (Core.Maybe Core.Int)
pPartitionNumber = Lens.field @"partitionNumber"
{-# INLINEABLE pPartitionNumber #-}
{-# DEPRECATED partitionNumber "Use generic-lens or generic-optics with 'partitionNumber' instead"  #-}

-- | Reserved for future use.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'spreadDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSpreadDomain :: Lens.Lens' Placement (Core.Maybe Core.Text)
pSpreadDomain = Lens.field @"spreadDomain"
{-# INLINEABLE pSpreadDomain #-}
{-# DEPRECATED spreadDomain "Use generic-lens or generic-optics with 'spreadDomain' instead"  #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for the <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance> command.
--
-- This parameter is not supported by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTenancy :: Lens.Lens' Placement (Core.Maybe Types.Tenancy)
pTenancy = Lens.field @"tenancy"
{-# INLINEABLE pTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

instance Core.ToQuery Placement where
        toQuery Placement{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Affinity") affinity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "HostId") hostId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HostResourceGroupArn")
                hostResourceGroupArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PartitionNumber")
                partitionNumber
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpreadDomain")
                spreadDomain
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Tenancy") tenancy

instance Core.FromXML Placement where
        parseXML x
          = Placement' Core.<$>
              (x Core..@? "affinity") Core.<*> x Core..@? "availabilityZone"
                Core.<*> x Core..@? "groupName"
                Core.<*> x Core..@? "hostId"
                Core.<*> x Core..@? "hostResourceGroupArn"
                Core.<*> x Core..@? "partitionNumber"
                Core.<*> x Core..@? "spreadDomain"
                Core.<*> x Core..@? "tenancy"
