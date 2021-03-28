{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotPlacement
  ( SpotPlacement (..)
  -- * Smart constructor
  , mkSpotPlacement
  -- * Lenses
  , spAvailabilityZone
  , spGroupName
  , spTenancy
  ) where

import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes Spot Instance placement.
--
-- /See:/ 'mkSpotPlacement' smart constructor.
data SpotPlacement = SpotPlacement'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
  , groupName :: Core.Maybe Core.Text
    -- ^ The name of the placement group.
  , tenancy :: Core.Maybe Types.Tenancy
    -- ^ The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotPlacement' value with any optional fields omitted.
mkSpotPlacement
    :: SpotPlacement
mkSpotPlacement
  = SpotPlacement'{availabilityZone = Core.Nothing,
                   groupName = Core.Nothing, tenancy = Core.Nothing}

-- | The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAvailabilityZone :: Lens.Lens' SpotPlacement (Core.Maybe Core.Text)
spAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE spAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spGroupName :: Lens.Lens' SpotPlacement (Core.Maybe Core.Text)
spGroupName = Lens.field @"groupName"
{-# INLINEABLE spGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTenancy :: Lens.Lens' SpotPlacement (Core.Maybe Types.Tenancy)
spTenancy = Lens.field @"tenancy"
{-# INLINEABLE spTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

instance Core.ToQuery SpotPlacement where
        toQuery SpotPlacement{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
              availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Tenancy") tenancy

instance Core.FromXML SpotPlacement where
        parseXML x
          = SpotPlacement' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "groupName"
                Core.<*> x Core..@? "tenancy"
