{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PhysicalConnectionRequirements
  ( PhysicalConnectionRequirements (..)
  -- * Smart constructor
  , mkPhysicalConnectionRequirements
  -- * Lenses
  , pcrAvailabilityZone
  , pcrSecurityGroupIdList
  , pcrSubnetId
  ) where

import qualified Network.AWS.Glue.Types.AvailabilityZone as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the physical requirements for a connection.
--
-- /See:/ 'mkPhysicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
  , securityGroupIdList :: Core.Maybe [Types.NameString]
    -- ^ The security group ID list used by the connection.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The subnet ID used by the connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhysicalConnectionRequirements' value with any optional fields omitted.
mkPhysicalConnectionRequirements
    :: PhysicalConnectionRequirements
mkPhysicalConnectionRequirements
  = PhysicalConnectionRequirements'{availabilityZone = Core.Nothing,
                                    securityGroupIdList = Core.Nothing, subnetId = Core.Nothing}

-- | The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrAvailabilityZone :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe Types.AvailabilityZone)
pcrAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE pcrAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The security group ID list used by the connection.
--
-- /Note:/ Consider using 'securityGroupIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrSecurityGroupIdList :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe [Types.NameString])
pcrSecurityGroupIdList = Lens.field @"securityGroupIdList"
{-# INLINEABLE pcrSecurityGroupIdList #-}
{-# DEPRECATED securityGroupIdList "Use generic-lens or generic-optics with 'securityGroupIdList' instead"  #-}

-- | The subnet ID used by the connection.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrSubnetId :: Lens.Lens' PhysicalConnectionRequirements (Core.Maybe Types.SubnetId)
pcrSubnetId = Lens.field @"subnetId"
{-# INLINEABLE pcrSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.FromJSON PhysicalConnectionRequirements where
        toJSON PhysicalConnectionRequirements{..}
          = Core.object
              (Core.catMaybes
                 [("AvailabilityZone" Core..=) Core.<$> availabilityZone,
                  ("SecurityGroupIdList" Core..=) Core.<$> securityGroupIdList,
                  ("SubnetId" Core..=) Core.<$> subnetId])

instance Core.FromJSON PhysicalConnectionRequirements where
        parseJSON
          = Core.withObject "PhysicalConnectionRequirements" Core.$
              \ x ->
                PhysicalConnectionRequirements' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*>
                    x Core..:? "SecurityGroupIdList"
                    Core.<*> x Core..:? "SubnetId"
