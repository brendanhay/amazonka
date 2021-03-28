{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Subnet
  ( Subnet (..)
  -- * Smart constructor
  , mkSubnet
  -- * Lenses
  , sSubnetAvailabilityZone
  , sSubnetIdentifier
  , sSubnetOutpost
  , sSubnetStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AvailabilityZone as Types
import qualified Network.AWS.RDS.Types.Outpost as Types

-- | This data type is used as a response element for the @DescribeDBSubnetGroups@ operation. 
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { subnetAvailabilityZone :: Core.Maybe Types.AvailabilityZone
  , subnetIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the subnet.
  , subnetOutpost :: Core.Maybe Types.Outpost
    -- ^ If the subnet is associated with an Outpost, this value specifies the Outpost.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./ 
  , subnetStatus :: Core.Maybe Core.Text
    -- ^ The status of the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subnet' value with any optional fields omitted.
mkSubnet
    :: Subnet
mkSubnet
  = Subnet'{subnetAvailabilityZone = Core.Nothing,
            subnetIdentifier = Core.Nothing, subnetOutpost = Core.Nothing,
            subnetStatus = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe Types.AvailabilityZone)
sSubnetAvailabilityZone = Lens.field @"subnetAvailabilityZone"
{-# INLINEABLE sSubnetAvailabilityZone #-}
{-# DEPRECATED subnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead"  #-}

-- | The identifier of the subnet.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Core.Text)
sSubnetIdentifier = Lens.field @"subnetIdentifier"
{-# INLINEABLE sSubnetIdentifier #-}
{-# DEPRECATED subnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead"  #-}

-- | If the subnet is associated with an Outpost, this value specifies the Outpost.
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'subnetOutpost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetOutpost :: Lens.Lens' Subnet (Core.Maybe Types.Outpost)
sSubnetOutpost = Lens.field @"subnetOutpost"
{-# INLINEABLE sSubnetOutpost #-}
{-# DEPRECATED subnetOutpost "Use generic-lens or generic-optics with 'subnetOutpost' instead"  #-}

-- | The status of the subnet.
--
-- /Note:/ Consider using 'subnetStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetStatus :: Lens.Lens' Subnet (Core.Maybe Core.Text)
sSubnetStatus = Lens.field @"subnetStatus"
{-# INLINEABLE sSubnetStatus #-}
{-# DEPRECATED subnetStatus "Use generic-lens or generic-optics with 'subnetStatus' instead"  #-}

instance Core.FromXML Subnet where
        parseXML x
          = Subnet' Core.<$>
              (x Core..@? "SubnetAvailabilityZone") Core.<*>
                x Core..@? "SubnetIdentifier"
                Core.<*> x Core..@? "SubnetOutpost"
                Core.<*> x Core..@? "SubnetStatus"
