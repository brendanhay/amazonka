{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.SubnetGroup
  ( SubnetGroup (..)
  -- * Smart constructor
  , mkSubnetGroup
  -- * Lenses
  , sgDescription
  , sgSubnetGroupName
  , sgSubnets
  , sgVpcId
  ) where

import qualified Network.AWS.DAX.Types.Subnet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of one of the following actions:
--
--
--     * /CreateSubnetGroup/ 
--
--
--     * /ModifySubnetGroup/ 
--
--
--
-- /See:/ 'mkSubnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the subnet group.
  , subnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the subnet group.
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ A list of subnets associated with the subnet group. 
  , vpcId :: Core.Maybe Core.Text
    -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetGroup' value with any optional fields omitted.
mkSubnetGroup
    :: SubnetGroup
mkSubnetGroup
  = SubnetGroup'{description = Core.Nothing,
                 subnetGroupName = Core.Nothing, subnets = Core.Nothing,
                 vpcId = Core.Nothing}

-- | The description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SubnetGroup (Core.Maybe Core.Text)
sgDescription = Lens.field @"description"
{-# INLINEABLE sgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnetGroupName :: Lens.Lens' SubnetGroup (Core.Maybe Core.Text)
sgSubnetGroupName = Lens.field @"subnetGroupName"
{-# INLINEABLE sgSubnetGroupName #-}
{-# DEPRECATED subnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead"  #-}

-- | A list of subnets associated with the subnet group. 
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnets :: Lens.Lens' SubnetGroup (Core.Maybe [Types.Subnet])
sgSubnets = Lens.field @"subnets"
{-# INLINEABLE sgSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVpcId :: Lens.Lens' SubnetGroup (Core.Maybe Core.Text)
sgVpcId = Lens.field @"vpcId"
{-# INLINEABLE sgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON SubnetGroup where
        parseJSON
          = Core.withObject "SubnetGroup" Core.$
              \ x ->
                SubnetGroup' Core.<$>
                  (x Core..:? "Description") Core.<*> x Core..:? "SubnetGroupName"
                    Core.<*> x Core..:? "Subnets"
                    Core.<*> x Core..:? "VpcId"
