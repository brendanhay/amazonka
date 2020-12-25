{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SubnetGroup
  ( SubnetGroup (..),

    -- * Smart constructor
    mkSubnetGroup,

    -- * Lenses
    sgDescription,
    sgSubnetGroupName,
    sgSubnets,
    sgVpcId,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
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
  { -- | The description of the subnet group.
    description :: Core.Maybe Types.String,
    -- | The name of the subnet group.
    subnetGroupName :: Core.Maybe Types.String,
    -- | A list of subnets associated with the subnet group.
    subnets :: Core.Maybe [Types.Subnet],
    -- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetGroup' value with any optional fields omitted.
mkSubnetGroup ::
  SubnetGroup
mkSubnetGroup =
  SubnetGroup'
    { description = Core.Nothing,
      subnetGroupName = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SubnetGroup (Core.Maybe Types.String)
sgDescription = Lens.field @"description"
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnetGroupName :: Lens.Lens' SubnetGroup (Core.Maybe Types.String)
sgSubnetGroupName = Lens.field @"subnetGroupName"
{-# DEPRECATED sgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | A list of subnets associated with the subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnets :: Lens.Lens' SubnetGroup (Core.Maybe [Types.Subnet])
sgSubnets = Lens.field @"subnets"
{-# DEPRECATED sgSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVpcId :: Lens.Lens' SubnetGroup (Core.Maybe Types.String)
sgVpcId = Lens.field @"vpcId"
{-# DEPRECATED sgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON SubnetGroup where
  parseJSON =
    Core.withObject "SubnetGroup" Core.$
      \x ->
        SubnetGroup'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "SubnetGroupName")
          Core.<*> (x Core..:? "Subnets")
          Core.<*> (x Core..:? "VpcId")
