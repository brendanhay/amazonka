{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSubnetGroup
  ( DBSubnetGroup (..),

    -- * Smart constructor
    mkDBSubnetGroup,

    -- * Lenses
    dDBSubnetGroupArn,
    dDBSubnetGroupDescription,
    dDBSubnetGroupName,
    dSubnetGroupStatus,
    dSubnets,
    dVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.Subnet as Types

-- | Contains the details of an Amazon RDS DB subnet group.
--
-- This data type is used as a response element in the @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'mkDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { -- | The Amazon Resource Name (ARN) for the DB subnet group.
    dBSubnetGroupArn :: Core.Maybe Types.String,
    -- | Provides the description of the DB subnet group.
    dBSubnetGroupDescription :: Core.Maybe Types.String,
    -- | The name of the DB subnet group.
    dBSubnetGroupName :: Core.Maybe Types.String,
    -- | Provides the status of the DB subnet group.
    subnetGroupStatus :: Core.Maybe Types.String,
    -- | Contains a list of @Subnet@ elements.
    subnets :: Core.Maybe [Types.Subnet],
    -- | Provides the VpcId of the DB subnet group.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSubnetGroup' value with any optional fields omitted.
mkDBSubnetGroup ::
  DBSubnetGroup
mkDBSubnetGroup =
  DBSubnetGroup'
    { dBSubnetGroupArn = Core.Nothing,
      dBSubnetGroupDescription = Core.Nothing,
      dBSubnetGroupName = Core.Nothing,
      subnetGroupStatus = Core.Nothing,
      subnets = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Core.Maybe Types.String)
dDBSubnetGroupArn = Lens.field @"dBSubnetGroupArn"
{-# DEPRECATED dDBSubnetGroupArn "Use generic-lens or generic-optics with 'dBSubnetGroupArn' instead." #-}

-- | Provides the description of the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Core.Maybe Types.String)
dDBSubnetGroupDescription = Lens.field @"dBSubnetGroupDescription"
{-# DEPRECATED dDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dBSubnetGroupDescription' instead." #-}

-- | The name of the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupName :: Lens.Lens' DBSubnetGroup (Core.Maybe Types.String)
dDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# DEPRECATED dDBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead." #-}

-- | Provides the status of the DB subnet group.
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubnetGroupStatus :: Lens.Lens' DBSubnetGroup (Core.Maybe Types.String)
dSubnetGroupStatus = Lens.field @"subnetGroupStatus"
{-# DEPRECATED dSubnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead." #-}

-- | Contains a list of @Subnet@ elements.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubnets :: Lens.Lens' DBSubnetGroup (Core.Maybe [Types.Subnet])
dSubnets = Lens.field @"subnets"
{-# DEPRECATED dSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | Provides the VpcId of the DB subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVpcId :: Lens.Lens' DBSubnetGroup (Core.Maybe Types.String)
dVpcId = Lens.field @"vpcId"
{-# DEPRECATED dVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Core.<$> (x Core..@? "DBSubnetGroupArn")
      Core.<*> (x Core..@? "DBSubnetGroupDescription")
      Core.<*> (x Core..@? "DBSubnetGroupName")
      Core.<*> (x Core..@? "SubnetGroupStatus")
      Core.<*> (x Core..@? "Subnets" Core..<@> Core.parseXMLList "Subnet")
      Core.<*> (x Core..@? "VpcId")
