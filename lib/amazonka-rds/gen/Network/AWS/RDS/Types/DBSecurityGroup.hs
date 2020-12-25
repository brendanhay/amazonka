{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSecurityGroup
  ( DBSecurityGroup (..),

    -- * Smart constructor
    mkDBSecurityGroup,

    -- * Lenses
    dbsgDBSecurityGroupArn,
    dbsgDBSecurityGroupDescription,
    dbsgDBSecurityGroupName,
    dbsgEC2SecurityGroups,
    dbsgIPRanges,
    dbsgOwnerId,
    dbsgVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.DBSecurityGroupArn as Types
import qualified Network.AWS.RDS.Types.DBSecurityGroupDescription as Types
import qualified Network.AWS.RDS.Types.DBSecurityGroupName as Types
import qualified Network.AWS.RDS.Types.EC2SecurityGroup as Types
import qualified Network.AWS.RDS.Types.IPRange as Types
import qualified Network.AWS.RDS.Types.OwnerId as Types
import qualified Network.AWS.RDS.Types.VpcId as Types

-- | Contains the details for an Amazon RDS DB security group.
--
-- This data type is used as a response element in the @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'mkDBSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { -- | The Amazon Resource Name (ARN) for the DB security group.
    dBSecurityGroupArn :: Core.Maybe Types.DBSecurityGroupArn,
    -- | Provides the description of the DB security group.
    dBSecurityGroupDescription :: Core.Maybe Types.DBSecurityGroupDescription,
    -- | Specifies the name of the DB security group.
    dBSecurityGroupName :: Core.Maybe Types.DBSecurityGroupName,
    -- | Contains a list of @EC2SecurityGroup@ elements.
    eC2SecurityGroups :: Core.Maybe [Types.EC2SecurityGroup],
    -- | Contains a list of @IPRange@ elements.
    iPRanges :: Core.Maybe [Types.IPRange],
    -- | Provides the AWS ID of the owner of a specific DB security group.
    ownerId :: Core.Maybe Types.OwnerId,
    -- | Provides the VpcId of the DB security group.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSecurityGroup' value with any optional fields omitted.
mkDBSecurityGroup ::
  DBSecurityGroup
mkDBSecurityGroup =
  DBSecurityGroup'
    { dBSecurityGroupArn = Core.Nothing,
      dBSecurityGroupDescription = Core.Nothing,
      dBSecurityGroupName = Core.Nothing,
      eC2SecurityGroups = Core.Nothing,
      iPRanges = Core.Nothing,
      ownerId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupArn :: Lens.Lens' DBSecurityGroup (Core.Maybe Types.DBSecurityGroupArn)
dbsgDBSecurityGroupArn = Lens.field @"dBSecurityGroupArn"
{-# DEPRECATED dbsgDBSecurityGroupArn "Use generic-lens or generic-optics with 'dBSecurityGroupArn' instead." #-}

-- | Provides the description of the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupDescription :: Lens.Lens' DBSecurityGroup (Core.Maybe Types.DBSecurityGroupDescription)
dbsgDBSecurityGroupDescription = Lens.field @"dBSecurityGroupDescription"
{-# DEPRECATED dbsgDBSecurityGroupDescription "Use generic-lens or generic-optics with 'dBSecurityGroupDescription' instead." #-}

-- | Specifies the name of the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupName :: Lens.Lens' DBSecurityGroup (Core.Maybe Types.DBSecurityGroupName)
dbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# DEPRECATED dbsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead." #-}

-- | Contains a list of @EC2SecurityGroup@ elements.
--
-- /Note:/ Consider using 'eC2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgEC2SecurityGroups :: Lens.Lens' DBSecurityGroup (Core.Maybe [Types.EC2SecurityGroup])
dbsgEC2SecurityGroups = Lens.field @"eC2SecurityGroups"
{-# DEPRECATED dbsgEC2SecurityGroups "Use generic-lens or generic-optics with 'eC2SecurityGroups' instead." #-}

-- | Contains a list of @IPRange@ elements.
--
-- /Note:/ Consider using 'iPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgIPRanges :: Lens.Lens' DBSecurityGroup (Core.Maybe [Types.IPRange])
dbsgIPRanges = Lens.field @"iPRanges"
{-# DEPRECATED dbsgIPRanges "Use generic-lens or generic-optics with 'iPRanges' instead." #-}

-- | Provides the AWS ID of the owner of a specific DB security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgOwnerId :: Lens.Lens' DBSecurityGroup (Core.Maybe Types.OwnerId)
dbsgOwnerId = Lens.field @"ownerId"
{-# DEPRECATED dbsgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Provides the VpcId of the DB security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgVpcId :: Lens.Lens' DBSecurityGroup (Core.Maybe Types.VpcId)
dbsgVpcId = Lens.field @"vpcId"
{-# DEPRECATED dbsgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML DBSecurityGroup where
  parseXML x =
    DBSecurityGroup'
      Core.<$> (x Core..@? "DBSecurityGroupArn")
      Core.<*> (x Core..@? "DBSecurityGroupDescription")
      Core.<*> (x Core..@? "DBSecurityGroupName")
      Core.<*> ( x Core..@? "EC2SecurityGroups"
                   Core..<@> Core.parseXMLList "EC2SecurityGroup"
               )
      Core.<*> (x Core..@? "IPRanges" Core..<@> Core.parseXMLList "IPRange")
      Core.<*> (x Core..@? "OwnerId")
      Core.<*> (x Core..@? "VpcId")
