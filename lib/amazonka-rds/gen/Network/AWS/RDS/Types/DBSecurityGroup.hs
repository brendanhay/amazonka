{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBSecurityGroup
  ( DBSecurityGroup (..)
  -- * Smart constructor
  , mkDBSecurityGroup
  -- * Lenses
  , dbsgDBSecurityGroupArn
  , dbsgDBSecurityGroupDescription
  , dbsgDBSecurityGroupName
  , dbsgEC2SecurityGroups
  , dbsgIPRanges
  , dbsgOwnerId
  , dbsgVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.EC2SecurityGroup as Types
import qualified Network.AWS.RDS.Types.IPRange as Types

-- | Contains the details for an Amazon RDS DB security group. 
--
-- This data type is used as a response element in the @DescribeDBSecurityGroups@ action. 
--
-- /See:/ 'mkDBSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { dBSecurityGroupArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the DB security group.
  , dBSecurityGroupDescription :: Core.Maybe Core.Text
    -- ^ Provides the description of the DB security group.
  , dBSecurityGroupName :: Core.Maybe Core.Text
    -- ^ Specifies the name of the DB security group.
  , eC2SecurityGroups :: Core.Maybe [Types.EC2SecurityGroup]
    -- ^ Contains a list of @EC2SecurityGroup@ elements. 
  , iPRanges :: Core.Maybe [Types.IPRange]
    -- ^ Contains a list of @IPRange@ elements. 
  , ownerId :: Core.Maybe Core.Text
    -- ^ Provides the AWS ID of the owner of a specific DB security group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ Provides the VpcId of the DB security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSecurityGroup' value with any optional fields omitted.
mkDBSecurityGroup
    :: DBSecurityGroup
mkDBSecurityGroup
  = DBSecurityGroup'{dBSecurityGroupArn = Core.Nothing,
                     dBSecurityGroupDescription = Core.Nothing,
                     dBSecurityGroupName = Core.Nothing,
                     eC2SecurityGroups = Core.Nothing, iPRanges = Core.Nothing,
                     ownerId = Core.Nothing, vpcId = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupArn :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbsgDBSecurityGroupArn = Lens.field @"dBSecurityGroupArn"
{-# INLINEABLE dbsgDBSecurityGroupArn #-}
{-# DEPRECATED dBSecurityGroupArn "Use generic-lens or generic-optics with 'dBSecurityGroupArn' instead"  #-}

-- | Provides the description of the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupDescription :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbsgDBSecurityGroupDescription = Lens.field @"dBSecurityGroupDescription"
{-# INLINEABLE dbsgDBSecurityGroupDescription #-}
{-# DEPRECATED dBSecurityGroupDescription "Use generic-lens or generic-optics with 'dBSecurityGroupDescription' instead"  #-}

-- | Specifies the name of the DB security group.
--
-- /Note:/ Consider using 'dBSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgDBSecurityGroupName :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbsgDBSecurityGroupName = Lens.field @"dBSecurityGroupName"
{-# INLINEABLE dbsgDBSecurityGroupName #-}
{-# DEPRECATED dBSecurityGroupName "Use generic-lens or generic-optics with 'dBSecurityGroupName' instead"  #-}

-- | Contains a list of @EC2SecurityGroup@ elements. 
--
-- /Note:/ Consider using 'eC2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgEC2SecurityGroups :: Lens.Lens' DBSecurityGroup (Core.Maybe [Types.EC2SecurityGroup])
dbsgEC2SecurityGroups = Lens.field @"eC2SecurityGroups"
{-# INLINEABLE dbsgEC2SecurityGroups #-}
{-# DEPRECATED eC2SecurityGroups "Use generic-lens or generic-optics with 'eC2SecurityGroups' instead"  #-}

-- | Contains a list of @IPRange@ elements. 
--
-- /Note:/ Consider using 'iPRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgIPRanges :: Lens.Lens' DBSecurityGroup (Core.Maybe [Types.IPRange])
dbsgIPRanges = Lens.field @"iPRanges"
{-# INLINEABLE dbsgIPRanges #-}
{-# DEPRECATED iPRanges "Use generic-lens or generic-optics with 'iPRanges' instead"  #-}

-- | Provides the AWS ID of the owner of a specific DB security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgOwnerId :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbsgOwnerId = Lens.field @"ownerId"
{-# INLINEABLE dbsgOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Provides the VpcId of the DB security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsgVpcId :: Lens.Lens' DBSecurityGroup (Core.Maybe Core.Text)
dbsgVpcId = Lens.field @"vpcId"
{-# INLINEABLE dbsgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML DBSecurityGroup where
        parseXML x
          = DBSecurityGroup' Core.<$>
              (x Core..@? "DBSecurityGroupArn") Core.<*>
                x Core..@? "DBSecurityGroupDescription"
                Core.<*> x Core..@? "DBSecurityGroupName"
                Core.<*>
                x Core..@? "EC2SecurityGroups" Core..<@>
                  Core.parseXMLList "EC2SecurityGroup"
                Core.<*>
                x Core..@? "IPRanges" Core..<@> Core.parseXMLList "IPRange"
                Core.<*> x Core..@? "OwnerId"
                Core.<*> x Core..@? "VpcId"
