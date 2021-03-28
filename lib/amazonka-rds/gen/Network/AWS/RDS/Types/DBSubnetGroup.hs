{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBSubnetGroup
  ( DBSubnetGroup (..)
  -- * Smart constructor
  , mkDBSubnetGroup
  -- * Lenses
  , dDBSubnetGroupArn
  , dDBSubnetGroupDescription
  , dDBSubnetGroupName
  , dSubnetGroupStatus
  , dSubnets
  , dVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Subnet as Types

-- | Contains the details of an Amazon RDS DB subnet group. 
--
-- This data type is used as a response element in the @DescribeDBSubnetGroups@ action. 
--
-- /See:/ 'mkDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { dBSubnetGroupArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the DB subnet group.
  , dBSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ Provides the description of the DB subnet group.
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB subnet group.
  , subnetGroupStatus :: Core.Maybe Core.Text
    -- ^ Provides the status of the DB subnet group.
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ Contains a list of @Subnet@ elements. 
  , vpcId :: Core.Maybe Core.Text
    -- ^ Provides the VpcId of the DB subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSubnetGroup' value with any optional fields omitted.
mkDBSubnetGroup
    :: DBSubnetGroup
mkDBSubnetGroup
  = DBSubnetGroup'{dBSubnetGroupArn = Core.Nothing,
                   dBSubnetGroupDescription = Core.Nothing,
                   dBSubnetGroupName = Core.Nothing, subnetGroupStatus = Core.Nothing,
                   subnets = Core.Nothing, vpcId = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupArn :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dDBSubnetGroupArn = Lens.field @"dBSubnetGroupArn"
{-# INLINEABLE dDBSubnetGroupArn #-}
{-# DEPRECATED dBSubnetGroupArn "Use generic-lens or generic-optics with 'dBSubnetGroupArn' instead"  #-}

-- | Provides the description of the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dDBSubnetGroupDescription = Lens.field @"dBSubnetGroupDescription"
{-# INLINEABLE dDBSubnetGroupDescription #-}
{-# DEPRECATED dBSubnetGroupDescription "Use generic-lens or generic-optics with 'dBSubnetGroupDescription' instead"  #-}

-- | The name of the DB subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDBSubnetGroupName :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE dDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | Provides the status of the DB subnet group.
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubnetGroupStatus :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dSubnetGroupStatus = Lens.field @"subnetGroupStatus"
{-# INLINEABLE dSubnetGroupStatus #-}
{-# DEPRECATED subnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead"  #-}

-- | Contains a list of @Subnet@ elements. 
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSubnets :: Lens.Lens' DBSubnetGroup (Core.Maybe [Types.Subnet])
dSubnets = Lens.field @"subnets"
{-# INLINEABLE dSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | Provides the VpcId of the DB subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVpcId :: Lens.Lens' DBSubnetGroup (Core.Maybe Core.Text)
dVpcId = Lens.field @"vpcId"
{-# INLINEABLE dVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML DBSubnetGroup where
        parseXML x
          = DBSubnetGroup' Core.<$>
              (x Core..@? "DBSubnetGroupArn") Core.<*>
                x Core..@? "DBSubnetGroupDescription"
                Core.<*> x Core..@? "DBSubnetGroupName"
                Core.<*> x Core..@? "SubnetGroupStatus"
                Core.<*> x Core..@? "Subnets" Core..<@> Core.parseXMLList "Subnet"
                Core.<*> x Core..@? "VpcId"
