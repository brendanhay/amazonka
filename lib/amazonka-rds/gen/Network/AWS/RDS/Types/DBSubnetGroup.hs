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
    dsgDBSubnetGroupName,
    dsgVPCId,
    dsgSubnets,
    dsgDBSubnetGroupDescription,
    dsgDBSubnetGroupARN,
    dsgSubnetGroupStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.Subnet

-- | Contains the details of an Amazon RDS DB subnet group.
--
-- This data type is used as a response element in the @DescribeDBSubnetGroups@ action.
--
-- /See:/ 'mkDBSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { dbSubnetGroupName ::
      Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Subnet],
    dbSubnetGroupDescription :: Lude.Maybe Lude.Text,
    dbSubnetGroupARN :: Lude.Maybe Lude.Text,
    subnetGroupStatus :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBSubnetGroup' with the minimum fields required to make a request.
--
-- * 'dbSubnetGroupARN' - The Amazon Resource Name (ARN) for the DB subnet group.
-- * 'dbSubnetGroupDescription' - Provides the description of the DB subnet group.
-- * 'dbSubnetGroupName' - The name of the DB subnet group.
-- * 'subnetGroupStatus' - Provides the status of the DB subnet group.
-- * 'subnets' - Contains a list of @Subnet@ elements.
-- * 'vpcId' - Provides the VpcId of the DB subnet group.
mkDBSubnetGroup ::
  DBSubnetGroup
mkDBSubnetGroup =
  DBSubnetGroup'
    { dbSubnetGroupName = Lude.Nothing,
      vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      dbSubnetGroupDescription = Lude.Nothing,
      dbSubnetGroupARN = Lude.Nothing,
      subnetGroupStatus = Lude.Nothing
    }

-- | The name of the DB subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSubnetGroupName :: Lens.Lens' DBSubnetGroup (Lude.Maybe Lude.Text)
dsgDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: DBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: DBSubnetGroup)
{-# DEPRECATED dsgDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | Provides the VpcId of the DB subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgVPCId :: Lens.Lens' DBSubnetGroup (Lude.Maybe Lude.Text)
dsgVPCId = Lens.lens (vpcId :: DBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DBSubnetGroup)
{-# DEPRECATED dsgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Contains a list of @Subnet@ elements.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnets :: Lens.Lens' DBSubnetGroup (Lude.Maybe [Subnet])
dsgSubnets = Lens.lens (subnets :: DBSubnetGroup -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: DBSubnetGroup)
{-# DEPRECATED dsgSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | Provides the description of the DB subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSubnetGroupDescription :: Lens.Lens' DBSubnetGroup (Lude.Maybe Lude.Text)
dsgDBSubnetGroupDescription = Lens.lens (dbSubnetGroupDescription :: DBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupDescription = a} :: DBSubnetGroup)
{-# DEPRECATED dsgDBSubnetGroupDescription "Use generic-lens or generic-optics with 'dbSubnetGroupDescription' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSubnetGroupARN :: Lens.Lens' DBSubnetGroup (Lude.Maybe Lude.Text)
dsgDBSubnetGroupARN = Lens.lens (dbSubnetGroupARN :: DBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupARN = a} :: DBSubnetGroup)
{-# DEPRECATED dsgDBSubnetGroupARN "Use generic-lens or generic-optics with 'dbSubnetGroupARN' instead." #-}

-- | Provides the status of the DB subnet group.
--
-- /Note:/ Consider using 'subnetGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSubnetGroupStatus :: Lens.Lens' DBSubnetGroup (Lude.Maybe Lude.Text)
dsgSubnetGroupStatus = Lens.lens (subnetGroupStatus :: DBSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroupStatus = a} :: DBSubnetGroup)
{-# DEPRECATED dsgSubnetGroupStatus "Use generic-lens or generic-optics with 'subnetGroupStatus' instead." #-}

instance Lude.FromXML DBSubnetGroup where
  parseXML x =
    DBSubnetGroup'
      Lude.<$> (x Lude..@? "DBSubnetGroupName")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Subnet")
               )
      Lude.<*> (x Lude..@? "DBSubnetGroupDescription")
      Lude.<*> (x Lude..@? "DBSubnetGroupArn")
      Lude.<*> (x Lude..@? "SubnetGroupStatus")
