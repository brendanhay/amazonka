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
    dsgVPCId,
    dsgOwnerId,
    dsgDBSecurityGroupARN,
    dsgIPRanges,
    dsgDBSecurityGroupName,
    dsgEC2SecurityGroups,
    dsgDBSecurityGroupDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.IPRange

-- | Contains the details for an Amazon RDS DB security group.
--
-- This data type is used as a response element in the @DescribeDBSecurityGroups@ action.
--
-- /See:/ 'mkDBSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { -- | Provides the VpcId of the DB security group.
    vpcId :: Lude.Maybe Lude.Text,
    -- | Provides the AWS ID of the owner of a specific DB security group.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the DB security group.
    dbSecurityGroupARN :: Lude.Maybe Lude.Text,
    -- | Contains a list of @IPRange@ elements.
    ipRanges :: Lude.Maybe [IPRange],
    -- | Specifies the name of the DB security group.
    dbSecurityGroupName :: Lude.Maybe Lude.Text,
    -- | Contains a list of @EC2SecurityGroup@ elements.
    ec2SecurityGroups :: Lude.Maybe [EC2SecurityGroup],
    -- | Provides the description of the DB security group.
    dbSecurityGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBSecurityGroup' with the minimum fields required to make a request.
--
-- * 'vpcId' - Provides the VpcId of the DB security group.
-- * 'ownerId' - Provides the AWS ID of the owner of a specific DB security group.
-- * 'dbSecurityGroupARN' - The Amazon Resource Name (ARN) for the DB security group.
-- * 'ipRanges' - Contains a list of @IPRange@ elements.
-- * 'dbSecurityGroupName' - Specifies the name of the DB security group.
-- * 'ec2SecurityGroups' - Contains a list of @EC2SecurityGroup@ elements.
-- * 'dbSecurityGroupDescription' - Provides the description of the DB security group.
mkDBSecurityGroup ::
  DBSecurityGroup
mkDBSecurityGroup =
  DBSecurityGroup'
    { vpcId = Lude.Nothing,
      ownerId = Lude.Nothing,
      dbSecurityGroupARN = Lude.Nothing,
      ipRanges = Lude.Nothing,
      dbSecurityGroupName = Lude.Nothing,
      ec2SecurityGroups = Lude.Nothing,
      dbSecurityGroupDescription = Lude.Nothing
    }

-- | Provides the VpcId of the DB security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgVPCId :: Lens.Lens' DBSecurityGroup (Lude.Maybe Lude.Text)
dsgVPCId = Lens.lens (vpcId :: DBSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DBSecurityGroup)
{-# DEPRECATED dsgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Provides the AWS ID of the owner of a specific DB security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgOwnerId :: Lens.Lens' DBSecurityGroup (Lude.Maybe Lude.Text)
dsgOwnerId = Lens.lens (ownerId :: DBSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: DBSecurityGroup)
{-# DEPRECATED dsgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB security group.
--
-- /Note:/ Consider using 'dbSecurityGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSecurityGroupARN :: Lens.Lens' DBSecurityGroup (Lude.Maybe Lude.Text)
dsgDBSecurityGroupARN = Lens.lens (dbSecurityGroupARN :: DBSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSecurityGroupARN = a} :: DBSecurityGroup)
{-# DEPRECATED dsgDBSecurityGroupARN "Use generic-lens or generic-optics with 'dbSecurityGroupARN' instead." #-}

-- | Contains a list of @IPRange@ elements.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgIPRanges :: Lens.Lens' DBSecurityGroup (Lude.Maybe [IPRange])
dsgIPRanges = Lens.lens (ipRanges :: DBSecurityGroup -> Lude.Maybe [IPRange]) (\s a -> s {ipRanges = a} :: DBSecurityGroup)
{-# DEPRECATED dsgIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

-- | Specifies the name of the DB security group.
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSecurityGroupName :: Lens.Lens' DBSecurityGroup (Lude.Maybe Lude.Text)
dsgDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: DBSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: DBSecurityGroup)
{-# DEPRECATED dsgDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

-- | Contains a list of @EC2SecurityGroup@ elements.
--
-- /Note:/ Consider using 'ec2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgEC2SecurityGroups :: Lens.Lens' DBSecurityGroup (Lude.Maybe [EC2SecurityGroup])
dsgEC2SecurityGroups = Lens.lens (ec2SecurityGroups :: DBSecurityGroup -> Lude.Maybe [EC2SecurityGroup]) (\s a -> s {ec2SecurityGroups = a} :: DBSecurityGroup)
{-# DEPRECATED dsgEC2SecurityGroups "Use generic-lens or generic-optics with 'ec2SecurityGroups' instead." #-}

-- | Provides the description of the DB security group.
--
-- /Note:/ Consider using 'dbSecurityGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgDBSecurityGroupDescription :: Lens.Lens' DBSecurityGroup (Lude.Maybe Lude.Text)
dsgDBSecurityGroupDescription = Lens.lens (dbSecurityGroupDescription :: DBSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbSecurityGroupDescription = a} :: DBSecurityGroup)
{-# DEPRECATED dsgDBSecurityGroupDescription "Use generic-lens or generic-optics with 'dbSecurityGroupDescription' instead." #-}

instance Lude.FromXML DBSecurityGroup where
  parseXML x =
    DBSecurityGroup'
      Lude.<$> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "OwnerId")
      Lude.<*> (x Lude..@? "DBSecurityGroupArn")
      Lude.<*> ( x Lude..@? "IPRanges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "IPRange")
               )
      Lude.<*> (x Lude..@? "DBSecurityGroupName")
      Lude.<*> ( x Lude..@? "EC2SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EC2SecurityGroup")
               )
      Lude.<*> (x Lude..@? "DBSecurityGroupDescription")
