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
    sgVPCId,
    sgSubnets,
    sgSubnetGroupName,
    sgDescription,
  )
where

import Network.AWS.DAX.Types.Subnet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { vpcId :: Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Subnet],
    subnetGroupName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubnetGroup' with the minimum fields required to make a request.
--
-- * 'description' - The description of the subnet group.
-- * 'subnetGroupName' - The name of the subnet group.
-- * 'subnets' - A list of subnets associated with the subnet group.
-- * 'vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
mkSubnetGroup ::
  SubnetGroup
mkSubnetGroup =
  SubnetGroup'
    { vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      subnetGroupName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVPCId :: Lens.Lens' SubnetGroup (Lude.Maybe Lude.Text)
sgVPCId = Lens.lens (vpcId :: SubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: SubnetGroup)
{-# DEPRECATED sgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of subnets associated with the subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnets :: Lens.Lens' SubnetGroup (Lude.Maybe [Subnet])
sgSubnets = Lens.lens (subnets :: SubnetGroup -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: SubnetGroup)
{-# DEPRECATED sgSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The name of the subnet group.
--
-- /Note:/ Consider using 'subnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSubnetGroupName :: Lens.Lens' SubnetGroup (Lude.Maybe Lude.Text)
sgSubnetGroupName = Lens.lens (subnetGroupName :: SubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {subnetGroupName = a} :: SubnetGroup)
{-# DEPRECATED sgSubnetGroupName "Use generic-lens or generic-optics with 'subnetGroupName' instead." #-}

-- | The description of the subnet group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SubnetGroup (Lude.Maybe Lude.Text)
sgDescription = Lens.lens (description :: SubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SubnetGroup)
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON SubnetGroup where
  parseJSON =
    Lude.withObject
      "SubnetGroup"
      ( \x ->
          SubnetGroup'
            Lude.<$> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "Subnets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubnetGroupName")
            Lude.<*> (x Lude..:? "Description")
      )
