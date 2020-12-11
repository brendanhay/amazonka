-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupReference
  ( SecurityGroupReference (..),

    -- * Smart constructor
    mkSecurityGroupReference,

    -- * Lenses
    sgrVPCPeeringConnectionId,
    sgrReferencingVPCId,
    sgrGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a VPC with a security group that references your security group.
--
-- /See:/ 'mkSecurityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    referencingVPCId :: Lude.Maybe Lude.Text,
    groupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroupReference' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of your security group.
-- * 'referencingVPCId' - The ID of the VPC with the referencing security group.
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
mkSecurityGroupReference ::
  SecurityGroupReference
mkSecurityGroupReference =
  SecurityGroupReference'
    { vpcPeeringConnectionId = Lude.Nothing,
      referencingVPCId = Lude.Nothing,
      groupId = Lude.Nothing
    }

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrVPCPeeringConnectionId :: Lens.Lens' SecurityGroupReference (Lude.Maybe Lude.Text)
sgrVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: SecurityGroupReference -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: SecurityGroupReference)
{-# DEPRECATED sgrVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The ID of the VPC with the referencing security group.
--
-- /Note:/ Consider using 'referencingVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrReferencingVPCId :: Lens.Lens' SecurityGroupReference (Lude.Maybe Lude.Text)
sgrReferencingVPCId = Lens.lens (referencingVPCId :: SecurityGroupReference -> Lude.Maybe Lude.Text) (\s a -> s {referencingVPCId = a} :: SecurityGroupReference)
{-# DEPRECATED sgrReferencingVPCId "Use generic-lens or generic-optics with 'referencingVPCId' instead." #-}

-- | The ID of your security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrGroupId :: Lens.Lens' SecurityGroupReference (Lude.Maybe Lude.Text)
sgrGroupId = Lens.lens (groupId :: SecurityGroupReference -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: SecurityGroupReference)
{-# DEPRECATED sgrGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.FromXML SecurityGroupReference where
  parseXML x =
    SecurityGroupReference'
      Lude.<$> (x Lude..@? "vpcPeeringConnectionId")
      Lude.<*> (x Lude..@? "referencingVpcId")
      Lude.<*> (x Lude..@? "groupId")
