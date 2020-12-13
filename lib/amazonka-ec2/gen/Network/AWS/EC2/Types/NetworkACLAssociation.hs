{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkACLAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkACLAssociation
  ( NetworkACLAssociation (..),

    -- * Smart constructor
    mkNetworkACLAssociation,

    -- * Lenses
    naaNetworkACLId,
    naaSubnetId,
    naaNetworkACLAssociationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a network ACL and a subnet.
--
-- /See:/ 'mkNetworkACLAssociation' smart constructor.
data NetworkACLAssociation = NetworkACLAssociation'
  { -- | The ID of the network ACL.
    networkACLId :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The ID of the association between a network ACL and a subnet.
    networkACLAssociationId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkACLAssociation' with the minimum fields required to make a request.
--
-- * 'networkACLId' - The ID of the network ACL.
-- * 'subnetId' - The ID of the subnet.
-- * 'networkACLAssociationId' - The ID of the association between a network ACL and a subnet.
mkNetworkACLAssociation ::
  NetworkACLAssociation
mkNetworkACLAssociation =
  NetworkACLAssociation'
    { networkACLId = Lude.Nothing,
      subnetId = Lude.Nothing,
      networkACLAssociationId = Lude.Nothing
    }

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaNetworkACLId :: Lens.Lens' NetworkACLAssociation (Lude.Maybe Lude.Text)
naaNetworkACLId = Lens.lens (networkACLId :: NetworkACLAssociation -> Lude.Maybe Lude.Text) (\s a -> s {networkACLId = a} :: NetworkACLAssociation)
{-# DEPRECATED naaNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaSubnetId :: Lens.Lens' NetworkACLAssociation (Lude.Maybe Lude.Text)
naaSubnetId = Lens.lens (subnetId :: NetworkACLAssociation -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NetworkACLAssociation)
{-# DEPRECATED naaSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the association between a network ACL and a subnet.
--
-- /Note:/ Consider using 'networkACLAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaNetworkACLAssociationId :: Lens.Lens' NetworkACLAssociation (Lude.Maybe Lude.Text)
naaNetworkACLAssociationId = Lens.lens (networkACLAssociationId :: NetworkACLAssociation -> Lude.Maybe Lude.Text) (\s a -> s {networkACLAssociationId = a} :: NetworkACLAssociation)
{-# DEPRECATED naaNetworkACLAssociationId "Use generic-lens or generic-optics with 'networkACLAssociationId' instead." #-}

instance Lude.FromXML NetworkACLAssociation where
  parseXML x =
    NetworkACLAssociation'
      Lude.<$> (x Lude..@? "networkAclId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "networkAclAssociationId")
