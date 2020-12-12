{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkACL
  ( NetworkACL (..),

    -- * Smart constructor
    mkNetworkACL,

    -- * Lenses
    naEntries,
    naNetworkACLId,
    naVPCId,
    naOwnerId,
    naAssociations,
    naTags,
    naIsDefault,
  )
where

import Network.AWS.EC2.Types.NetworkACLAssociation
import Network.AWS.EC2.Types.NetworkACLEntry
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network ACL.
--
-- /See:/ 'mkNetworkACL' smart constructor.
data NetworkACL = NetworkACL'
  { entries ::
      Lude.Maybe [NetworkACLEntry],
    networkACLId :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    associations :: Lude.Maybe [NetworkACLAssociation],
    tags :: Lude.Maybe [Tag],
    isDefault :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkACL' with the minimum fields required to make a request.
--
-- * 'associations' - Any associations between the network ACL and one or more subnets
-- * 'entries' - One or more entries (rules) in the network ACL.
-- * 'isDefault' - Indicates whether this is the default network ACL for the VPC.
-- * 'networkACLId' - The ID of the network ACL.
-- * 'ownerId' - The ID of the AWS account that owns the network ACL.
-- * 'tags' - Any tags assigned to the network ACL.
-- * 'vpcId' - The ID of the VPC for the network ACL.
mkNetworkACL ::
  NetworkACL
mkNetworkACL =
  NetworkACL'
    { entries = Lude.Nothing,
      networkACLId = Lude.Nothing,
      vpcId = Lude.Nothing,
      ownerId = Lude.Nothing,
      associations = Lude.Nothing,
      tags = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | One or more entries (rules) in the network ACL.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naEntries :: Lens.Lens' NetworkACL (Lude.Maybe [NetworkACLEntry])
naEntries = Lens.lens (entries :: NetworkACL -> Lude.Maybe [NetworkACLEntry]) (\s a -> s {entries = a} :: NetworkACL)
{-# DEPRECATED naEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naNetworkACLId :: Lens.Lens' NetworkACL (Lude.Maybe Lude.Text)
naNetworkACLId = Lens.lens (networkACLId :: NetworkACL -> Lude.Maybe Lude.Text) (\s a -> s {networkACLId = a} :: NetworkACL)
{-# DEPRECATED naNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

-- | The ID of the VPC for the network ACL.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naVPCId :: Lens.Lens' NetworkACL (Lude.Maybe Lude.Text)
naVPCId = Lens.lens (vpcId :: NetworkACL -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: NetworkACL)
{-# DEPRECATED naVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the AWS account that owns the network ACL.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naOwnerId :: Lens.Lens' NetworkACL (Lude.Maybe Lude.Text)
naOwnerId = Lens.lens (ownerId :: NetworkACL -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: NetworkACL)
{-# DEPRECATED naOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Any associations between the network ACL and one or more subnets
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naAssociations :: Lens.Lens' NetworkACL (Lude.Maybe [NetworkACLAssociation])
naAssociations = Lens.lens (associations :: NetworkACL -> Lude.Maybe [NetworkACLAssociation]) (\s a -> s {associations = a} :: NetworkACL)
{-# DEPRECATED naAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | Any tags assigned to the network ACL.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naTags :: Lens.Lens' NetworkACL (Lude.Maybe [Tag])
naTags = Lens.lens (tags :: NetworkACL -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: NetworkACL)
{-# DEPRECATED naTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether this is the default network ACL for the VPC.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naIsDefault :: Lens.Lens' NetworkACL (Lude.Maybe Lude.Bool)
naIsDefault = Lens.lens (isDefault :: NetworkACL -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: NetworkACL)
{-# DEPRECATED naIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML NetworkACL where
  parseXML x =
    NetworkACL'
      Lude.<$> ( x Lude..@? "entrySet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "networkAclId")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "associationSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "default")
