{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVPCAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTableVPCAssociation
  ( LocalGatewayRouteTableVPCAssociation (..),

    -- * Smart constructor
    mkLocalGatewayRouteTableVPCAssociation,

    -- * Lenses
    lgrtvaState,
    lgrtvaLocalGatewayRouteTableARN,
    lgrtvaVPCId,
    lgrtvaLocalGatewayId,
    lgrtvaLocalGatewayRouteTableVPCAssociationId,
    lgrtvaOwnerId,
    lgrtvaLocalGatewayRouteTableId,
    lgrtvaTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a local gateway route table and a VPC.
--
-- /See:/ 'mkLocalGatewayRouteTableVPCAssociation' smart constructor.
data LocalGatewayRouteTableVPCAssociation = LocalGatewayRouteTableVPCAssociation'
  { state ::
      Lude.Maybe
        Lude.Text,
    localGatewayRouteTableARN ::
      Lude.Maybe
        Lude.Text,
    vpcId ::
      Lude.Maybe
        Lude.Text,
    localGatewayId ::
      Lude.Maybe
        Lude.Text,
    localGatewayRouteTableVPCAssociationId ::
      Lude.Maybe
        Lude.Text,
    ownerId ::
      Lude.Maybe
        Lude.Text,
    localGatewayRouteTableId ::
      Lude.Maybe
        Lude.Text,
    tags ::
      Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalGatewayRouteTableVPCAssociation' with the minimum fields required to make a request.
--
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'localGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table for the association.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'localGatewayRouteTableVPCAssociationId' - The ID of the association.
-- * 'ownerId' - The AWS account ID that owns the local gateway route table for the association.
-- * 'state' - The state of the association.
-- * 'tags' - The tags assigned to the association.
-- * 'vpcId' - The ID of the VPC.
mkLocalGatewayRouteTableVPCAssociation ::
  LocalGatewayRouteTableVPCAssociation
mkLocalGatewayRouteTableVPCAssociation =
  LocalGatewayRouteTableVPCAssociation'
    { state = Lude.Nothing,
      localGatewayRouteTableARN = Lude.Nothing,
      vpcId = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      localGatewayRouteTableVPCAssociationId = Lude.Nothing,
      ownerId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaState :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaState = Lens.lens (state :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table for the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableARN :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaLocalGatewayRouteTableARN = Lens.lens (localGatewayRouteTableARN :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableARN = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaLocalGatewayRouteTableARN "Use generic-lens or generic-optics with 'localGatewayRouteTableARN' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaVPCId :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaVPCId = Lens.lens (vpcId :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayId :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaLocalGatewayId = Lens.lens (localGatewayId :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableVPCAssociationId :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaLocalGatewayRouteTableVPCAssociationId = Lens.lens (localGatewayRouteTableVPCAssociationId :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableVPCAssociationId = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaLocalGatewayRouteTableVPCAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociationId' instead." #-}

-- | The AWS account ID that owns the local gateway route table for the association.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaOwnerId :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaOwnerId = Lens.lens (ownerId :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe Lude.Text)
lgrtvaLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The tags assigned to the association.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvaTags :: Lens.Lens' LocalGatewayRouteTableVPCAssociation (Lude.Maybe [Tag])
lgrtvaTags = Lens.lens (tags :: LocalGatewayRouteTableVPCAssociation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGatewayRouteTableVPCAssociation)
{-# DEPRECATED lgrtvaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LocalGatewayRouteTableVPCAssociation where
  parseXML x =
    LocalGatewayRouteTableVPCAssociation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "localGatewayRouteTableArn")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableVpcAssociationId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
