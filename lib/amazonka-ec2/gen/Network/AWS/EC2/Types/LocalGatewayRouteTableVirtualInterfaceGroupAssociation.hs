{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  ( LocalGatewayRouteTableVirtualInterfaceGroupAssociation (..),

    -- * Smart constructor
    mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation,

    -- * Lenses
    lgrtvigaState,
    lgrtvigaLocalGatewayRouteTableARN,
    lgrtvigaLocalGatewayId,
    lgrtvigaOwnerId,
    lgrtvigaLocalGatewayRouteTableId,
    lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId,
    lgrtvigaLocalGatewayVirtualInterfaceGroupId,
    lgrtvigaTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an association between a local gateway route table and a virtual interface group.
--
-- /See:/ 'mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation' smart constructor.
data LocalGatewayRouteTableVirtualInterfaceGroupAssociation = LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
  { -- | The state of the association.
    state :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the local gateway route table for the virtual interface group.
    localGatewayRouteTableARN :: Lude.Maybe Lude.Text,
    -- | The ID of the local gateway.
    localGatewayId :: Lude.Maybe Lude.Text,
    -- | The AWS account ID that owns the local gateway virtual interface group association.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Lude.Maybe Lude.Text,
    -- | The ID of the association.
    localGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lude.Maybe Lude.Text,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Lude.Maybe Lude.Text,
    -- | The tags assigned to the association.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalGatewayRouteTableVirtualInterfaceGroupAssociation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the association.
-- * 'localGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table for the virtual interface group.
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'ownerId' - The AWS account ID that owns the local gateway virtual interface group association.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'localGatewayRouteTableVirtualInterfaceGroupAssociationId' - The ID of the association.
-- * 'localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
-- * 'tags' - The tags assigned to the association.
mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation ::
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation
mkLocalGatewayRouteTableVirtualInterfaceGroupAssociation =
  LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
    { state =
        Lude.Nothing,
      localGatewayRouteTableARN =
        Lude.Nothing,
      localGatewayId = Lude.Nothing,
      ownerId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationId =
        Lude.Nothing,
      localGatewayVirtualInterfaceGroupId =
        Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the association.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaState :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaState = Lens.lens (state :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table for the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayRouteTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableARN :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaLocalGatewayRouteTableARN = Lens.lens (localGatewayRouteTableARN :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableARN = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaLocalGatewayRouteTableARN "Use generic-lens or generic-optics with 'localGatewayRouteTableARN' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaLocalGatewayId = Lens.lens (localGatewayId :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The AWS account ID that owns the local gateway virtual interface group association.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaOwnerId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaOwnerId = Lens.lens (ownerId :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The ID of the association.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId = Lens.lens (localGatewayRouteTableVirtualInterfaceGroupAssociationId :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationId "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociationId' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe Lude.Text)
lgrtvigaLocalGatewayVirtualInterfaceGroupId = Lens.lens (localGatewayVirtualInterfaceGroupId :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

-- | The tags assigned to the association.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtvigaTags :: Lens.Lens' LocalGatewayRouteTableVirtualInterfaceGroupAssociation (Lude.Maybe [Tag])
lgrtvigaTags = Lens.lens (tags :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGatewayRouteTableVirtualInterfaceGroupAssociation)
{-# DEPRECATED lgrtvigaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance
  Lude.FromXML
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation
  where
  parseXML x =
    LocalGatewayRouteTableVirtualInterfaceGroupAssociation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "localGatewayRouteTableArn")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableId")
      Lude.<*> ( x
                   Lude..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationId"
               )
      Lude.<*> (x Lude..@? "localGatewayVirtualInterfaceGroupId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
