{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRoute
  ( LocalGatewayRoute (..),

    -- * Smart constructor
    mkLocalGatewayRoute,

    -- * Lenses
    lgrState,
    lgrLocalGatewayRouteTableARN,
    lgrOwnerId,
    lgrLocalGatewayRouteTableId,
    lgrType,
    lgrLocalGatewayVirtualInterfaceGroupId,
    lgrDestinationCidrBlock,
  )
where

import Network.AWS.EC2.Types.LocalGatewayRouteState
import Network.AWS.EC2.Types.LocalGatewayRouteType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a route for a local gateway route table.
--
-- /See:/ 'mkLocalGatewayRoute' smart constructor.
data LocalGatewayRoute = LocalGatewayRoute'
  { -- | The state of the route.
    state :: Lude.Maybe LocalGatewayRouteState,
    -- | The Amazon Resource Name (ARN) of the local gateway route table.
    localGatewayRouteTableARN :: Lude.Maybe Lude.Text,
    -- | The AWS account ID that owns the local gateway route.
    ownerId :: Lude.Maybe Lude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Lude.Maybe Lude.Text,
    -- | The route type.
    type' :: Lude.Maybe LocalGatewayRouteType,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Lude.Maybe Lude.Text,
    -- | The CIDR block used for destination matches.
    destinationCidrBlock :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalGatewayRoute' with the minimum fields required to make a request.
--
-- * 'state' - The state of the route.
-- * 'localGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table.
-- * 'ownerId' - The AWS account ID that owns the local gateway route.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'type'' - The route type.
-- * 'localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
-- * 'destinationCidrBlock' - The CIDR block used for destination matches.
mkLocalGatewayRoute ::
  LocalGatewayRoute
mkLocalGatewayRoute =
  LocalGatewayRoute'
    { state = Lude.Nothing,
      localGatewayRouteTableARN = Lude.Nothing,
      ownerId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      type' = Lude.Nothing,
      localGatewayVirtualInterfaceGroupId = Lude.Nothing,
      destinationCidrBlock = Lude.Nothing
    }

-- | The state of the route.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrState :: Lens.Lens' LocalGatewayRoute (Lude.Maybe LocalGatewayRouteState)
lgrState = Lens.lens (state :: LocalGatewayRoute -> Lude.Maybe LocalGatewayRouteState) (\s a -> s {state = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayRouteTableARN :: Lens.Lens' LocalGatewayRoute (Lude.Maybe Lude.Text)
lgrLocalGatewayRouteTableARN = Lens.lens (localGatewayRouteTableARN :: LocalGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableARN = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrLocalGatewayRouteTableARN "Use generic-lens or generic-optics with 'localGatewayRouteTableARN' instead." #-}

-- | The AWS account ID that owns the local gateway route.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrOwnerId :: Lens.Lens' LocalGatewayRoute (Lude.Maybe Lude.Text)
lgrOwnerId = Lens.lens (ownerId :: LocalGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRoute (Lude.Maybe Lude.Text)
lgrLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: LocalGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrType :: Lens.Lens' LocalGatewayRoute (Lude.Maybe LocalGatewayRouteType)
lgrType = Lens.lens (type' :: LocalGatewayRoute -> Lude.Maybe LocalGatewayRouteType) (\s a -> s {type' = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayRoute (Lude.Maybe Lude.Text)
lgrLocalGatewayVirtualInterfaceGroupId = Lens.lens (localGatewayVirtualInterfaceGroupId :: LocalGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

-- | The CIDR block used for destination matches.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrDestinationCidrBlock :: Lens.Lens' LocalGatewayRoute (Lude.Maybe Lude.Text)
lgrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: LocalGatewayRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidrBlock = a} :: LocalGatewayRoute)
{-# DEPRECATED lgrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.FromXML LocalGatewayRoute where
  parseXML x =
    LocalGatewayRoute'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "localGatewayRouteTableArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableId")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "localGatewayVirtualInterfaceGroupId")
      Lude.<*> (x Lude..@? "destinationCidrBlock")
