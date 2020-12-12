{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayRouteTable
  ( LocalGatewayRouteTable (..),

    -- * Smart constructor
    mkLocalGatewayRouteTable,

    -- * Lenses
    lgrtState,
    lgrtLocalGatewayRouteTableARN,
    lgrtLocalGatewayId,
    lgrtOutpostARN,
    lgrtOwnerId,
    lgrtLocalGatewayRouteTableId,
    lgrtTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a local gateway route table.
--
-- /See:/ 'mkLocalGatewayRouteTable' smart constructor.
data LocalGatewayRouteTable = LocalGatewayRouteTable'
  { state ::
      Lude.Maybe Lude.Text,
    localGatewayRouteTableARN ::
      Lude.Maybe Lude.Text,
    localGatewayId :: Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    localGatewayRouteTableId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalGatewayRouteTable' with the minimum fields required to make a request.
--
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'localGatewayRouteTableARN' - The Amazon Resource Name (ARN) of the local gateway route table.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'ownerId' - The AWS account ID that owns the local gateway route table.
-- * 'state' - The state of the local gateway route table.
-- * 'tags' - The tags assigned to the local gateway route table.
mkLocalGatewayRouteTable ::
  LocalGatewayRouteTable
mkLocalGatewayRouteTable =
  LocalGatewayRouteTable'
    { state = Lude.Nothing,
      localGatewayRouteTableARN = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      outpostARN = Lude.Nothing,
      ownerId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the local gateway route table.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtState :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtState = Lens.lens (state :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Amazon Resource Name (ARN) of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtLocalGatewayRouteTableARN :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtLocalGatewayRouteTableARN = Lens.lens (localGatewayRouteTableARN :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableARN = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtLocalGatewayRouteTableARN "Use generic-lens or generic-optics with 'localGatewayRouteTableARN' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtLocalGatewayId :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtLocalGatewayId = Lens.lens (localGatewayId :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtOutpostARN :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtOutpostARN = Lens.lens (outpostARN :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The AWS account ID that owns the local gateway route table.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtOwnerId :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtOwnerId = Lens.lens (ownerId :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtLocalGatewayRouteTableId :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe Lude.Text)
lgrtLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: LocalGatewayRouteTable -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The tags assigned to the local gateway route table.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrtTags :: Lens.Lens' LocalGatewayRouteTable (Lude.Maybe [Tag])
lgrtTags = Lens.lens (tags :: LocalGatewayRouteTable -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGatewayRouteTable)
{-# DEPRECATED lgrtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LocalGatewayRouteTable where
  parseXML x =
    LocalGatewayRouteTable'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "localGatewayRouteTableArn")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
