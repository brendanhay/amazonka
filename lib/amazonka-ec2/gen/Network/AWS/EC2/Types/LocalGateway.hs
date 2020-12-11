-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGateway
  ( LocalGateway (..),

    -- * Smart constructor
    mkLocalGateway,

    -- * Lenses
    lgState,
    lgLocalGatewayId,
    lgOutpostARN,
    lgOwnerId,
    lgTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a local gateway.
--
-- /See:/ 'mkLocalGateway' smart constructor.
data LocalGateway = LocalGateway'
  { state :: Lude.Maybe Lude.Text,
    localGatewayId :: Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LocalGateway' with the minimum fields required to make a request.
--
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'ownerId' - The AWS account ID that owns the local gateway.
-- * 'state' - The state of the local gateway.
-- * 'tags' - The tags assigned to the local gateway.
mkLocalGateway ::
  LocalGateway
mkLocalGateway =
  LocalGateway'
    { state = Lude.Nothing,
      localGatewayId = Lude.Nothing,
      outpostARN = Lude.Nothing,
      ownerId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the local gateway.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgState :: Lens.Lens' LocalGateway (Lude.Maybe Lude.Text)
lgState = Lens.lens (state :: LocalGateway -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: LocalGateway)
{-# DEPRECATED lgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLocalGatewayId :: Lens.Lens' LocalGateway (Lude.Maybe Lude.Text)
lgLocalGatewayId = Lens.lens (localGatewayId :: LocalGateway -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGateway)
{-# DEPRECATED lgLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOutpostARN :: Lens.Lens' LocalGateway (Lude.Maybe Lude.Text)
lgOutpostARN = Lens.lens (outpostARN :: LocalGateway -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: LocalGateway)
{-# DEPRECATED lgOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The AWS account ID that owns the local gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgOwnerId :: Lens.Lens' LocalGateway (Lude.Maybe Lude.Text)
lgOwnerId = Lens.lens (ownerId :: LocalGateway -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGateway)
{-# DEPRECATED lgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The tags assigned to the local gateway.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgTags :: Lens.Lens' LocalGateway (Lude.Maybe [Tag])
lgTags = Lens.lens (tags :: LocalGateway -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGateway)
{-# DEPRECATED lgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LocalGateway where
  parseXML x =
    LocalGateway'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
