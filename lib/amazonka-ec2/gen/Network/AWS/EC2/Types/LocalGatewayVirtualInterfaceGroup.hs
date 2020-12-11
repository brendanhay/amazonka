-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroup
  ( LocalGatewayVirtualInterfaceGroup (..),

    -- * Smart constructor
    mkLocalGatewayVirtualInterfaceGroup,

    -- * Lenses
    lgvigLocalGatewayId,
    lgvigOwnerId,
    lgvigLocalGatewayVirtualInterfaceIds,
    lgvigLocalGatewayVirtualInterfaceGroupId,
    lgvigTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a local gateway virtual interface group.
--
-- /See:/ 'mkLocalGatewayVirtualInterfaceGroup' smart constructor.
data LocalGatewayVirtualInterfaceGroup = LocalGatewayVirtualInterfaceGroup'
  { localGatewayId ::
      Lude.Maybe Lude.Text,
    ownerId ::
      Lude.Maybe Lude.Text,
    localGatewayVirtualInterfaceIds ::
      Lude.Maybe [Lude.Text],
    localGatewayVirtualInterfaceGroupId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LocalGatewayVirtualInterfaceGroup' with the minimum fields required to make a request.
--
-- * 'localGatewayId' - The ID of the local gateway.
-- * 'localGatewayVirtualInterfaceGroupId' - The ID of the virtual interface group.
-- * 'localGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
-- * 'ownerId' - The AWS account ID that owns the local gateway virtual interface group.
-- * 'tags' - The tags assigned to the virtual interface group.
mkLocalGatewayVirtualInterfaceGroup ::
  LocalGatewayVirtualInterfaceGroup
mkLocalGatewayVirtualInterfaceGroup =
  LocalGatewayVirtualInterfaceGroup'
    { localGatewayId = Lude.Nothing,
      ownerId = Lude.Nothing,
      localGatewayVirtualInterfaceIds = Lude.Nothing,
      localGatewayVirtualInterfaceGroupId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Lude.Maybe Lude.Text)
lgvigLocalGatewayId = Lens.lens (localGatewayId :: LocalGatewayVirtualInterfaceGroup -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayId = a} :: LocalGatewayVirtualInterfaceGroup)
{-# DEPRECATED lgvigLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The AWS account ID that owns the local gateway virtual interface group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigOwnerId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Lude.Maybe Lude.Text)
lgvigOwnerId = Lens.lens (ownerId :: LocalGatewayVirtualInterfaceGroup -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: LocalGatewayVirtualInterfaceGroup)
{-# DEPRECATED lgvigOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The IDs of the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayVirtualInterfaceIds :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Lude.Maybe [Lude.Text])
lgvigLocalGatewayVirtualInterfaceIds = Lens.lens (localGatewayVirtualInterfaceIds :: LocalGatewayVirtualInterfaceGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayVirtualInterfaceIds = a} :: LocalGatewayVirtualInterfaceGroup)
{-# DEPRECATED lgvigLocalGatewayVirtualInterfaceIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceIds' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Lude.Maybe Lude.Text)
lgvigLocalGatewayVirtualInterfaceGroupId = Lens.lens (localGatewayVirtualInterfaceGroupId :: LocalGatewayVirtualInterfaceGroup -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayVirtualInterfaceGroupId = a} :: LocalGatewayVirtualInterfaceGroup)
{-# DEPRECATED lgvigLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

-- | The tags assigned to the virtual interface group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigTags :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Lude.Maybe [Tag])
lgvigTags = Lens.lens (tags :: LocalGatewayVirtualInterfaceGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LocalGatewayVirtualInterfaceGroup)
{-# DEPRECATED lgvigTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LocalGatewayVirtualInterfaceGroup where
  parseXML x =
    LocalGatewayVirtualInterfaceGroup'
      Lude.<$> (x Lude..@? "localGatewayId")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> ( x Lude..@? "localGatewayVirtualInterfaceIdSet"
                   Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "localGatewayVirtualInterfaceGroupId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
