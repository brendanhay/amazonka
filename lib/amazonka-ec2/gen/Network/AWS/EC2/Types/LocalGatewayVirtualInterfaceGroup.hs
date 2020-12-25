{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    lgvigLocalGatewayVirtualInterfaceGroupId,
    lgvigLocalGatewayVirtualInterfaceIds,
    lgvigOwnerId,
    lgvigTags,
  )
where

import qualified Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceGroupId as Types
import qualified Network.AWS.EC2.Types.LocalGatewayVirtualInterfaceId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a local gateway virtual interface group.
--
-- /See:/ 'mkLocalGatewayVirtualInterfaceGroup' smart constructor.
data LocalGatewayVirtualInterfaceGroup = LocalGatewayVirtualInterfaceGroup'
  { -- | The ID of the local gateway.
    localGatewayId :: Core.Maybe Types.String,
    -- | The ID of the virtual interface group.
    localGatewayVirtualInterfaceGroupId :: Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId,
    -- | The IDs of the virtual interfaces.
    localGatewayVirtualInterfaceIds :: Core.Maybe [Types.LocalGatewayVirtualInterfaceId],
    -- | The AWS account ID that owns the local gateway virtual interface group.
    ownerId :: Core.Maybe Types.String,
    -- | The tags assigned to the virtual interface group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LocalGatewayVirtualInterfaceGroup' value with any optional fields omitted.
mkLocalGatewayVirtualInterfaceGroup ::
  LocalGatewayVirtualInterfaceGroup
mkLocalGatewayVirtualInterfaceGroup =
  LocalGatewayVirtualInterfaceGroup'
    { localGatewayId = Core.Nothing,
      localGatewayVirtualInterfaceGroupId = Core.Nothing,
      localGatewayVirtualInterfaceIds = Core.Nothing,
      ownerId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID of the local gateway.
--
-- /Note:/ Consider using 'localGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Core.Maybe Types.String)
lgvigLocalGatewayId = Lens.field @"localGatewayId"
{-# DEPRECATED lgvigLocalGatewayId "Use generic-lens or generic-optics with 'localGatewayId' instead." #-}

-- | The ID of the virtual interface group.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayVirtualInterfaceGroupId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Core.Maybe Types.LocalGatewayVirtualInterfaceGroupId)
lgvigLocalGatewayVirtualInterfaceGroupId = Lens.field @"localGatewayVirtualInterfaceGroupId"
{-# DEPRECATED lgvigLocalGatewayVirtualInterfaceGroupId "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceGroupId' instead." #-}

-- | The IDs of the virtual interfaces.
--
-- /Note:/ Consider using 'localGatewayVirtualInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigLocalGatewayVirtualInterfaceIds :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Core.Maybe [Types.LocalGatewayVirtualInterfaceId])
lgvigLocalGatewayVirtualInterfaceIds = Lens.field @"localGatewayVirtualInterfaceIds"
{-# DEPRECATED lgvigLocalGatewayVirtualInterfaceIds "Use generic-lens or generic-optics with 'localGatewayVirtualInterfaceIds' instead." #-}

-- | The AWS account ID that owns the local gateway virtual interface group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigOwnerId :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Core.Maybe Types.String)
lgvigOwnerId = Lens.field @"ownerId"
{-# DEPRECATED lgvigOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The tags assigned to the virtual interface group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvigTags :: Lens.Lens' LocalGatewayVirtualInterfaceGroup (Core.Maybe [Types.Tag])
lgvigTags = Lens.field @"tags"
{-# DEPRECATED lgvigTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML LocalGatewayVirtualInterfaceGroup where
  parseXML x =
    LocalGatewayVirtualInterfaceGroup'
      Core.<$> (x Core..@? "localGatewayId")
      Core.<*> (x Core..@? "localGatewayVirtualInterfaceGroupId")
      Core.<*> ( x Core..@? "localGatewayVirtualInterfaceIdSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
