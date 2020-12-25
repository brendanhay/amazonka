{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentGroupList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentGroupList
  ( SegmentGroupList (..),

    -- * Smart constructor
    mkSegmentGroupList,

    -- * Lenses
    sglGroups,
    sglInclude,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.Include as Types
import qualified Network.AWS.Pinpoint.Types.SegmentGroup as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings that define the relationships between segment groups for a segment.
--
-- /See:/ 'mkSegmentGroupList' smart constructor.
data SegmentGroupList = SegmentGroupList'
  { -- | An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
    groups :: Core.Maybe [Types.SegmentGroup],
    -- | Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
    include :: Core.Maybe Types.Include
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentGroupList' value with any optional fields omitted.
mkSegmentGroupList ::
  SegmentGroupList
mkSegmentGroupList =
  SegmentGroupList' {groups = Core.Nothing, include = Core.Nothing}

-- | An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglGroups :: Lens.Lens' SegmentGroupList (Core.Maybe [Types.SegmentGroup])
sglGroups = Lens.field @"groups"
{-# DEPRECATED sglGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglInclude :: Lens.Lens' SegmentGroupList (Core.Maybe Types.Include)
sglInclude = Lens.field @"include"
{-# DEPRECATED sglInclude "Use generic-lens or generic-optics with 'include' instead." #-}

instance Core.FromJSON SegmentGroupList where
  toJSON SegmentGroupList {..} =
    Core.object
      ( Core.catMaybes
          [ ("Groups" Core..=) Core.<$> groups,
            ("Include" Core..=) Core.<$> include
          ]
      )

instance Core.FromJSON SegmentGroupList where
  parseJSON =
    Core.withObject "SegmentGroupList" Core.$
      \x ->
        SegmentGroupList'
          Core.<$> (x Core..:? "Groups") Core.<*> (x Core..:? "Include")
