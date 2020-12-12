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
    sglInclude,
    sglGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Include
import Network.AWS.Pinpoint.Types.SegmentGroup
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings that define the relationships between segment groups for a segment.
--
-- /See:/ 'mkSegmentGroupList' smart constructor.
data SegmentGroupList = SegmentGroupList'
  { include ::
      Lude.Maybe Include,
    groups :: Lude.Maybe [SegmentGroup]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentGroupList' with the minimum fields required to make a request.
--
-- * 'groups' - An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
-- * 'include' - Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
mkSegmentGroupList ::
  SegmentGroupList
mkSegmentGroupList =
  SegmentGroupList' {include = Lude.Nothing, groups = Lude.Nothing}

-- | Specifies how to handle multiple segment groups for the segment. For example, if the segment includes three segment groups, whether the resulting segment includes endpoints that match all, any, or none of the segment groups.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglInclude :: Lens.Lens' SegmentGroupList (Lude.Maybe Include)
sglInclude = Lens.lens (include :: SegmentGroupList -> Lude.Maybe Include) (\s a -> s {include = a} :: SegmentGroupList)
{-# DEPRECATED sglInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | An array that defines the set of segment criteria to evaluate when handling segment groups for the segment.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sglGroups :: Lens.Lens' SegmentGroupList (Lude.Maybe [SegmentGroup])
sglGroups = Lens.lens (groups :: SegmentGroupList -> Lude.Maybe [SegmentGroup]) (\s a -> s {groups = a} :: SegmentGroupList)
{-# DEPRECATED sglGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

instance Lude.FromJSON SegmentGroupList where
  parseJSON =
    Lude.withObject
      "SegmentGroupList"
      ( \x ->
          SegmentGroupList'
            Lude.<$> (x Lude..:? "Include")
            Lude.<*> (x Lude..:? "Groups" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SegmentGroupList where
  toJSON SegmentGroupList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Include" Lude..=) Lude.<$> include,
            ("Groups" Lude..=) Lude.<$> groups
          ]
      )
