{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentReference
  ( SegmentReference (..),

    -- * Smart constructor
    mkSegmentReference,

    -- * Lenses
    sId,
    sVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the segment identifier and version of a segment.
--
-- /See:/ 'mkSegmentReference' smart constructor.
data SegmentReference = SegmentReference'
  { -- | The unique identifier for the segment.
    id :: Core.Text,
    -- | The version number of the segment.
    version :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentReference' value with any optional fields omitted.
mkSegmentReference ::
  -- | 'id'
  Core.Text ->
  SegmentReference
mkSegmentReference id =
  SegmentReference' {id, version = Core.Nothing}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' SegmentReference Core.Text
sId = Lens.field @"id"
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version number of the segment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVersion :: Lens.Lens' SegmentReference (Core.Maybe Core.Int)
sVersion = Lens.field @"version"
{-# DEPRECATED sVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON SegmentReference where
  toJSON SegmentReference {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("Version" Core..=) Core.<$> version
          ]
      )

instance Core.FromJSON SegmentReference where
  parseJSON =
    Core.withObject "SegmentReference" Core.$
      \x ->
        SegmentReference'
          Core.<$> (x Core..: "Id") Core.<*> (x Core..:? "Version")
