{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelector
  ( ClipFragmentSelector (..),

    -- * Smart constructor
    mkClipFragmentSelector,

    -- * Lenses
    cfsFragmentSelectorType,
    cfsTimestampRange,
  )
where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types.ClipFragmentSelectorType as Types
import qualified Network.AWS.KinesisVideoArchivedMedia.Types.ClipTimestampRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the timestamp range and timestamp origin of a range of fragments.
--
-- Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the clip will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned.
--
-- /See:/ 'mkClipFragmentSelector' smart constructor.
data ClipFragmentSelector = ClipFragmentSelector'
  { -- | The origin of the timestamps to use (Server or Producer).
    fragmentSelectorType :: Types.ClipFragmentSelectorType,
    -- | The range of timestamps to return.
    timestampRange :: Types.ClipTimestampRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ClipFragmentSelector' value with any optional fields omitted.
mkClipFragmentSelector ::
  -- | 'fragmentSelectorType'
  Types.ClipFragmentSelectorType ->
  -- | 'timestampRange'
  Types.ClipTimestampRange ->
  ClipFragmentSelector
mkClipFragmentSelector fragmentSelectorType timestampRange =
  ClipFragmentSelector' {fragmentSelectorType, timestampRange}

-- | The origin of the timestamps to use (Server or Producer).
--
-- /Note:/ Consider using 'fragmentSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsFragmentSelectorType :: Lens.Lens' ClipFragmentSelector Types.ClipFragmentSelectorType
cfsFragmentSelectorType = Lens.field @"fragmentSelectorType"
{-# DEPRECATED cfsFragmentSelectorType "Use generic-lens or generic-optics with 'fragmentSelectorType' instead." #-}

-- | The range of timestamps to return.
--
-- /Note:/ Consider using 'timestampRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsTimestampRange :: Lens.Lens' ClipFragmentSelector Types.ClipTimestampRange
cfsTimestampRange = Lens.field @"timestampRange"
{-# DEPRECATED cfsTimestampRange "Use generic-lens or generic-optics with 'timestampRange' instead." #-}

instance Core.FromJSON ClipFragmentSelector where
  toJSON ClipFragmentSelector {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FragmentSelectorType" Core..= fragmentSelectorType),
            Core.Just ("TimestampRange" Core..= timestampRange)
          ]
      )
