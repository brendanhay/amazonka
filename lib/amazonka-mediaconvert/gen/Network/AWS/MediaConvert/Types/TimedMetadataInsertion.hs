{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadataInsertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadataInsertion
  ( TimedMetadataInsertion (..),

    -- * Smart constructor
    mkTimedMetadataInsertion,

    -- * Lenses
    tmiId3Insertions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Id3Insertion as Types
import qualified Network.AWS.Prelude as Core

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /See:/ 'mkTimedMetadataInsertion' smart constructor.
newtype TimedMetadataInsertion = TimedMetadataInsertion'
  { -- | Id3Insertions contains the array of Id3Insertion instances.
    id3Insertions :: Core.Maybe [Types.Id3Insertion]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TimedMetadataInsertion' value with any optional fields omitted.
mkTimedMetadataInsertion ::
  TimedMetadataInsertion
mkTimedMetadataInsertion =
  TimedMetadataInsertion' {id3Insertions = Core.Nothing}

-- | Id3Insertions contains the array of Id3Insertion instances.
--
-- /Note:/ Consider using 'id3Insertions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmiId3Insertions :: Lens.Lens' TimedMetadataInsertion (Core.Maybe [Types.Id3Insertion])
tmiId3Insertions = Lens.field @"id3Insertions"
{-# DEPRECATED tmiId3Insertions "Use generic-lens or generic-optics with 'id3Insertions' instead." #-}

instance Core.FromJSON TimedMetadataInsertion where
  toJSON TimedMetadataInsertion {..} =
    Core.object
      (Core.catMaybes [("id3Insertions" Core..=) Core.<$> id3Insertions])

instance Core.FromJSON TimedMetadataInsertion where
  parseJSON =
    Core.withObject "TimedMetadataInsertion" Core.$
      \x ->
        TimedMetadataInsertion' Core.<$> (x Core..:? "id3Insertions")
