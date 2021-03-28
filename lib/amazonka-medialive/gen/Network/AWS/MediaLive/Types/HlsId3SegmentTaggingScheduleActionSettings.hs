{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
  ( HlsId3SegmentTaggingScheduleActionSettings (..)
  -- * Smart constructor
  , mkHlsId3SegmentTaggingScheduleActionSettings
  -- * Lenses
  , histsasTag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the action to insert a user-defined ID3 tag in each HLS segment
--
-- /See:/ 'mkHlsId3SegmentTaggingScheduleActionSettings' smart constructor.
newtype HlsId3SegmentTaggingScheduleActionSettings = HlsId3SegmentTaggingScheduleActionSettings'
  { tag :: Core.Text
    -- ^ ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HlsId3SegmentTaggingScheduleActionSettings' value with any optional fields omitted.
mkHlsId3SegmentTaggingScheduleActionSettings
    :: Core.Text -- ^ 'tag'
    -> HlsId3SegmentTaggingScheduleActionSettings
mkHlsId3SegmentTaggingScheduleActionSettings tag
  = HlsId3SegmentTaggingScheduleActionSettings'{tag}

-- | ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
histsasTag :: Lens.Lens' HlsId3SegmentTaggingScheduleActionSettings Core.Text
histsasTag = Lens.field @"tag"
{-# INLINEABLE histsasTag #-}
{-# DEPRECATED tag "Use generic-lens or generic-optics with 'tag' instead"  #-}

instance Core.FromJSON HlsId3SegmentTaggingScheduleActionSettings
         where
        toJSON HlsId3SegmentTaggingScheduleActionSettings{..}
          = Core.object (Core.catMaybes [Core.Just ("tag" Core..= tag)])

instance Core.FromJSON HlsId3SegmentTaggingScheduleActionSettings
         where
        parseJSON
          = Core.withObject "HlsId3SegmentTaggingScheduleActionSettings"
              Core.$
              \ x ->
                HlsId3SegmentTaggingScheduleActionSettings' Core.<$>
                  (x Core..: "tag")
