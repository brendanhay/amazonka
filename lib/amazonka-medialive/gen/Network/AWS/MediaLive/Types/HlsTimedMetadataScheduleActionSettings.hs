{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
  ( HlsTimedMetadataScheduleActionSettings (..),

    -- * Smart constructor
    mkHlsTimedMetadataScheduleActionSettings,

    -- * Lenses
    htmsasId3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'mkHlsTimedMetadataScheduleActionSettings' smart constructor.
newtype HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'
  { -- | Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
    id3 :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HlsTimedMetadataScheduleActionSettings' value with any optional fields omitted.
mkHlsTimedMetadataScheduleActionSettings ::
  -- | 'id3'
  Core.Text ->
  HlsTimedMetadataScheduleActionSettings
mkHlsTimedMetadataScheduleActionSettings id3 =
  HlsTimedMetadataScheduleActionSettings' {id3}

-- | Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
--
-- /Note:/ Consider using 'id3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htmsasId3 :: Lens.Lens' HlsTimedMetadataScheduleActionSettings Core.Text
htmsasId3 = Lens.field @"id3"
{-# DEPRECATED htmsasId3 "Use generic-lens or generic-optics with 'id3' instead." #-}

instance Core.FromJSON HlsTimedMetadataScheduleActionSettings where
  toJSON HlsTimedMetadataScheduleActionSettings {..} =
    Core.object (Core.catMaybes [Core.Just ("id3" Core..= id3)])

instance Core.FromJSON HlsTimedMetadataScheduleActionSettings where
  parseJSON =
    Core.withObject "HlsTimedMetadataScheduleActionSettings" Core.$
      \x ->
        HlsTimedMetadataScheduleActionSettings' Core.<$> (x Core..: "id3")
