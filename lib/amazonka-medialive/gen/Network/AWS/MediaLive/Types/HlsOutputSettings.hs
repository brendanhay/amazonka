{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsOutputSettings
  ( HlsOutputSettings (..),

    -- * Smart constructor
    mkHlsOutputSettings,

    -- * Lenses
    hosHlsSettings,
    hosH265PackagingType,
    hosNameModifier,
    hosSegmentModifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.HlsH265PackagingType as Types
import qualified Network.AWS.MediaLive.Types.HlsSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Hls Output Settings
--
-- /See:/ 'mkHlsOutputSettings' smart constructor.
data HlsOutputSettings = HlsOutputSettings'
  { -- | Settings regarding the underlying stream. These settings are different for audio-only outputs.
    hlsSettings :: Types.HlsSettings,
    -- | Only applicable when this output is referencing an H.265 video description.
    --
    -- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
    h265PackagingType :: Core.Maybe Types.HlsH265PackagingType,
    -- | String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
    nameModifier :: Core.Maybe Core.Text,
    -- | String concatenated to end of segment filenames.
    segmentModifier :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsOutputSettings' value with any optional fields omitted.
mkHlsOutputSettings ::
  -- | 'hlsSettings'
  Types.HlsSettings ->
  HlsOutputSettings
mkHlsOutputSettings hlsSettings =
  HlsOutputSettings'
    { hlsSettings,
      h265PackagingType = Core.Nothing,
      nameModifier = Core.Nothing,
      segmentModifier = Core.Nothing
    }

-- | Settings regarding the underlying stream. These settings are different for audio-only outputs.
--
-- /Note:/ Consider using 'hlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosHlsSettings :: Lens.Lens' HlsOutputSettings Types.HlsSettings
hosHlsSettings = Lens.field @"hlsSettings"
{-# DEPRECATED hosHlsSettings "Use generic-lens or generic-optics with 'hlsSettings' instead." #-}

-- | Only applicable when this output is referencing an H.265 video description.
--
-- Specifies whether MP4 segments should be packaged as HEV1 or HVC1.
--
-- /Note:/ Consider using 'h265PackagingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosH265PackagingType :: Lens.Lens' HlsOutputSettings (Core.Maybe Types.HlsH265PackagingType)
hosH265PackagingType = Lens.field @"h265PackagingType"
{-# DEPRECATED hosH265PackagingType "Use generic-lens or generic-optics with 'h265PackagingType' instead." #-}

-- | String concatenated to the end of the destination filename. Accepts \"Format Identifiers\":#formatIdentifierParameters.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosNameModifier :: Lens.Lens' HlsOutputSettings (Core.Maybe Core.Text)
hosNameModifier = Lens.field @"nameModifier"
{-# DEPRECATED hosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

-- | String concatenated to end of segment filenames.
--
-- /Note:/ Consider using 'segmentModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hosSegmentModifier :: Lens.Lens' HlsOutputSettings (Core.Maybe Core.Text)
hosSegmentModifier = Lens.field @"segmentModifier"
{-# DEPRECATED hosSegmentModifier "Use generic-lens or generic-optics with 'segmentModifier' instead." #-}

instance Core.FromJSON HlsOutputSettings where
  toJSON HlsOutputSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("hlsSettings" Core..= hlsSettings),
            ("h265PackagingType" Core..=) Core.<$> h265PackagingType,
            ("nameModifier" Core..=) Core.<$> nameModifier,
            ("segmentModifier" Core..=) Core.<$> segmentModifier
          ]
      )

instance Core.FromJSON HlsOutputSettings where
  parseJSON =
    Core.withObject "HlsOutputSettings" Core.$
      \x ->
        HlsOutputSettings'
          Core.<$> (x Core..: "hlsSettings")
          Core.<*> (x Core..:? "h265PackagingType")
          Core.<*> (x Core..:? "nameModifier")
          Core.<*> (x Core..:? "segmentModifier")
