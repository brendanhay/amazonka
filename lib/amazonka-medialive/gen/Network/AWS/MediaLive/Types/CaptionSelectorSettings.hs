{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelectorSettings
  ( CaptionSelectorSettings (..),

    -- * Smart constructor
    mkCaptionSelectorSettings,

    -- * Lenses
    cssAncillarySourceSettings,
    cssAribSourceSettings,
    cssDvbSubSourceSettings,
    cssEmbeddedSourceSettings,
    cssScte20SourceSettings,
    cssScte27SourceSettings,
    cssTeletextSourceSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AncillarySourceSettings as Types
import qualified Network.AWS.MediaLive.Types.AribSourceSettings as Types
import qualified Network.AWS.MediaLive.Types.DvbSubSourceSettings as Types
import qualified Network.AWS.MediaLive.Types.EmbeddedSourceSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte20SourceSettings as Types
import qualified Network.AWS.MediaLive.Types.Scte27SourceSettings as Types
import qualified Network.AWS.MediaLive.Types.TeletextSourceSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Caption Selector Settings
--
-- /See:/ 'mkCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { ancillarySourceSettings :: Core.Maybe Types.AncillarySourceSettings,
    aribSourceSettings :: Core.Maybe Types.AribSourceSettings,
    dvbSubSourceSettings :: Core.Maybe Types.DvbSubSourceSettings,
    embeddedSourceSettings :: Core.Maybe Types.EmbeddedSourceSettings,
    scte20SourceSettings :: Core.Maybe Types.Scte20SourceSettings,
    scte27SourceSettings :: Core.Maybe Types.Scte27SourceSettings,
    teletextSourceSettings :: Core.Maybe Types.TeletextSourceSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionSelectorSettings' value with any optional fields omitted.
mkCaptionSelectorSettings ::
  CaptionSelectorSettings
mkCaptionSelectorSettings =
  CaptionSelectorSettings'
    { ancillarySourceSettings = Core.Nothing,
      aribSourceSettings = Core.Nothing,
      dvbSubSourceSettings = Core.Nothing,
      embeddedSourceSettings = Core.Nothing,
      scte20SourceSettings = Core.Nothing,
      scte27SourceSettings = Core.Nothing,
      teletextSourceSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ancillarySourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAncillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.AncillarySourceSettings)
cssAncillarySourceSettings = Lens.field @"ancillarySourceSettings"
{-# DEPRECATED cssAncillarySourceSettings "Use generic-lens or generic-optics with 'ancillarySourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aribSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.AribSourceSettings)
cssAribSourceSettings = Lens.field @"aribSourceSettings"
{-# DEPRECATED cssAribSourceSettings "Use generic-lens or generic-optics with 'aribSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dvbSubSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.DvbSubSourceSettings)
cssDvbSubSourceSettings = Lens.field @"dvbSubSourceSettings"
{-# DEPRECATED cssDvbSubSourceSettings "Use generic-lens or generic-optics with 'dvbSubSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'embeddedSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssEmbeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.EmbeddedSourceSettings)
cssEmbeddedSourceSettings = Lens.field @"embeddedSourceSettings"
{-# DEPRECATED cssEmbeddedSourceSettings "Use generic-lens or generic-optics with 'embeddedSourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte20SourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.Scte20SourceSettings)
cssScte20SourceSettings = Lens.field @"scte20SourceSettings"
{-# DEPRECATED cssScte20SourceSettings "Use generic-lens or generic-optics with 'scte20SourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scte27SourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssScte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.Scte27SourceSettings)
cssScte27SourceSettings = Lens.field @"scte27SourceSettings"
{-# DEPRECATED cssScte27SourceSettings "Use generic-lens or generic-optics with 'scte27SourceSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'teletextSourceSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTeletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Core.Maybe Types.TeletextSourceSettings)
cssTeletextSourceSettings = Lens.field @"teletextSourceSettings"
{-# DEPRECATED cssTeletextSourceSettings "Use generic-lens or generic-optics with 'teletextSourceSettings' instead." #-}

instance Core.FromJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("ancillarySourceSettings" Core..=)
              Core.<$> ancillarySourceSettings,
            ("aribSourceSettings" Core..=) Core.<$> aribSourceSettings,
            ("dvbSubSourceSettings" Core..=) Core.<$> dvbSubSourceSettings,
            ("embeddedSourceSettings" Core..=) Core.<$> embeddedSourceSettings,
            ("scte20SourceSettings" Core..=) Core.<$> scte20SourceSettings,
            ("scte27SourceSettings" Core..=) Core.<$> scte27SourceSettings,
            ("teletextSourceSettings" Core..=)
              Core.<$> teletextSourceSettings
          ]
      )

instance Core.FromJSON CaptionSelectorSettings where
  parseJSON =
    Core.withObject "CaptionSelectorSettings" Core.$
      \x ->
        CaptionSelectorSettings'
          Core.<$> (x Core..:? "ancillarySourceSettings")
          Core.<*> (x Core..:? "aribSourceSettings")
          Core.<*> (x Core..:? "dvbSubSourceSettings")
          Core.<*> (x Core..:? "embeddedSourceSettings")
          Core.<*> (x Core..:? "scte20SourceSettings")
          Core.<*> (x Core..:? "scte27SourceSettings")
          Core.<*> (x Core..:? "teletextSourceSettings")
