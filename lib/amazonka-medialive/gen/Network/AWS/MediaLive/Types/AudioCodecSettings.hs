{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioCodecSettings
  ( AudioCodecSettings (..),

    -- * Smart constructor
    mkAudioCodecSettings,

    -- * Lenses
    acsAacSettings,
    acsAc3Settings,
    acsEac3Settings,
    acsMp2Settings,
    acsPassThroughSettings,
    acsWavSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AacSettings as Types
import qualified Network.AWS.MediaLive.Types.Ac3Settings as Types
import qualified Network.AWS.MediaLive.Types.Eac3Settings as Types
import qualified Network.AWS.MediaLive.Types.Mp2Settings as Types
import qualified Network.AWS.MediaLive.Types.PassThroughSettings as Types
import qualified Network.AWS.MediaLive.Types.WavSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Codec Settings
--
-- /See:/ 'mkAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { aacSettings :: Core.Maybe Types.AacSettings,
    ac3Settings :: Core.Maybe Types.Ac3Settings,
    eac3Settings :: Core.Maybe Types.Eac3Settings,
    mp2Settings :: Core.Maybe Types.Mp2Settings,
    passThroughSettings :: Core.Maybe Types.PassThroughSettings,
    wavSettings :: Core.Maybe Types.WavSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioCodecSettings' value with any optional fields omitted.
mkAudioCodecSettings ::
  AudioCodecSettings
mkAudioCodecSettings =
  AudioCodecSettings'
    { aacSettings = Core.Nothing,
      ac3Settings = Core.Nothing,
      eac3Settings = Core.Nothing,
      mp2Settings = Core.Nothing,
      passThroughSettings = Core.Nothing,
      wavSettings = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'aacSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAacSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.AacSettings)
acsAacSettings = Lens.field @"aacSettings"
{-# DEPRECATED acsAacSettings "Use generic-lens or generic-optics with 'aacSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAc3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Ac3Settings)
acsAc3Settings = Lens.field @"ac3Settings"
{-# DEPRECATED acsAc3Settings "Use generic-lens or generic-optics with 'ac3Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'eac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Eac3Settings)
acsEac3Settings = Lens.field @"eac3Settings"
{-# DEPRECATED acsEac3Settings "Use generic-lens or generic-optics with 'eac3Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mp2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp2Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Mp2Settings)
acsMp2Settings = Lens.field @"mp2Settings"
{-# DEPRECATED acsMp2Settings "Use generic-lens or generic-optics with 'mp2Settings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'passThroughSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsPassThroughSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.PassThroughSettings)
acsPassThroughSettings = Lens.field @"passThroughSettings"
{-# DEPRECATED acsPassThroughSettings "Use generic-lens or generic-optics with 'passThroughSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'wavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsWavSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.WavSettings)
acsWavSettings = Lens.field @"wavSettings"
{-# DEPRECATED acsWavSettings "Use generic-lens or generic-optics with 'wavSettings' instead." #-}

instance Core.FromJSON AudioCodecSettings where
  toJSON AudioCodecSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("aacSettings" Core..=) Core.<$> aacSettings,
            ("ac3Settings" Core..=) Core.<$> ac3Settings,
            ("eac3Settings" Core..=) Core.<$> eac3Settings,
            ("mp2Settings" Core..=) Core.<$> mp2Settings,
            ("passThroughSettings" Core..=) Core.<$> passThroughSettings,
            ("wavSettings" Core..=) Core.<$> wavSettings
          ]
      )

instance Core.FromJSON AudioCodecSettings where
  parseJSON =
    Core.withObject "AudioCodecSettings" Core.$
      \x ->
        AudioCodecSettings'
          Core.<$> (x Core..:? "aacSettings")
          Core.<*> (x Core..:? "ac3Settings")
          Core.<*> (x Core..:? "eac3Settings")
          Core.<*> (x Core..:? "mp2Settings")
          Core.<*> (x Core..:? "passThroughSettings")
          Core.<*> (x Core..:? "wavSettings")
