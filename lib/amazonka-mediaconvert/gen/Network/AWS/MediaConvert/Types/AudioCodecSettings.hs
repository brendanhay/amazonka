{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioCodecSettings
  ( AudioCodecSettings (..)
  -- * Smart constructor
  , mkAudioCodecSettings
  -- * Lenses
  , acsAacSettings
  , acsAc3Settings
  , acsAiffSettings
  , acsCodec
  , acsEac3AtmosSettings
  , acsEac3Settings
  , acsMp2Settings
  , acsMp3Settings
  , acsOpusSettings
  , acsVorbisSettings
  , acsWavSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AacSettings as Types
import qualified Network.AWS.MediaConvert.Types.Ac3Settings as Types
import qualified Network.AWS.MediaConvert.Types.AiffSettings as Types
import qualified Network.AWS.MediaConvert.Types.AudioCodec as Types
import qualified Network.AWS.MediaConvert.Types.Eac3AtmosSettings as Types
import qualified Network.AWS.MediaConvert.Types.Eac3Settings as Types
import qualified Network.AWS.MediaConvert.Types.Mp2Settings as Types
import qualified Network.AWS.MediaConvert.Types.Mp3Settings as Types
import qualified Network.AWS.MediaConvert.Types.OpusSettings as Types
import qualified Network.AWS.MediaConvert.Types.VorbisSettings as Types
import qualified Network.AWS.MediaConvert.Types.WavSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- /See:/ 'mkAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { aacSettings :: Core.Maybe Types.AacSettings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
  , ac3Settings :: Core.Maybe Types.Ac3Settings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
  , aiffSettings :: Core.Maybe Types.AiffSettings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
  , codec :: Core.Maybe Types.AudioCodec
    -- ^ Type of Audio codec.
  , eac3AtmosSettings :: Core.Maybe Types.Eac3AtmosSettings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
  , eac3Settings :: Core.Maybe Types.Eac3Settings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
  , mp2Settings :: Core.Maybe Types.Mp2Settings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
  , mp3Settings :: Core.Maybe Types.Mp3Settings
    -- ^ Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
  , opusSettings :: Core.Maybe Types.OpusSettings
    -- ^ Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
  , vorbisSettings :: Core.Maybe Types.VorbisSettings
    -- ^ Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
  , wavSettings :: Core.Maybe Types.WavSettings
    -- ^ Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioCodecSettings' value with any optional fields omitted.
mkAudioCodecSettings
    :: AudioCodecSettings
mkAudioCodecSettings
  = AudioCodecSettings'{aacSettings = Core.Nothing,
                        ac3Settings = Core.Nothing, aiffSettings = Core.Nothing,
                        codec = Core.Nothing, eac3AtmosSettings = Core.Nothing,
                        eac3Settings = Core.Nothing, mp2Settings = Core.Nothing,
                        mp3Settings = Core.Nothing, opusSettings = Core.Nothing,
                        vorbisSettings = Core.Nothing, wavSettings = Core.Nothing}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /Note:/ Consider using 'aacSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAacSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.AacSettings)
acsAacSettings = Lens.field @"aacSettings"
{-# INLINEABLE acsAacSettings #-}
{-# DEPRECATED aacSettings "Use generic-lens or generic-optics with 'aacSettings' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /Note:/ Consider using 'ac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAc3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Ac3Settings)
acsAc3Settings = Lens.field @"ac3Settings"
{-# INLINEABLE acsAc3Settings #-}
{-# DEPRECATED ac3Settings "Use generic-lens or generic-optics with 'ac3Settings' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /Note:/ Consider using 'aiffSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAiffSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.AiffSettings)
acsAiffSettings = Lens.field @"aiffSettings"
{-# INLINEABLE acsAiffSettings #-}
{-# DEPRECATED aiffSettings "Use generic-lens or generic-optics with 'aiffSettings' instead"  #-}

-- | Type of Audio codec.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsCodec :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.AudioCodec)
acsCodec = Lens.field @"codec"
{-# INLINEABLE acsCodec #-}
{-# DEPRECATED codec "Use generic-lens or generic-optics with 'codec' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- /Note:/ Consider using 'eac3AtmosSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Eac3AtmosSettings)
acsEac3AtmosSettings = Lens.field @"eac3AtmosSettings"
{-# INLINEABLE acsEac3AtmosSettings #-}
{-# DEPRECATED eac3AtmosSettings "Use generic-lens or generic-optics with 'eac3AtmosSettings' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /Note:/ Consider using 'eac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Eac3Settings)
acsEac3Settings = Lens.field @"eac3Settings"
{-# INLINEABLE acsEac3Settings #-}
{-# DEPRECATED eac3Settings "Use generic-lens or generic-optics with 'eac3Settings' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /Note:/ Consider using 'mp2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp2Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Mp2Settings)
acsMp2Settings = Lens.field @"mp2Settings"
{-# INLINEABLE acsMp2Settings #-}
{-# DEPRECATED mp2Settings "Use generic-lens or generic-optics with 'mp2Settings' instead"  #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- /Note:/ Consider using 'mp3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.Mp3Settings)
acsMp3Settings = Lens.field @"mp3Settings"
{-# INLINEABLE acsMp3Settings #-}
{-# DEPRECATED mp3Settings "Use generic-lens or generic-optics with 'mp3Settings' instead"  #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- /Note:/ Consider using 'opusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsOpusSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.OpusSettings)
acsOpusSettings = Lens.field @"opusSettings"
{-# INLINEABLE acsOpusSettings #-}
{-# DEPRECATED opusSettings "Use generic-lens or generic-optics with 'opusSettings' instead"  #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- /Note:/ Consider using 'vorbisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsVorbisSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.VorbisSettings)
acsVorbisSettings = Lens.field @"vorbisSettings"
{-# INLINEABLE acsVorbisSettings #-}
{-# DEPRECATED vorbisSettings "Use generic-lens or generic-optics with 'vorbisSettings' instead"  #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /Note:/ Consider using 'wavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsWavSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Types.WavSettings)
acsWavSettings = Lens.field @"wavSettings"
{-# INLINEABLE acsWavSettings #-}
{-# DEPRECATED wavSettings "Use generic-lens or generic-optics with 'wavSettings' instead"  #-}

instance Core.FromJSON AudioCodecSettings where
        toJSON AudioCodecSettings{..}
          = Core.object
              (Core.catMaybes
                 [("aacSettings" Core..=) Core.<$> aacSettings,
                  ("ac3Settings" Core..=) Core.<$> ac3Settings,
                  ("aiffSettings" Core..=) Core.<$> aiffSettings,
                  ("codec" Core..=) Core.<$> codec,
                  ("eac3AtmosSettings" Core..=) Core.<$> eac3AtmosSettings,
                  ("eac3Settings" Core..=) Core.<$> eac3Settings,
                  ("mp2Settings" Core..=) Core.<$> mp2Settings,
                  ("mp3Settings" Core..=) Core.<$> mp3Settings,
                  ("opusSettings" Core..=) Core.<$> opusSettings,
                  ("vorbisSettings" Core..=) Core.<$> vorbisSettings,
                  ("wavSettings" Core..=) Core.<$> wavSettings])

instance Core.FromJSON AudioCodecSettings where
        parseJSON
          = Core.withObject "AudioCodecSettings" Core.$
              \ x ->
                AudioCodecSettings' Core.<$>
                  (x Core..:? "aacSettings") Core.<*> x Core..:? "ac3Settings"
                    Core.<*> x Core..:? "aiffSettings"
                    Core.<*> x Core..:? "codec"
                    Core.<*> x Core..:? "eac3AtmosSettings"
                    Core.<*> x Core..:? "eac3Settings"
                    Core.<*> x Core..:? "mp2Settings"
                    Core.<*> x Core..:? "mp3Settings"
                    Core.<*> x Core..:? "opusSettings"
                    Core.<*> x Core..:? "vorbisSettings"
                    Core.<*> x Core..:? "wavSettings"
