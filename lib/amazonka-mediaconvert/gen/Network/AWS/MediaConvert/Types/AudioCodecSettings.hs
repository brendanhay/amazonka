-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodecSettings
  ( AudioCodecSettings (..),

    -- * Smart constructor
    mkAudioCodecSettings,

    -- * Lenses
    acsAiffSettings,
    acsCodec,
    acsAc3Settings,
    acsOpusSettings,
    acsMp2Settings,
    acsWavSettings,
    acsEac3AtmosSettings,
    acsMp3Settings,
    acsVorbisSettings,
    acsAacSettings,
    acsEac3Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AacSettings
import Network.AWS.MediaConvert.Types.Ac3Settings
import Network.AWS.MediaConvert.Types.AiffSettings
import Network.AWS.MediaConvert.Types.AudioCodec
import Network.AWS.MediaConvert.Types.Eac3AtmosSettings
import Network.AWS.MediaConvert.Types.Eac3Settings
import Network.AWS.MediaConvert.Types.Mp2Settings
import Network.AWS.MediaConvert.Types.Mp3Settings
import Network.AWS.MediaConvert.Types.OpusSettings
import Network.AWS.MediaConvert.Types.VorbisSettings
import Network.AWS.MediaConvert.Types.WavSettings
import qualified Network.AWS.Prelude as Lude

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value that you choose for Audio codec (Codec). For each codec enum that you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS, OpusSettings
--
-- /See:/ 'mkAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { aiffSettings ::
      Lude.Maybe AiffSettings,
    codec :: Lude.Maybe AudioCodec,
    ac3Settings :: Lude.Maybe Ac3Settings,
    opusSettings :: Lude.Maybe OpusSettings,
    mp2Settings :: Lude.Maybe Mp2Settings,
    wavSettings :: Lude.Maybe WavSettings,
    eac3AtmosSettings :: Lude.Maybe Eac3AtmosSettings,
    mp3Settings :: Lude.Maybe Mp3Settings,
    vorbisSettings :: Lude.Maybe VorbisSettings,
    aacSettings :: Lude.Maybe AacSettings,
    eac3Settings :: Lude.Maybe Eac3Settings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioCodecSettings' with the minimum fields required to make a request.
--
-- * 'aacSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
-- * 'ac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
-- * 'aiffSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
-- * 'codec' - Type of Audio codec.
-- * 'eac3AtmosSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
-- * 'eac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
-- * 'mp2Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
-- * 'mp3Settings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
-- * 'opusSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
-- * 'vorbisSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
-- * 'wavSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
mkAudioCodecSettings ::
  AudioCodecSettings
mkAudioCodecSettings =
  AudioCodecSettings'
    { aiffSettings = Lude.Nothing,
      codec = Lude.Nothing,
      ac3Settings = Lude.Nothing,
      opusSettings = Lude.Nothing,
      mp2Settings = Lude.Nothing,
      wavSettings = Lude.Nothing,
      eac3AtmosSettings = Lude.Nothing,
      mp3Settings = Lude.Nothing,
      vorbisSettings = Lude.Nothing,
      aacSettings = Lude.Nothing,
      eac3Settings = Lude.Nothing
    }

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
--
-- /Note:/ Consider using 'aiffSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAiffSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe AiffSettings)
acsAiffSettings = Lens.lens (aiffSettings :: AudioCodecSettings -> Lude.Maybe AiffSettings) (\s a -> s {aiffSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsAiffSettings "Use generic-lens or generic-optics with 'aiffSettings' instead." #-}

-- | Type of Audio codec.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsCodec :: Lens.Lens' AudioCodecSettings (Lude.Maybe AudioCodec)
acsCodec = Lens.lens (codec :: AudioCodecSettings -> Lude.Maybe AudioCodec) (\s a -> s {codec = a} :: AudioCodecSettings)
{-# DEPRECATED acsCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
--
-- /Note:/ Consider using 'ac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAc3Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Ac3Settings)
acsAc3Settings = Lens.lens (ac3Settings :: AudioCodecSettings -> Lude.Maybe Ac3Settings) (\s a -> s {ac3Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsAc3Settings "Use generic-lens or generic-optics with 'ac3Settings' instead." #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value OPUS.
--
-- /Note:/ Consider using 'opusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsOpusSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe OpusSettings)
acsOpusSettings = Lens.lens (opusSettings :: AudioCodecSettings -> Lude.Maybe OpusSettings) (\s a -> s {opusSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsOpusSettings "Use generic-lens or generic-optics with 'opusSettings' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
--
-- /Note:/ Consider using 'mp2Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp2Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Mp2Settings)
acsMp2Settings = Lens.lens (mp2Settings :: AudioCodecSettings -> Lude.Maybe Mp2Settings) (\s a -> s {mp2Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsMp2Settings "Use generic-lens or generic-optics with 'mp2Settings' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
--
-- /Note:/ Consider using 'wavSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsWavSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe WavSettings)
acsWavSettings = Lens.lens (wavSettings :: AudioCodecSettings -> Lude.Maybe WavSettings) (\s a -> s {wavSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsWavSettings "Use generic-lens or generic-optics with 'wavSettings' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- /Note:/ Consider using 'eac3AtmosSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Eac3AtmosSettings)
acsEac3AtmosSettings = Lens.lens (eac3AtmosSettings :: AudioCodecSettings -> Lude.Maybe Eac3AtmosSettings) (\s a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsEac3AtmosSettings "Use generic-lens or generic-optics with 'eac3AtmosSettings' instead." #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value MP3.
--
-- /Note:/ Consider using 'mp3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsMp3Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Mp3Settings)
acsMp3Settings = Lens.lens (mp3Settings :: AudioCodecSettings -> Lude.Maybe Mp3Settings) (\s a -> s {mp3Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsMp3Settings "Use generic-lens or generic-optics with 'mp3Settings' instead." #-}

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to the value Vorbis.
--
-- /Note:/ Consider using 'vorbisSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsVorbisSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe VorbisSettings)
acsVorbisSettings = Lens.lens (vorbisSettings :: AudioCodecSettings -> Lude.Maybe VorbisSettings) (\s a -> s {vorbisSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsVorbisSettings "Use generic-lens or generic-optics with 'vorbisSettings' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC. The service accepts one of two mutually exclusive groups of AAC settings--VBR and CBR. To select one of these modes, set the value of Bitrate control mode (rateControlMode) to "VBR" or "CBR".  In VBR mode, you control the audio quality with the setting VBR quality (vbrQuality). In CBR mode, you use the setting Bitrate (bitrate). Defaults and valid values depend on the rate control mode.
--
-- /Note:/ Consider using 'aacSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsAacSettings :: Lens.Lens' AudioCodecSettings (Lude.Maybe AacSettings)
acsAacSettings = Lens.lens (aacSettings :: AudioCodecSettings -> Lude.Maybe AacSettings) (\s a -> s {aacSettings = a} :: AudioCodecSettings)
{-# DEPRECATED acsAacSettings "Use generic-lens or generic-optics with 'aacSettings' instead." #-}

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /Note:/ Consider using 'eac3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsEac3Settings :: Lens.Lens' AudioCodecSettings (Lude.Maybe Eac3Settings)
acsEac3Settings = Lens.lens (eac3Settings :: AudioCodecSettings -> Lude.Maybe Eac3Settings) (\s a -> s {eac3Settings = a} :: AudioCodecSettings)
{-# DEPRECATED acsEac3Settings "Use generic-lens or generic-optics with 'eac3Settings' instead." #-}

instance Lude.FromJSON AudioCodecSettings where
  parseJSON =
    Lude.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Lude.<$> (x Lude..:? "aiffSettings")
            Lude.<*> (x Lude..:? "codec")
            Lude.<*> (x Lude..:? "ac3Settings")
            Lude.<*> (x Lude..:? "opusSettings")
            Lude.<*> (x Lude..:? "mp2Settings")
            Lude.<*> (x Lude..:? "wavSettings")
            Lude.<*> (x Lude..:? "eac3AtmosSettings")
            Lude.<*> (x Lude..:? "mp3Settings")
            Lude.<*> (x Lude..:? "vorbisSettings")
            Lude.<*> (x Lude..:? "aacSettings")
            Lude.<*> (x Lude..:? "eac3Settings")
      )

instance Lude.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("aiffSettings" Lude..=) Lude.<$> aiffSettings,
            ("codec" Lude..=) Lude.<$> codec,
            ("ac3Settings" Lude..=) Lude.<$> ac3Settings,
            ("opusSettings" Lude..=) Lude.<$> opusSettings,
            ("mp2Settings" Lude..=) Lude.<$> mp2Settings,
            ("wavSettings" Lude..=) Lude.<$> wavSettings,
            ("eac3AtmosSettings" Lude..=) Lude.<$> eac3AtmosSettings,
            ("mp3Settings" Lude..=) Lude.<$> mp3Settings,
            ("vorbisSettings" Lude..=) Lude.<$> vorbisSettings,
            ("aacSettings" Lude..=) Lude.<$> aacSettings,
            ("eac3Settings" Lude..=) Lude.<$> eac3Settings
          ]
      )
