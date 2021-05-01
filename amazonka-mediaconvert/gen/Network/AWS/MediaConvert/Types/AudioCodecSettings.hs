{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioCodecSettings where

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
import qualified Network.AWS.Prelude as Prelude

-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains
-- the group of settings related to audio encoding. The settings in this
-- group vary depending on the value that you choose for Audio codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AAC, AacSettings * MP2, Mp2Settings * MP3, Mp3Settings * WAV,
-- WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings
-- * EAC3_ATMOS, Eac3AtmosSettings * VORBIS, VorbisSettings * OPUS,
-- OpusSettings
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AC3.
    ac3Settings :: Prelude.Maybe Ac3Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value Vorbis.
    vorbisSettings :: Prelude.Maybe VorbisSettings,
    -- | Type of Audio codec.
    codec :: Prelude.Maybe AudioCodec,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value MP3.
    mp3Settings :: Prelude.Maybe Mp3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value MP2.
    mp2Settings :: Prelude.Maybe Mp2Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value OPUS.
    opusSettings :: Prelude.Maybe OpusSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3.
    eac3Settings :: Prelude.Maybe Eac3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AAC. The service accepts one of two mutually exclusive
    -- groups of AAC settings--VBR and CBR. To select one of these modes, set
    -- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
    -- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
    -- quality (vbrQuality). In CBR mode, you use the setting Bitrate
    -- (bitrate). Defaults and valid values depend on the rate control mode.
    aacSettings :: Prelude.Maybe AacSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value WAV.
    wavSettings :: Prelude.Maybe WavSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AIFF.
    aiffSettings :: Prelude.Maybe AiffSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3_ATMOS.
    eac3AtmosSettings :: Prelude.Maybe Eac3AtmosSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ac3Settings', 'audioCodecSettings_ac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- 'vorbisSettings', 'audioCodecSettings_vorbisSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
--
-- 'codec', 'audioCodecSettings_codec' - Type of Audio codec.
--
-- 'mp3Settings', 'audioCodecSettings_mp3Settings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
--
-- 'mp2Settings', 'audioCodecSettings_mp2Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
--
-- 'opusSettings', 'audioCodecSettings_opusSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
--
-- 'eac3Settings', 'audioCodecSettings_eac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
--
-- 'aacSettings', 'audioCodecSettings_aacSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
--
-- 'wavSettings', 'audioCodecSettings_wavSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
--
-- 'aiffSettings', 'audioCodecSettings_aiffSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
--
-- 'eac3AtmosSettings', 'audioCodecSettings_eac3AtmosSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
newAudioCodecSettings ::
  AudioCodecSettings
newAudioCodecSettings =
  AudioCodecSettings'
    { ac3Settings = Prelude.Nothing,
      vorbisSettings = Prelude.Nothing,
      codec = Prelude.Nothing,
      mp3Settings = Prelude.Nothing,
      mp2Settings = Prelude.Nothing,
      opusSettings = Prelude.Nothing,
      eac3Settings = Prelude.Nothing,
      aacSettings = Prelude.Nothing,
      wavSettings = Prelude.Nothing,
      aiffSettings = Prelude.Nothing,
      eac3AtmosSettings = Prelude.Nothing
    }

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
audioCodecSettings_vorbisSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe VorbisSettings)
audioCodecSettings_vorbisSettings = Lens.lens (\AudioCodecSettings' {vorbisSettings} -> vorbisSettings) (\s@AudioCodecSettings' {} a -> s {vorbisSettings = a} :: AudioCodecSettings)

-- | Type of Audio codec.
audioCodecSettings_codec :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AudioCodec)
audioCodecSettings_codec = Lens.lens (\AudioCodecSettings' {codec} -> codec) (\s@AudioCodecSettings' {} a -> s {codec = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
audioCodecSettings_mp3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp3Settings)
audioCodecSettings_mp3Settings = Lens.lens (\AudioCodecSettings' {mp3Settings} -> mp3Settings) (\s@AudioCodecSettings' {} a -> s {mp3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
audioCodecSettings_opusSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe OpusSettings)
audioCodecSettings_opusSettings = Lens.lens (\AudioCodecSettings' {opusSettings} -> opusSettings) (\s@AudioCodecSettings' {} a -> s {opusSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
audioCodecSettings_aiffSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AiffSettings)
audioCodecSettings_aiffSettings = Lens.lens (\AudioCodecSettings' {aiffSettings} -> aiffSettings) (\s@AudioCodecSettings' {} a -> s {aiffSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
audioCodecSettings_eac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3AtmosSettings)
audioCodecSettings_eac3AtmosSettings = Lens.lens (\AudioCodecSettings' {eac3AtmosSettings} -> eac3AtmosSettings) (\s@AudioCodecSettings' {} a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)

instance Prelude.FromJSON AudioCodecSettings where
  parseJSON =
    Prelude.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Prelude.<$> (x Prelude..:? "ac3Settings")
            Prelude.<*> (x Prelude..:? "vorbisSettings")
            Prelude.<*> (x Prelude..:? "codec")
            Prelude.<*> (x Prelude..:? "mp3Settings")
            Prelude.<*> (x Prelude..:? "mp2Settings")
            Prelude.<*> (x Prelude..:? "opusSettings")
            Prelude.<*> (x Prelude..:? "eac3Settings")
            Prelude.<*> (x Prelude..:? "aacSettings")
            Prelude.<*> (x Prelude..:? "wavSettings")
            Prelude.<*> (x Prelude..:? "aiffSettings")
            Prelude.<*> (x Prelude..:? "eac3AtmosSettings")
      )

instance Prelude.Hashable AudioCodecSettings

instance Prelude.NFData AudioCodecSettings

instance Prelude.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ac3Settings" Prelude..=) Prelude.<$> ac3Settings,
            ("vorbisSettings" Prelude..=)
              Prelude.<$> vorbisSettings,
            ("codec" Prelude..=) Prelude.<$> codec,
            ("mp3Settings" Prelude..=) Prelude.<$> mp3Settings,
            ("mp2Settings" Prelude..=) Prelude.<$> mp2Settings,
            ("opusSettings" Prelude..=) Prelude.<$> opusSettings,
            ("eac3Settings" Prelude..=) Prelude.<$> eac3Settings,
            ("aacSettings" Prelude..=) Prelude.<$> aacSettings,
            ("wavSettings" Prelude..=) Prelude.<$> wavSettings,
            ("aiffSettings" Prelude..=) Prelude.<$> aiffSettings,
            ("eac3AtmosSettings" Prelude..=)
              Prelude.<$> eac3AtmosSettings
          ]
      )
