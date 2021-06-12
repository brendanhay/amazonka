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

import qualified Network.AWS.Core as Core
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
    ac3Settings :: Core.Maybe Ac3Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value Vorbis.
    vorbisSettings :: Core.Maybe VorbisSettings,
    -- | Type of Audio codec.
    codec :: Core.Maybe AudioCodec,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value MP3.
    mp3Settings :: Core.Maybe Mp3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value MP2.
    mp2Settings :: Core.Maybe Mp2Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value OPUS.
    opusSettings :: Core.Maybe OpusSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3.
    eac3Settings :: Core.Maybe Eac3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AAC. The service accepts one of two mutually exclusive
    -- groups of AAC settings--VBR and CBR. To select one of these modes, set
    -- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
    -- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
    -- quality (vbrQuality). In CBR mode, you use the setting Bitrate
    -- (bitrate). Defaults and valid values depend on the rate control mode.
    aacSettings :: Core.Maybe AacSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value WAV.
    wavSettings :: Core.Maybe WavSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AIFF.
    aiffSettings :: Core.Maybe AiffSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3_ATMOS.
    eac3AtmosSettings :: Core.Maybe Eac3AtmosSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ac3Settings = Core.Nothing,
      vorbisSettings = Core.Nothing,
      codec = Core.Nothing,
      mp3Settings = Core.Nothing,
      mp2Settings = Core.Nothing,
      opusSettings = Core.Nothing,
      eac3Settings = Core.Nothing,
      aacSettings = Core.Nothing,
      wavSettings = Core.Nothing,
      aiffSettings = Core.Nothing,
      eac3AtmosSettings = Core.Nothing
    }

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
audioCodecSettings_vorbisSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe VorbisSettings)
audioCodecSettings_vorbisSettings = Lens.lens (\AudioCodecSettings' {vorbisSettings} -> vorbisSettings) (\s@AudioCodecSettings' {} a -> s {vorbisSettings = a} :: AudioCodecSettings)

-- | Type of Audio codec.
audioCodecSettings_codec :: Lens.Lens' AudioCodecSettings (Core.Maybe AudioCodec)
audioCodecSettings_codec = Lens.lens (\AudioCodecSettings' {codec} -> codec) (\s@AudioCodecSettings' {} a -> s {codec = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
audioCodecSettings_mp3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Mp3Settings)
audioCodecSettings_mp3Settings = Lens.lens (\AudioCodecSettings' {mp3Settings} -> mp3Settings) (\s@AudioCodecSettings' {} a -> s {mp3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
audioCodecSettings_opusSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe OpusSettings)
audioCodecSettings_opusSettings = Lens.lens (\AudioCodecSettings' {opusSettings} -> opusSettings) (\s@AudioCodecSettings' {} a -> s {opusSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Core.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
audioCodecSettings_aiffSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe AiffSettings)
audioCodecSettings_aiffSettings = Lens.lens (\AudioCodecSettings' {aiffSettings} -> aiffSettings) (\s@AudioCodecSettings' {} a -> s {aiffSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
audioCodecSettings_eac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Core.Maybe Eac3AtmosSettings)
audioCodecSettings_eac3AtmosSettings = Lens.lens (\AudioCodecSettings' {eac3AtmosSettings} -> eac3AtmosSettings) (\s@AudioCodecSettings' {} a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)

instance Core.FromJSON AudioCodecSettings where
  parseJSON =
    Core.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Core.<$> (x Core..:? "ac3Settings")
            Core.<*> (x Core..:? "vorbisSettings")
            Core.<*> (x Core..:? "codec")
            Core.<*> (x Core..:? "mp3Settings")
            Core.<*> (x Core..:? "mp2Settings")
            Core.<*> (x Core..:? "opusSettings")
            Core.<*> (x Core..:? "eac3Settings")
            Core.<*> (x Core..:? "aacSettings")
            Core.<*> (x Core..:? "wavSettings")
            Core.<*> (x Core..:? "aiffSettings")
            Core.<*> (x Core..:? "eac3AtmosSettings")
      )

instance Core.Hashable AudioCodecSettings

instance Core.NFData AudioCodecSettings

instance Core.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ac3Settings" Core..=) Core.<$> ac3Settings,
            ("vorbisSettings" Core..=) Core.<$> vorbisSettings,
            ("codec" Core..=) Core.<$> codec,
            ("mp3Settings" Core..=) Core.<$> mp3Settings,
            ("mp2Settings" Core..=) Core.<$> mp2Settings,
            ("opusSettings" Core..=) Core.<$> opusSettings,
            ("eac3Settings" Core..=) Core.<$> eac3Settings,
            ("aacSettings" Core..=) Core.<$> aacSettings,
            ("wavSettings" Core..=) Core.<$> wavSettings,
            ("aiffSettings" Core..=) Core.<$> aiffSettings,
            ("eac3AtmosSettings" Core..=)
              Core.<$> eac3AtmosSettings
          ]
      )
