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
-- Module      : Amazonka.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioCodecSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AacSettings
import Amazonka.MediaConvert.Types.Ac3Settings
import Amazonka.MediaConvert.Types.AiffSettings
import Amazonka.MediaConvert.Types.AudioCodec
import Amazonka.MediaConvert.Types.Eac3AtmosSettings
import Amazonka.MediaConvert.Types.Eac3Settings
import Amazonka.MediaConvert.Types.Mp2Settings
import Amazonka.MediaConvert.Types.Mp3Settings
import Amazonka.MediaConvert.Types.OpusSettings
import Amazonka.MediaConvert.Types.VorbisSettings
import Amazonka.MediaConvert.Types.WavSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to audio encoding. The settings in this group vary
-- depending on the value that you choose for your audio codec.
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AAC. The service accepts one of two mutually exclusive
    -- groups of AAC settings--VBR and CBR. To select one of these modes, set
    -- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
    -- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
    -- quality (vbrQuality). In CBR mode, you use the setting Bitrate
    -- (bitrate). Defaults and valid values depend on the rate control mode.
    aacSettings :: Prelude.Maybe AacSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AC3.
    ac3Settings :: Prelude.Maybe Ac3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AIFF.
    aiffSettings :: Prelude.Maybe AiffSettings,
    -- | Choose the audio codec for this output. Note that the option Dolby
    -- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
    -- Dolby Digital Plus audio inputs. Make sure that you choose a codec
    -- that\'s supported with your output container:
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
    -- For audio-only outputs, make sure that both your input audio codec and
    -- your output audio codec are supported for audio-only workflows. For more
    -- information, see:
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
    -- and
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
    codec :: Prelude.Maybe AudioCodec,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3_ATMOS.
    eac3AtmosSettings :: Prelude.Maybe Eac3AtmosSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3.
    eac3Settings :: Prelude.Maybe Eac3Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value MP2.
    mp2Settings :: Prelude.Maybe Mp2Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value MP3.
    mp3Settings :: Prelude.Maybe Mp3Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value OPUS.
    opusSettings :: Prelude.Maybe OpusSettings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value Vorbis.
    vorbisSettings :: Prelude.Maybe VorbisSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value WAV.
    wavSettings :: Prelude.Maybe WavSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aacSettings', 'audioCodecSettings_aacSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
--
-- 'ac3Settings', 'audioCodecSettings_ac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- 'aiffSettings', 'audioCodecSettings_aiffSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
--
-- 'codec', 'audioCodecSettings_codec' - Choose the audio codec for this output. Note that the option Dolby
-- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
-- Dolby Digital Plus audio inputs. Make sure that you choose a codec
-- that\'s supported with your output container:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
-- For audio-only outputs, make sure that both your input audio codec and
-- your output audio codec are supported for audio-only workflows. For more
-- information, see:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
-- and
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
--
-- 'eac3AtmosSettings', 'audioCodecSettings_eac3AtmosSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
--
-- 'eac3Settings', 'audioCodecSettings_eac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
--
-- 'mp2Settings', 'audioCodecSettings_mp2Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
--
-- 'mp3Settings', 'audioCodecSettings_mp3Settings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
--
-- 'opusSettings', 'audioCodecSettings_opusSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
--
-- 'vorbisSettings', 'audioCodecSettings_vorbisSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
--
-- 'wavSettings', 'audioCodecSettings_wavSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
newAudioCodecSettings ::
  AudioCodecSettings
newAudioCodecSettings =
  AudioCodecSettings'
    { aacSettings = Prelude.Nothing,
      ac3Settings = Prelude.Nothing,
      aiffSettings = Prelude.Nothing,
      codec = Prelude.Nothing,
      eac3AtmosSettings = Prelude.Nothing,
      eac3Settings = Prelude.Nothing,
      mp2Settings = Prelude.Nothing,
      mp3Settings = Prelude.Nothing,
      opusSettings = Prelude.Nothing,
      vorbisSettings = Prelude.Nothing,
      wavSettings = Prelude.Nothing
    }

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
-- to the value AC3.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
audioCodecSettings_aiffSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AiffSettings)
audioCodecSettings_aiffSettings = Lens.lens (\AudioCodecSettings' {aiffSettings} -> aiffSettings) (\s@AudioCodecSettings' {} a -> s {aiffSettings = a} :: AudioCodecSettings)

-- | Choose the audio codec for this output. Note that the option Dolby
-- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
-- Dolby Digital Plus audio inputs. Make sure that you choose a codec
-- that\'s supported with your output container:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
-- For audio-only outputs, make sure that both your input audio codec and
-- your output audio codec are supported for audio-only workflows. For more
-- information, see:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
-- and
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
audioCodecSettings_codec :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AudioCodec)
audioCodecSettings_codec = Lens.lens (\AudioCodecSettings' {codec} -> codec) (\s@AudioCodecSettings' {} a -> s {codec = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
audioCodecSettings_eac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3AtmosSettings)
audioCodecSettings_eac3AtmosSettings = Lens.lens (\AudioCodecSettings' {eac3AtmosSettings} -> eac3AtmosSettings) (\s@AudioCodecSettings' {} a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
audioCodecSettings_mp3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp3Settings)
audioCodecSettings_mp3Settings = Lens.lens (\AudioCodecSettings' {mp3Settings} -> mp3Settings) (\s@AudioCodecSettings' {} a -> s {mp3Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
audioCodecSettings_opusSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe OpusSettings)
audioCodecSettings_opusSettings = Lens.lens (\AudioCodecSettings' {opusSettings} -> opusSettings) (\s@AudioCodecSettings' {} a -> s {opusSettings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
audioCodecSettings_vorbisSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe VorbisSettings)
audioCodecSettings_vorbisSettings = Lens.lens (\AudioCodecSettings' {vorbisSettings} -> vorbisSettings) (\s@AudioCodecSettings' {} a -> s {vorbisSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

instance Data.FromJSON AudioCodecSettings where
  parseJSON =
    Data.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Prelude.<$> (x Data..:? "aacSettings")
            Prelude.<*> (x Data..:? "ac3Settings")
            Prelude.<*> (x Data..:? "aiffSettings")
            Prelude.<*> (x Data..:? "codec")
            Prelude.<*> (x Data..:? "eac3AtmosSettings")
            Prelude.<*> (x Data..:? "eac3Settings")
            Prelude.<*> (x Data..:? "mp2Settings")
            Prelude.<*> (x Data..:? "mp3Settings")
            Prelude.<*> (x Data..:? "opusSettings")
            Prelude.<*> (x Data..:? "vorbisSettings")
            Prelude.<*> (x Data..:? "wavSettings")
      )

instance Prelude.Hashable AudioCodecSettings where
  hashWithSalt _salt AudioCodecSettings' {..} =
    _salt
      `Prelude.hashWithSalt` aacSettings
      `Prelude.hashWithSalt` ac3Settings
      `Prelude.hashWithSalt` aiffSettings
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` eac3AtmosSettings
      `Prelude.hashWithSalt` eac3Settings
      `Prelude.hashWithSalt` mp2Settings
      `Prelude.hashWithSalt` mp3Settings
      `Prelude.hashWithSalt` opusSettings
      `Prelude.hashWithSalt` vorbisSettings
      `Prelude.hashWithSalt` wavSettings

instance Prelude.NFData AudioCodecSettings where
  rnf AudioCodecSettings' {..} =
    Prelude.rnf aacSettings
      `Prelude.seq` Prelude.rnf ac3Settings
      `Prelude.seq` Prelude.rnf aiffSettings
      `Prelude.seq` Prelude.rnf codec
      `Prelude.seq` Prelude.rnf eac3AtmosSettings
      `Prelude.seq` Prelude.rnf eac3Settings
      `Prelude.seq` Prelude.rnf mp2Settings
      `Prelude.seq` Prelude.rnf mp3Settings
      `Prelude.seq` Prelude.rnf opusSettings
      `Prelude.seq` Prelude.rnf vorbisSettings
      `Prelude.seq` Prelude.rnf wavSettings

instance Data.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("aacSettings" Data..=) Prelude.<$> aacSettings,
            ("ac3Settings" Data..=) Prelude.<$> ac3Settings,
            ("aiffSettings" Data..=) Prelude.<$> aiffSettings,
            ("codec" Data..=) Prelude.<$> codec,
            ("eac3AtmosSettings" Data..=)
              Prelude.<$> eac3AtmosSettings,
            ("eac3Settings" Data..=) Prelude.<$> eac3Settings,
            ("mp2Settings" Data..=) Prelude.<$> mp2Settings,
            ("mp3Settings" Data..=) Prelude.<$> mp3Settings,
            ("opusSettings" Data..=) Prelude.<$> opusSettings,
            ("vorbisSettings" Data..=)
              Prelude.<$> vorbisSettings,
            ("wavSettings" Data..=) Prelude.<$> wavSettings
          ]
      )
