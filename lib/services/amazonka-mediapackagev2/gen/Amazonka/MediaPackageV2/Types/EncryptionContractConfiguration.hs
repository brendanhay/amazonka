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
-- Module      : Amazonka.MediaPackageV2.Types.EncryptionContractConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.EncryptionContractConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.PresetSpeke20Audio
import Amazonka.MediaPackageV2.Types.PresetSpeke20Video
import qualified Amazonka.Prelude as Prelude

-- | Configure one or more content encryption keys for your endpoints that
-- use SPEKE Version 2.0. The encryption contract defines which content
-- keys are used to encrypt the audio and video tracks in your stream. To
-- configure the encryption contract, specify which audio and video
-- encryption presets to use.
--
-- /See:/ 'newEncryptionContractConfiguration' smart constructor.
data EncryptionContractConfiguration = EncryptionContractConfiguration'
  { -- | A collection of audio encryption presets.
    --
    -- Value description:
    --
    -- -   PRESET-AUDIO-1 - Use one content key to encrypt all of the audio
    --     tracks in your stream.
    --
    -- -   PRESET-AUDIO-2 - Use one content key to encrypt all of the stereo
    --     audio tracks and one content key to encrypt all of the multichannel
    --     audio tracks.
    --
    -- -   PRESET-AUDIO-3 - Use one content key to encrypt all of the stereo
    --     audio tracks, one content key to encrypt all of the multichannel
    --     audio tracks with 3 to 6 channels, and one content key to encrypt
    --     all of the multichannel audio tracks with more than 6 channels.
    --
    -- -   SHARED - Use the same content key for all of the audio and video
    --     tracks in your stream.
    --
    -- -   UNENCRYPTED - Don\'t encrypt any of the audio tracks in your stream.
    presetSpeke20Audio :: PresetSpeke20Audio,
    -- | A collection of video encryption presets.
    --
    -- Value description:
    --
    -- -   PRESET-VIDEO-1 - Use one content key to encrypt all of the video
    --     tracks in your stream.
    --
    -- -   PRESET-VIDEO-2 - Use one content key to encrypt all of the SD video
    --     tracks and one content key for all HD and higher resolutions video
    --     tracks.
    --
    -- -   PRESET-VIDEO-3 - Use one content key to encrypt all of the SD video
    --     tracks, one content key for HD video tracks and one content key for
    --     all UHD video tracks.
    --
    -- -   PRESET-VIDEO-4 - Use one content key to encrypt all of the SD video
    --     tracks, one content key for HD video tracks, one content key for all
    --     UHD1 video tracks and one content key for all UHD2 video tracks.
    --
    -- -   PRESET-VIDEO-5 - Use one content key to encrypt all of the SD video
    --     tracks, one content key for HD1 video tracks, one content key for
    --     HD2 video tracks, one content key for all UHD1 video tracks and one
    --     content key for all UHD2 video tracks.
    --
    -- -   PRESET-VIDEO-6 - Use one content key to encrypt all of the SD video
    --     tracks, one content key for HD1 video tracks, one content key for
    --     HD2 video tracks and one content key for all UHD video tracks.
    --
    -- -   PRESET-VIDEO-7 - Use one content key to encrypt all of the SD+HD1
    --     video tracks, one content key for HD2 video tracks and one content
    --     key for all UHD video tracks.
    --
    -- -   PRESET-VIDEO-8 - Use one content key to encrypt all of the SD+HD1
    --     video tracks, one content key for HD2 video tracks, one content key
    --     for all UHD1 video tracks and one content key for all UHD2 video
    --     tracks.
    --
    -- -   SHARED - Use the same content key for all of the video and audio
    --     tracks in your stream.
    --
    -- -   UNENCRYPTED - Don\'t encrypt any of the video tracks in your stream.
    presetSpeke20Video :: PresetSpeke20Video
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionContractConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'presetSpeke20Audio', 'encryptionContractConfiguration_presetSpeke20Audio' - A collection of audio encryption presets.
--
-- Value description:
--
-- -   PRESET-AUDIO-1 - Use one content key to encrypt all of the audio
--     tracks in your stream.
--
-- -   PRESET-AUDIO-2 - Use one content key to encrypt all of the stereo
--     audio tracks and one content key to encrypt all of the multichannel
--     audio tracks.
--
-- -   PRESET-AUDIO-3 - Use one content key to encrypt all of the stereo
--     audio tracks, one content key to encrypt all of the multichannel
--     audio tracks with 3 to 6 channels, and one content key to encrypt
--     all of the multichannel audio tracks with more than 6 channels.
--
-- -   SHARED - Use the same content key for all of the audio and video
--     tracks in your stream.
--
-- -   UNENCRYPTED - Don\'t encrypt any of the audio tracks in your stream.
--
-- 'presetSpeke20Video', 'encryptionContractConfiguration_presetSpeke20Video' - A collection of video encryption presets.
--
-- Value description:
--
-- -   PRESET-VIDEO-1 - Use one content key to encrypt all of the video
--     tracks in your stream.
--
-- -   PRESET-VIDEO-2 - Use one content key to encrypt all of the SD video
--     tracks and one content key for all HD and higher resolutions video
--     tracks.
--
-- -   PRESET-VIDEO-3 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD video tracks and one content key for
--     all UHD video tracks.
--
-- -   PRESET-VIDEO-4 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD video tracks, one content key for all
--     UHD1 video tracks and one content key for all UHD2 video tracks.
--
-- -   PRESET-VIDEO-5 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD1 video tracks, one content key for
--     HD2 video tracks, one content key for all UHD1 video tracks and one
--     content key for all UHD2 video tracks.
--
-- -   PRESET-VIDEO-6 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD1 video tracks, one content key for
--     HD2 video tracks and one content key for all UHD video tracks.
--
-- -   PRESET-VIDEO-7 - Use one content key to encrypt all of the SD+HD1
--     video tracks, one content key for HD2 video tracks and one content
--     key for all UHD video tracks.
--
-- -   PRESET-VIDEO-8 - Use one content key to encrypt all of the SD+HD1
--     video tracks, one content key for HD2 video tracks, one content key
--     for all UHD1 video tracks and one content key for all UHD2 video
--     tracks.
--
-- -   SHARED - Use the same content key for all of the video and audio
--     tracks in your stream.
--
-- -   UNENCRYPTED - Don\'t encrypt any of the video tracks in your stream.
newEncryptionContractConfiguration ::
  -- | 'presetSpeke20Audio'
  PresetSpeke20Audio ->
  -- | 'presetSpeke20Video'
  PresetSpeke20Video ->
  EncryptionContractConfiguration
newEncryptionContractConfiguration
  pPresetSpeke20Audio_
  pPresetSpeke20Video_ =
    EncryptionContractConfiguration'
      { presetSpeke20Audio =
          pPresetSpeke20Audio_,
        presetSpeke20Video = pPresetSpeke20Video_
      }

-- | A collection of audio encryption presets.
--
-- Value description:
--
-- -   PRESET-AUDIO-1 - Use one content key to encrypt all of the audio
--     tracks in your stream.
--
-- -   PRESET-AUDIO-2 - Use one content key to encrypt all of the stereo
--     audio tracks and one content key to encrypt all of the multichannel
--     audio tracks.
--
-- -   PRESET-AUDIO-3 - Use one content key to encrypt all of the stereo
--     audio tracks, one content key to encrypt all of the multichannel
--     audio tracks with 3 to 6 channels, and one content key to encrypt
--     all of the multichannel audio tracks with more than 6 channels.
--
-- -   SHARED - Use the same content key for all of the audio and video
--     tracks in your stream.
--
-- -   UNENCRYPTED - Don\'t encrypt any of the audio tracks in your stream.
encryptionContractConfiguration_presetSpeke20Audio :: Lens.Lens' EncryptionContractConfiguration PresetSpeke20Audio
encryptionContractConfiguration_presetSpeke20Audio = Lens.lens (\EncryptionContractConfiguration' {presetSpeke20Audio} -> presetSpeke20Audio) (\s@EncryptionContractConfiguration' {} a -> s {presetSpeke20Audio = a} :: EncryptionContractConfiguration)

-- | A collection of video encryption presets.
--
-- Value description:
--
-- -   PRESET-VIDEO-1 - Use one content key to encrypt all of the video
--     tracks in your stream.
--
-- -   PRESET-VIDEO-2 - Use one content key to encrypt all of the SD video
--     tracks and one content key for all HD and higher resolutions video
--     tracks.
--
-- -   PRESET-VIDEO-3 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD video tracks and one content key for
--     all UHD video tracks.
--
-- -   PRESET-VIDEO-4 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD video tracks, one content key for all
--     UHD1 video tracks and one content key for all UHD2 video tracks.
--
-- -   PRESET-VIDEO-5 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD1 video tracks, one content key for
--     HD2 video tracks, one content key for all UHD1 video tracks and one
--     content key for all UHD2 video tracks.
--
-- -   PRESET-VIDEO-6 - Use one content key to encrypt all of the SD video
--     tracks, one content key for HD1 video tracks, one content key for
--     HD2 video tracks and one content key for all UHD video tracks.
--
-- -   PRESET-VIDEO-7 - Use one content key to encrypt all of the SD+HD1
--     video tracks, one content key for HD2 video tracks and one content
--     key for all UHD video tracks.
--
-- -   PRESET-VIDEO-8 - Use one content key to encrypt all of the SD+HD1
--     video tracks, one content key for HD2 video tracks, one content key
--     for all UHD1 video tracks and one content key for all UHD2 video
--     tracks.
--
-- -   SHARED - Use the same content key for all of the video and audio
--     tracks in your stream.
--
-- -   UNENCRYPTED - Don\'t encrypt any of the video tracks in your stream.
encryptionContractConfiguration_presetSpeke20Video :: Lens.Lens' EncryptionContractConfiguration PresetSpeke20Video
encryptionContractConfiguration_presetSpeke20Video = Lens.lens (\EncryptionContractConfiguration' {presetSpeke20Video} -> presetSpeke20Video) (\s@EncryptionContractConfiguration' {} a -> s {presetSpeke20Video = a} :: EncryptionContractConfiguration)

instance
  Data.FromJSON
    EncryptionContractConfiguration
  where
  parseJSON =
    Data.withObject
      "EncryptionContractConfiguration"
      ( \x ->
          EncryptionContractConfiguration'
            Prelude.<$> (x Data..: "PresetSpeke20Audio")
            Prelude.<*> (x Data..: "PresetSpeke20Video")
      )

instance
  Prelude.Hashable
    EncryptionContractConfiguration
  where
  hashWithSalt
    _salt
    EncryptionContractConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` presetSpeke20Audio
        `Prelude.hashWithSalt` presetSpeke20Video

instance
  Prelude.NFData
    EncryptionContractConfiguration
  where
  rnf EncryptionContractConfiguration' {..} =
    Prelude.rnf presetSpeke20Audio
      `Prelude.seq` Prelude.rnf presetSpeke20Video

instance Data.ToJSON EncryptionContractConfiguration where
  toJSON EncryptionContractConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PresetSpeke20Audio" Data..= presetSpeke20Audio),
            Prelude.Just
              ("PresetSpeke20Video" Data..= presetSpeke20Video)
          ]
      )
