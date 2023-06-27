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
-- Module      : Amazonka.MediaPackageV2.Types.CreateLowLatencyHlsManifestConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.CreateLowLatencyHlsManifestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.ScteHls
import qualified Amazonka.Prelude as Prelude

-- | Create a low-latency HTTP live streaming (HLS) manifest configuration.
--
-- /See:/ 'newCreateLowLatencyHlsManifestConfiguration' smart constructor.
data CreateLowLatencyHlsManifestConfiguration = CreateLowLatencyHlsManifestConfiguration'
  { -- | A short string that\'s appended to the endpoint URL. The child manifest
    -- name creates a unique path to this endpoint. If you don\'t enter a
    -- value, MediaPackage uses the default manifest name, index, with an added
    -- suffix to distinguish it from the manifest name. The manifestName on the
    -- HLSManifest object overrides the manifestName you provided on the
    -- originEndpoint object.
    childManifestName :: Prelude.Maybe Prelude.Text,
    -- | The total duration (in seconds) of the manifest\'s content.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
    -- interval that you specify. If you don\'t enter an interval,
    -- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
    -- sync the stream to the wall clock so that viewers can seek to a specific
    -- time in the playback timeline on the player. ID3Timed metadata messages
    -- generate every 5 seconds whenever the content is ingested.
    --
    -- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
    -- input, it is passed through to the HLS output.
    programDateTimeIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    scteHls :: Prelude.Maybe ScteHls,
    -- | A short short string that\'s appended to the endpoint URL. The manifest
    -- name creates a unique path to this endpoint. If you don\'t enter a
    -- value, MediaPackage uses the default manifest name, index. MediaPackage
    -- automatically inserts the format extension, such as .m3u8. You can\'t
    -- use the same manifest name if you use HLS manifest and low-latency HLS
    -- manifest. The manifestName on the HLSManifest object overrides the
    -- manifestName you provided on the originEndpoint object.
    manifestName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLowLatencyHlsManifestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childManifestName', 'createLowLatencyHlsManifestConfiguration_childManifestName' - A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index, with an added
-- suffix to distinguish it from the manifest name. The manifestName on the
-- HLSManifest object overrides the manifestName you provided on the
-- originEndpoint object.
--
-- 'manifestWindowSeconds', 'createLowLatencyHlsManifestConfiguration_manifestWindowSeconds' - The total duration (in seconds) of the manifest\'s content.
--
-- 'programDateTimeIntervalSeconds', 'createLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds' - Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
--
-- 'scteHls', 'createLowLatencyHlsManifestConfiguration_scteHls' - Undocumented member.
--
-- 'manifestName', 'createLowLatencyHlsManifestConfiguration_manifestName' - A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
newCreateLowLatencyHlsManifestConfiguration ::
  -- | 'manifestName'
  Prelude.Text ->
  CreateLowLatencyHlsManifestConfiguration
newCreateLowLatencyHlsManifestConfiguration
  pManifestName_ =
    CreateLowLatencyHlsManifestConfiguration'
      { childManifestName =
          Prelude.Nothing,
        manifestWindowSeconds =
          Prelude.Nothing,
        programDateTimeIntervalSeconds =
          Prelude.Nothing,
        scteHls = Prelude.Nothing,
        manifestName = pManifestName_
      }

-- | A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index, with an added
-- suffix to distinguish it from the manifest name. The manifestName on the
-- HLSManifest object overrides the manifestName you provided on the
-- originEndpoint object.
createLowLatencyHlsManifestConfiguration_childManifestName :: Lens.Lens' CreateLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
createLowLatencyHlsManifestConfiguration_childManifestName = Lens.lens (\CreateLowLatencyHlsManifestConfiguration' {childManifestName} -> childManifestName) (\s@CreateLowLatencyHlsManifestConfiguration' {} a -> s {childManifestName = a} :: CreateLowLatencyHlsManifestConfiguration)

-- | The total duration (in seconds) of the manifest\'s content.
createLowLatencyHlsManifestConfiguration_manifestWindowSeconds :: Lens.Lens' CreateLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Natural)
createLowLatencyHlsManifestConfiguration_manifestWindowSeconds = Lens.lens (\CreateLowLatencyHlsManifestConfiguration' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@CreateLowLatencyHlsManifestConfiguration' {} a -> s {manifestWindowSeconds = a} :: CreateLowLatencyHlsManifestConfiguration)

-- | Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
createLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds :: Lens.Lens' CreateLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Natural)
createLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds = Lens.lens (\CreateLowLatencyHlsManifestConfiguration' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@CreateLowLatencyHlsManifestConfiguration' {} a -> s {programDateTimeIntervalSeconds = a} :: CreateLowLatencyHlsManifestConfiguration)

-- | Undocumented member.
createLowLatencyHlsManifestConfiguration_scteHls :: Lens.Lens' CreateLowLatencyHlsManifestConfiguration (Prelude.Maybe ScteHls)
createLowLatencyHlsManifestConfiguration_scteHls = Lens.lens (\CreateLowLatencyHlsManifestConfiguration' {scteHls} -> scteHls) (\s@CreateLowLatencyHlsManifestConfiguration' {} a -> s {scteHls = a} :: CreateLowLatencyHlsManifestConfiguration)

-- | A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
createLowLatencyHlsManifestConfiguration_manifestName :: Lens.Lens' CreateLowLatencyHlsManifestConfiguration Prelude.Text
createLowLatencyHlsManifestConfiguration_manifestName = Lens.lens (\CreateLowLatencyHlsManifestConfiguration' {manifestName} -> manifestName) (\s@CreateLowLatencyHlsManifestConfiguration' {} a -> s {manifestName = a} :: CreateLowLatencyHlsManifestConfiguration)

instance
  Prelude.Hashable
    CreateLowLatencyHlsManifestConfiguration
  where
  hashWithSalt
    _salt
    CreateLowLatencyHlsManifestConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` childManifestName
        `Prelude.hashWithSalt` manifestWindowSeconds
        `Prelude.hashWithSalt` programDateTimeIntervalSeconds
        `Prelude.hashWithSalt` scteHls
        `Prelude.hashWithSalt` manifestName

instance
  Prelude.NFData
    CreateLowLatencyHlsManifestConfiguration
  where
  rnf CreateLowLatencyHlsManifestConfiguration' {..} =
    Prelude.rnf childManifestName
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf scteHls
      `Prelude.seq` Prelude.rnf manifestName

instance
  Data.ToJSON
    CreateLowLatencyHlsManifestConfiguration
  where
  toJSON CreateLowLatencyHlsManifestConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChildManifestName" Data..=)
              Prelude.<$> childManifestName,
            ("ManifestWindowSeconds" Data..=)
              Prelude.<$> manifestWindowSeconds,
            ("ProgramDateTimeIntervalSeconds" Data..=)
              Prelude.<$> programDateTimeIntervalSeconds,
            ("ScteHls" Data..=) Prelude.<$> scteHls,
            Prelude.Just ("ManifestName" Data..= manifestName)
          ]
      )
