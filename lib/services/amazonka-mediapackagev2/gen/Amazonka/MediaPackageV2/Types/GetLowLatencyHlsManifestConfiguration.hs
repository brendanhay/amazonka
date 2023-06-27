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
-- Module      : Amazonka.MediaPackageV2.Types.GetLowLatencyHlsManifestConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.GetLowLatencyHlsManifestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.ScteHls
import qualified Amazonka.Prelude as Prelude

-- | Retrieve the low-latency HTTP live streaming (HLS) manifest
-- configuration.
--
-- /See:/ 'newGetLowLatencyHlsManifestConfiguration' smart constructor.
data GetLowLatencyHlsManifestConfiguration = GetLowLatencyHlsManifestConfiguration'
  { -- | A short string that\'s appended to the endpoint URL. The child manifest
    -- name creates a unique path to this endpoint. If you don\'t enter a
    -- value, MediaPackage uses the default child manifest name, index_1. The
    -- manifestName on the HLSManifest object overrides the manifestName you
    -- provided on the originEndpoint object.
    childManifestName :: Prelude.Maybe Prelude.Text,
    -- | The total duration (in seconds) of the manifest\'s content.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
    -- interval that you specify. If you don\'t enter an interval,
    -- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
    -- sync the stream to the wall clock so that viewers can seek to a specific
    -- time in the playback timeline on the player. ID3Timed metadata messages
    -- generate every 5 seconds whenever the content is ingested.
    --
    -- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
    -- input, it is passed through to the HLS output.
    programDateTimeIntervalSeconds :: Prelude.Maybe Prelude.Int,
    scteHls :: Prelude.Maybe ScteHls,
    -- | A short short string that\'s appended to the endpoint URL. The manifest
    -- name creates a unique path to this endpoint. If you don\'t enter a
    -- value, MediaPackage uses the default manifest name, index. MediaPackage
    -- automatically inserts the format extension, such as .m3u8. You can\'t
    -- use the same manifest name if you use HLS manifest and low-latency HLS
    -- manifest. The manifestName on the HLSManifest object overrides the
    -- manifestName you provided on the originEndpoint object.
    manifestName :: Prelude.Text,
    -- | The egress domain URL for stream delivery from MediaPackage.
    url :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLowLatencyHlsManifestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childManifestName', 'getLowLatencyHlsManifestConfiguration_childManifestName' - A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
--
-- 'manifestWindowSeconds', 'getLowLatencyHlsManifestConfiguration_manifestWindowSeconds' - The total duration (in seconds) of the manifest\'s content.
--
-- 'programDateTimeIntervalSeconds', 'getLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds' - Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
--
-- 'scteHls', 'getLowLatencyHlsManifestConfiguration_scteHls' - Undocumented member.
--
-- 'manifestName', 'getLowLatencyHlsManifestConfiguration_manifestName' - A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
--
-- 'url', 'getLowLatencyHlsManifestConfiguration_url' - The egress domain URL for stream delivery from MediaPackage.
newGetLowLatencyHlsManifestConfiguration ::
  -- | 'manifestName'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  GetLowLatencyHlsManifestConfiguration
newGetLowLatencyHlsManifestConfiguration
  pManifestName_
  pUrl_ =
    GetLowLatencyHlsManifestConfiguration'
      { childManifestName =
          Prelude.Nothing,
        manifestWindowSeconds =
          Prelude.Nothing,
        programDateTimeIntervalSeconds =
          Prelude.Nothing,
        scteHls = Prelude.Nothing,
        manifestName = pManifestName_,
        url = pUrl_
      }

-- | A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
getLowLatencyHlsManifestConfiguration_childManifestName :: Lens.Lens' GetLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
getLowLatencyHlsManifestConfiguration_childManifestName = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {childManifestName} -> childManifestName) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {childManifestName = a} :: GetLowLatencyHlsManifestConfiguration)

-- | The total duration (in seconds) of the manifest\'s content.
getLowLatencyHlsManifestConfiguration_manifestWindowSeconds :: Lens.Lens' GetLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Int)
getLowLatencyHlsManifestConfiguration_manifestWindowSeconds = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {manifestWindowSeconds = a} :: GetLowLatencyHlsManifestConfiguration)

-- | Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
getLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds :: Lens.Lens' GetLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Int)
getLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {programDateTimeIntervalSeconds = a} :: GetLowLatencyHlsManifestConfiguration)

-- | Undocumented member.
getLowLatencyHlsManifestConfiguration_scteHls :: Lens.Lens' GetLowLatencyHlsManifestConfiguration (Prelude.Maybe ScteHls)
getLowLatencyHlsManifestConfiguration_scteHls = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {scteHls} -> scteHls) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {scteHls = a} :: GetLowLatencyHlsManifestConfiguration)

-- | A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
getLowLatencyHlsManifestConfiguration_manifestName :: Lens.Lens' GetLowLatencyHlsManifestConfiguration Prelude.Text
getLowLatencyHlsManifestConfiguration_manifestName = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {manifestName} -> manifestName) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {manifestName = a} :: GetLowLatencyHlsManifestConfiguration)

-- | The egress domain URL for stream delivery from MediaPackage.
getLowLatencyHlsManifestConfiguration_url :: Lens.Lens' GetLowLatencyHlsManifestConfiguration Prelude.Text
getLowLatencyHlsManifestConfiguration_url = Lens.lens (\GetLowLatencyHlsManifestConfiguration' {url} -> url) (\s@GetLowLatencyHlsManifestConfiguration' {} a -> s {url = a} :: GetLowLatencyHlsManifestConfiguration)

instance
  Data.FromJSON
    GetLowLatencyHlsManifestConfiguration
  where
  parseJSON =
    Data.withObject
      "GetLowLatencyHlsManifestConfiguration"
      ( \x ->
          GetLowLatencyHlsManifestConfiguration'
            Prelude.<$> (x Data..:? "ChildManifestName")
            Prelude.<*> (x Data..:? "ManifestWindowSeconds")
            Prelude.<*> (x Data..:? "ProgramDateTimeIntervalSeconds")
            Prelude.<*> (x Data..:? "ScteHls")
            Prelude.<*> (x Data..: "ManifestName")
            Prelude.<*> (x Data..: "Url")
      )

instance
  Prelude.Hashable
    GetLowLatencyHlsManifestConfiguration
  where
  hashWithSalt
    _salt
    GetLowLatencyHlsManifestConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` childManifestName
        `Prelude.hashWithSalt` manifestWindowSeconds
        `Prelude.hashWithSalt` programDateTimeIntervalSeconds
        `Prelude.hashWithSalt` scteHls
        `Prelude.hashWithSalt` manifestName
        `Prelude.hashWithSalt` url

instance
  Prelude.NFData
    GetLowLatencyHlsManifestConfiguration
  where
  rnf GetLowLatencyHlsManifestConfiguration' {..} =
    Prelude.rnf childManifestName
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf scteHls
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf url
