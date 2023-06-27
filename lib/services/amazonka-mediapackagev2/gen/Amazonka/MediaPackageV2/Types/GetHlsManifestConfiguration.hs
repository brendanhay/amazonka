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
-- Module      : Amazonka.MediaPackageV2.Types.GetHlsManifestConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.GetHlsManifestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.ScteHls
import qualified Amazonka.Prelude as Prelude

-- | Retrieve the HTTP live streaming (HLS) manifest configuration.
--
-- /See:/ 'newGetHlsManifestConfiguration' smart constructor.
data GetHlsManifestConfiguration = GetHlsManifestConfiguration'
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
-- Create a value of 'GetHlsManifestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childManifestName', 'getHlsManifestConfiguration_childManifestName' - A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
--
-- 'manifestWindowSeconds', 'getHlsManifestConfiguration_manifestWindowSeconds' - The total duration (in seconds) of the manifest\'s content.
--
-- 'programDateTimeIntervalSeconds', 'getHlsManifestConfiguration_programDateTimeIntervalSeconds' - Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
--
-- 'scteHls', 'getHlsManifestConfiguration_scteHls' - Undocumented member.
--
-- 'manifestName', 'getHlsManifestConfiguration_manifestName' - A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
--
-- 'url', 'getHlsManifestConfiguration_url' - The egress domain URL for stream delivery from MediaPackage.
newGetHlsManifestConfiguration ::
  -- | 'manifestName'
  Prelude.Text ->
  -- | 'url'
  Prelude.Text ->
  GetHlsManifestConfiguration
newGetHlsManifestConfiguration pManifestName_ pUrl_ =
  GetHlsManifestConfiguration'
    { childManifestName =
        Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing,
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
getHlsManifestConfiguration_childManifestName :: Lens.Lens' GetHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
getHlsManifestConfiguration_childManifestName = Lens.lens (\GetHlsManifestConfiguration' {childManifestName} -> childManifestName) (\s@GetHlsManifestConfiguration' {} a -> s {childManifestName = a} :: GetHlsManifestConfiguration)

-- | The total duration (in seconds) of the manifest\'s content.
getHlsManifestConfiguration_manifestWindowSeconds :: Lens.Lens' GetHlsManifestConfiguration (Prelude.Maybe Prelude.Int)
getHlsManifestConfiguration_manifestWindowSeconds = Lens.lens (\GetHlsManifestConfiguration' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@GetHlsManifestConfiguration' {} a -> s {manifestWindowSeconds = a} :: GetHlsManifestConfiguration)

-- | Inserts EXT-X-PROGRAM-DATE-TIME tags in the output manifest at the
-- interval that you specify. If you don\'t enter an interval,
-- EXT-X-PROGRAM-DATE-TIME tags aren\'t included in the manifest. The tags
-- sync the stream to the wall clock so that viewers can seek to a specific
-- time in the playback timeline on the player. ID3Timed metadata messages
-- generate every 5 seconds whenever the content is ingested.
--
-- Irrespective of this parameter, if any ID3Timed metadata is in the HLS
-- input, it is passed through to the HLS output.
getHlsManifestConfiguration_programDateTimeIntervalSeconds :: Lens.Lens' GetHlsManifestConfiguration (Prelude.Maybe Prelude.Int)
getHlsManifestConfiguration_programDateTimeIntervalSeconds = Lens.lens (\GetHlsManifestConfiguration' {programDateTimeIntervalSeconds} -> programDateTimeIntervalSeconds) (\s@GetHlsManifestConfiguration' {} a -> s {programDateTimeIntervalSeconds = a} :: GetHlsManifestConfiguration)

-- | Undocumented member.
getHlsManifestConfiguration_scteHls :: Lens.Lens' GetHlsManifestConfiguration (Prelude.Maybe ScteHls)
getHlsManifestConfiguration_scteHls = Lens.lens (\GetHlsManifestConfiguration' {scteHls} -> scteHls) (\s@GetHlsManifestConfiguration' {} a -> s {scteHls = a} :: GetHlsManifestConfiguration)

-- | A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
getHlsManifestConfiguration_manifestName :: Lens.Lens' GetHlsManifestConfiguration Prelude.Text
getHlsManifestConfiguration_manifestName = Lens.lens (\GetHlsManifestConfiguration' {manifestName} -> manifestName) (\s@GetHlsManifestConfiguration' {} a -> s {manifestName = a} :: GetHlsManifestConfiguration)

-- | The egress domain URL for stream delivery from MediaPackage.
getHlsManifestConfiguration_url :: Lens.Lens' GetHlsManifestConfiguration Prelude.Text
getHlsManifestConfiguration_url = Lens.lens (\GetHlsManifestConfiguration' {url} -> url) (\s@GetHlsManifestConfiguration' {} a -> s {url = a} :: GetHlsManifestConfiguration)

instance Data.FromJSON GetHlsManifestConfiguration where
  parseJSON =
    Data.withObject
      "GetHlsManifestConfiguration"
      ( \x ->
          GetHlsManifestConfiguration'
            Prelude.<$> (x Data..:? "ChildManifestName")
            Prelude.<*> (x Data..:? "ManifestWindowSeconds")
            Prelude.<*> (x Data..:? "ProgramDateTimeIntervalSeconds")
            Prelude.<*> (x Data..:? "ScteHls")
            Prelude.<*> (x Data..: "ManifestName")
            Prelude.<*> (x Data..: "Url")
      )

instance Prelude.Hashable GetHlsManifestConfiguration where
  hashWithSalt _salt GetHlsManifestConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` childManifestName
      `Prelude.hashWithSalt` manifestWindowSeconds
      `Prelude.hashWithSalt` programDateTimeIntervalSeconds
      `Prelude.hashWithSalt` scteHls
      `Prelude.hashWithSalt` manifestName
      `Prelude.hashWithSalt` url

instance Prelude.NFData GetHlsManifestConfiguration where
  rnf GetHlsManifestConfiguration' {..} =
    Prelude.rnf childManifestName
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf programDateTimeIntervalSeconds
      `Prelude.seq` Prelude.rnf scteHls
      `Prelude.seq` Prelude.rnf manifestName
      `Prelude.seq` Prelude.rnf url
