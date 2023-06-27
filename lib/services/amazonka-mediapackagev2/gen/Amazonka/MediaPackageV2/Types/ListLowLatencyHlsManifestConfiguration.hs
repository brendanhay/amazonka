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
-- Module      : Amazonka.MediaPackageV2.Types.ListLowLatencyHlsManifestConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ListLowLatencyHlsManifestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List the low-latency HTTP live streaming (HLS) manifest configuration.
--
-- /See:/ 'newListLowLatencyHlsManifestConfiguration' smart constructor.
data ListLowLatencyHlsManifestConfiguration = ListLowLatencyHlsManifestConfiguration'
  { -- | A short string that\'s appended to the endpoint URL. The child manifest
    -- name creates a unique path to this endpoint. If you don\'t enter a
    -- value, MediaPackage uses the default child manifest name, index_1. The
    -- manifestName on the HLSManifest object overrides the manifestName you
    -- provided on the originEndpoint object.
    childManifestName :: Prelude.Maybe Prelude.Text,
    -- | The egress domain URL for stream delivery from MediaPackage.
    url :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListLowLatencyHlsManifestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childManifestName', 'listLowLatencyHlsManifestConfiguration_childManifestName' - A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
--
-- 'url', 'listLowLatencyHlsManifestConfiguration_url' - The egress domain URL for stream delivery from MediaPackage.
--
-- 'manifestName', 'listLowLatencyHlsManifestConfiguration_manifestName' - A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
newListLowLatencyHlsManifestConfiguration ::
  -- | 'manifestName'
  Prelude.Text ->
  ListLowLatencyHlsManifestConfiguration
newListLowLatencyHlsManifestConfiguration
  pManifestName_ =
    ListLowLatencyHlsManifestConfiguration'
      { childManifestName =
          Prelude.Nothing,
        url = Prelude.Nothing,
        manifestName = pManifestName_
      }

-- | A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
listLowLatencyHlsManifestConfiguration_childManifestName :: Lens.Lens' ListLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
listLowLatencyHlsManifestConfiguration_childManifestName = Lens.lens (\ListLowLatencyHlsManifestConfiguration' {childManifestName} -> childManifestName) (\s@ListLowLatencyHlsManifestConfiguration' {} a -> s {childManifestName = a} :: ListLowLatencyHlsManifestConfiguration)

-- | The egress domain URL for stream delivery from MediaPackage.
listLowLatencyHlsManifestConfiguration_url :: Lens.Lens' ListLowLatencyHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
listLowLatencyHlsManifestConfiguration_url = Lens.lens (\ListLowLatencyHlsManifestConfiguration' {url} -> url) (\s@ListLowLatencyHlsManifestConfiguration' {} a -> s {url = a} :: ListLowLatencyHlsManifestConfiguration)

-- | A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
listLowLatencyHlsManifestConfiguration_manifestName :: Lens.Lens' ListLowLatencyHlsManifestConfiguration Prelude.Text
listLowLatencyHlsManifestConfiguration_manifestName = Lens.lens (\ListLowLatencyHlsManifestConfiguration' {manifestName} -> manifestName) (\s@ListLowLatencyHlsManifestConfiguration' {} a -> s {manifestName = a} :: ListLowLatencyHlsManifestConfiguration)

instance
  Data.FromJSON
    ListLowLatencyHlsManifestConfiguration
  where
  parseJSON =
    Data.withObject
      "ListLowLatencyHlsManifestConfiguration"
      ( \x ->
          ListLowLatencyHlsManifestConfiguration'
            Prelude.<$> (x Data..:? "ChildManifestName")
            Prelude.<*> (x Data..:? "Url")
            Prelude.<*> (x Data..: "ManifestName")
      )

instance
  Prelude.Hashable
    ListLowLatencyHlsManifestConfiguration
  where
  hashWithSalt
    _salt
    ListLowLatencyHlsManifestConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` childManifestName
        `Prelude.hashWithSalt` url
        `Prelude.hashWithSalt` manifestName

instance
  Prelude.NFData
    ListLowLatencyHlsManifestConfiguration
  where
  rnf ListLowLatencyHlsManifestConfiguration' {..} =
    Prelude.rnf childManifestName
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf manifestName
