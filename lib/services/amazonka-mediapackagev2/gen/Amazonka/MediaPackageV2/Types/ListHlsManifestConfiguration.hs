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
-- Module      : Amazonka.MediaPackageV2.Types.ListHlsManifestConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ListHlsManifestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List the HTTP live streaming (HLS) manifest configuration.
--
-- /See:/ 'newListHlsManifestConfiguration' smart constructor.
data ListHlsManifestConfiguration = ListHlsManifestConfiguration'
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
-- Create a value of 'ListHlsManifestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childManifestName', 'listHlsManifestConfiguration_childManifestName' - A short string that\'s appended to the endpoint URL. The child manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default child manifest name, index_1. The
-- manifestName on the HLSManifest object overrides the manifestName you
-- provided on the originEndpoint object.
--
-- 'url', 'listHlsManifestConfiguration_url' - The egress domain URL for stream delivery from MediaPackage.
--
-- 'manifestName', 'listHlsManifestConfiguration_manifestName' - A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
newListHlsManifestConfiguration ::
  -- | 'manifestName'
  Prelude.Text ->
  ListHlsManifestConfiguration
newListHlsManifestConfiguration pManifestName_ =
  ListHlsManifestConfiguration'
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
listHlsManifestConfiguration_childManifestName :: Lens.Lens' ListHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
listHlsManifestConfiguration_childManifestName = Lens.lens (\ListHlsManifestConfiguration' {childManifestName} -> childManifestName) (\s@ListHlsManifestConfiguration' {} a -> s {childManifestName = a} :: ListHlsManifestConfiguration)

-- | The egress domain URL for stream delivery from MediaPackage.
listHlsManifestConfiguration_url :: Lens.Lens' ListHlsManifestConfiguration (Prelude.Maybe Prelude.Text)
listHlsManifestConfiguration_url = Lens.lens (\ListHlsManifestConfiguration' {url} -> url) (\s@ListHlsManifestConfiguration' {} a -> s {url = a} :: ListHlsManifestConfiguration)

-- | A short short string that\'s appended to the endpoint URL. The manifest
-- name creates a unique path to this endpoint. If you don\'t enter a
-- value, MediaPackage uses the default manifest name, index. MediaPackage
-- automatically inserts the format extension, such as .m3u8. You can\'t
-- use the same manifest name if you use HLS manifest and low-latency HLS
-- manifest. The manifestName on the HLSManifest object overrides the
-- manifestName you provided on the originEndpoint object.
listHlsManifestConfiguration_manifestName :: Lens.Lens' ListHlsManifestConfiguration Prelude.Text
listHlsManifestConfiguration_manifestName = Lens.lens (\ListHlsManifestConfiguration' {manifestName} -> manifestName) (\s@ListHlsManifestConfiguration' {} a -> s {manifestName = a} :: ListHlsManifestConfiguration)

instance Data.FromJSON ListHlsManifestConfiguration where
  parseJSON =
    Data.withObject
      "ListHlsManifestConfiguration"
      ( \x ->
          ListHlsManifestConfiguration'
            Prelude.<$> (x Data..:? "ChildManifestName")
            Prelude.<*> (x Data..:? "Url")
            Prelude.<*> (x Data..: "ManifestName")
      )

instance
  Prelude.Hashable
    ListHlsManifestConfiguration
  where
  hashWithSalt _salt ListHlsManifestConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` childManifestName
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` manifestName

instance Prelude.NFData ListHlsManifestConfiguration where
  rnf ListHlsManifestConfiguration' {..} =
    Prelude.rnf childManifestName
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf manifestName
