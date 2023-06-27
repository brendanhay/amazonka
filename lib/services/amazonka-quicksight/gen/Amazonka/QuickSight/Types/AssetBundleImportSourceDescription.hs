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
-- Module      : Amazonka.QuickSight.Types.AssetBundleImportSourceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AssetBundleImportSourceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A description of the import source that you provide at the start of an
-- import job. This value is set to either @Body@ or @S3Uri@, depending on
-- how the @StartAssetBundleImportJobRequest@ is configured.
--
-- /See:/ 'newAssetBundleImportSourceDescription' smart constructor.
data AssetBundleImportSourceDescription = AssetBundleImportSourceDescription'
  { -- | An HTTPS download URL for the provided asset bundle that you optionally
    -- provided at the start of the import job. This URL is valid for five
    -- minutes after issuance. Call @DescribeAssetBundleExportJob@ again for a
    -- fresh URL if needed. The downloaded asset bundle is a @.qs@ zip file.
    body :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI that you provided at the start of the import job.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetBundleImportSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'assetBundleImportSourceDescription_body' - An HTTPS download URL for the provided asset bundle that you optionally
-- provided at the start of the import job. This URL is valid for five
-- minutes after issuance. Call @DescribeAssetBundleExportJob@ again for a
-- fresh URL if needed. The downloaded asset bundle is a @.qs@ zip file.
--
-- 's3Uri', 'assetBundleImportSourceDescription_s3Uri' - The Amazon S3 URI that you provided at the start of the import job.
newAssetBundleImportSourceDescription ::
  AssetBundleImportSourceDescription
newAssetBundleImportSourceDescription =
  AssetBundleImportSourceDescription'
    { body =
        Prelude.Nothing,
      s3Uri = Prelude.Nothing
    }

-- | An HTTPS download URL for the provided asset bundle that you optionally
-- provided at the start of the import job. This URL is valid for five
-- minutes after issuance. Call @DescribeAssetBundleExportJob@ again for a
-- fresh URL if needed. The downloaded asset bundle is a @.qs@ zip file.
assetBundleImportSourceDescription_body :: Lens.Lens' AssetBundleImportSourceDescription (Prelude.Maybe Prelude.Text)
assetBundleImportSourceDescription_body = Lens.lens (\AssetBundleImportSourceDescription' {body} -> body) (\s@AssetBundleImportSourceDescription' {} a -> s {body = a} :: AssetBundleImportSourceDescription)

-- | The Amazon S3 URI that you provided at the start of the import job.
assetBundleImportSourceDescription_s3Uri :: Lens.Lens' AssetBundleImportSourceDescription (Prelude.Maybe Prelude.Text)
assetBundleImportSourceDescription_s3Uri = Lens.lens (\AssetBundleImportSourceDescription' {s3Uri} -> s3Uri) (\s@AssetBundleImportSourceDescription' {} a -> s {s3Uri = a} :: AssetBundleImportSourceDescription)

instance
  Data.FromJSON
    AssetBundleImportSourceDescription
  where
  parseJSON =
    Data.withObject
      "AssetBundleImportSourceDescription"
      ( \x ->
          AssetBundleImportSourceDescription'
            Prelude.<$> (x Data..:? "Body")
            Prelude.<*> (x Data..:? "S3Uri")
      )

instance
  Prelude.Hashable
    AssetBundleImportSourceDescription
  where
  hashWithSalt
    _salt
    AssetBundleImportSourceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` body
        `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    AssetBundleImportSourceDescription
  where
  rnf AssetBundleImportSourceDescription' {..} =
    Prelude.rnf body `Prelude.seq` Prelude.rnf s3Uri
