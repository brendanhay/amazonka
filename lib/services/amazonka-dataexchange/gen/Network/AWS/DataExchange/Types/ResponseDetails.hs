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
-- Module      : Amazonka.DataExchange.Types.ResponseDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ResponseDetails where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3ResponseDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details for the response.
--
-- /See:/ 'newResponseDetails' smart constructor.
data ResponseDetails = ResponseDetails'
  { -- | Details for the export to Amazon S3 response.
    exportAssetsToS3 :: Prelude.Maybe ExportAssetsToS3ResponseDetails,
    -- | Details for the export revisions to Amazon S3 response.
    exportRevisionsToS3 :: Prelude.Maybe ExportRevisionsToS3ResponseDetails,
    -- | Details for the import from signed URL response.
    importAssetFromSignedUrl :: Prelude.Maybe ImportAssetFromSignedUrlResponseDetails,
    -- | Details from an import from Amazon Redshift datashare response.
    importAssetsFromRedshiftDataShares :: Prelude.Maybe ImportAssetsFromRedshiftDataSharesResponseDetails,
    -- | Details for the import from Amazon S3 response.
    importAssetsFromS3 :: Prelude.Maybe ImportAssetsFromS3ResponseDetails,
    -- | Details for the export to signed URL response.
    exportAssetToSignedUrl :: Prelude.Maybe ExportAssetToSignedUrlResponseDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportAssetsToS3', 'responseDetails_exportAssetsToS3' - Details for the export to Amazon S3 response.
--
-- 'exportRevisionsToS3', 'responseDetails_exportRevisionsToS3' - Details for the export revisions to Amazon S3 response.
--
-- 'importAssetFromSignedUrl', 'responseDetails_importAssetFromSignedUrl' - Details for the import from signed URL response.
--
-- 'importAssetsFromRedshiftDataShares', 'responseDetails_importAssetsFromRedshiftDataShares' - Details from an import from Amazon Redshift datashare response.
--
-- 'importAssetsFromS3', 'responseDetails_importAssetsFromS3' - Details for the import from Amazon S3 response.
--
-- 'exportAssetToSignedUrl', 'responseDetails_exportAssetToSignedUrl' - Details for the export to signed URL response.
newResponseDetails ::
  ResponseDetails
newResponseDetails =
  ResponseDetails'
    { exportAssetsToS3 =
        Prelude.Nothing,
      exportRevisionsToS3 = Prelude.Nothing,
      importAssetFromSignedUrl = Prelude.Nothing,
      importAssetsFromRedshiftDataShares = Prelude.Nothing,
      importAssetsFromS3 = Prelude.Nothing,
      exportAssetToSignedUrl = Prelude.Nothing
    }

-- | Details for the export to Amazon S3 response.
responseDetails_exportAssetsToS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportAssetsToS3ResponseDetails)
responseDetails_exportAssetsToS3 = Lens.lens (\ResponseDetails' {exportAssetsToS3} -> exportAssetsToS3) (\s@ResponseDetails' {} a -> s {exportAssetsToS3 = a} :: ResponseDetails)

-- | Details for the export revisions to Amazon S3 response.
responseDetails_exportRevisionsToS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportRevisionsToS3ResponseDetails)
responseDetails_exportRevisionsToS3 = Lens.lens (\ResponseDetails' {exportRevisionsToS3} -> exportRevisionsToS3) (\s@ResponseDetails' {} a -> s {exportRevisionsToS3 = a} :: ResponseDetails)

-- | Details for the import from signed URL response.
responseDetails_importAssetFromSignedUrl :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetFromSignedUrlResponseDetails)
responseDetails_importAssetFromSignedUrl = Lens.lens (\ResponseDetails' {importAssetFromSignedUrl} -> importAssetFromSignedUrl) (\s@ResponseDetails' {} a -> s {importAssetFromSignedUrl = a} :: ResponseDetails)

-- | Details from an import from Amazon Redshift datashare response.
responseDetails_importAssetsFromRedshiftDataShares :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetsFromRedshiftDataSharesResponseDetails)
responseDetails_importAssetsFromRedshiftDataShares = Lens.lens (\ResponseDetails' {importAssetsFromRedshiftDataShares} -> importAssetsFromRedshiftDataShares) (\s@ResponseDetails' {} a -> s {importAssetsFromRedshiftDataShares = a} :: ResponseDetails)

-- | Details for the import from Amazon S3 response.
responseDetails_importAssetsFromS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetsFromS3ResponseDetails)
responseDetails_importAssetsFromS3 = Lens.lens (\ResponseDetails' {importAssetsFromS3} -> importAssetsFromS3) (\s@ResponseDetails' {} a -> s {importAssetsFromS3 = a} :: ResponseDetails)

-- | Details for the export to signed URL response.
responseDetails_exportAssetToSignedUrl :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportAssetToSignedUrlResponseDetails)
responseDetails_exportAssetToSignedUrl = Lens.lens (\ResponseDetails' {exportAssetToSignedUrl} -> exportAssetToSignedUrl) (\s@ResponseDetails' {} a -> s {exportAssetToSignedUrl = a} :: ResponseDetails)

instance Core.FromJSON ResponseDetails where
  parseJSON =
    Core.withObject
      "ResponseDetails"
      ( \x ->
          ResponseDetails'
            Prelude.<$> (x Core..:? "ExportAssetsToS3")
            Prelude.<*> (x Core..:? "ExportRevisionsToS3")
            Prelude.<*> (x Core..:? "ImportAssetFromSignedUrl")
            Prelude.<*> (x Core..:? "ImportAssetsFromRedshiftDataShares")
            Prelude.<*> (x Core..:? "ImportAssetsFromS3")
            Prelude.<*> (x Core..:? "ExportAssetToSignedUrl")
      )

instance Prelude.Hashable ResponseDetails

instance Prelude.NFData ResponseDetails
