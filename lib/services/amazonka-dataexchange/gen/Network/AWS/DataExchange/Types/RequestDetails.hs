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
-- Module      : Network.AWS.DataExchange.Types.RequestDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.RequestDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Network.AWS.DataExchange.Types.ExportAssetsToS3RequestDetails
import Network.AWS.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Network.AWS.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Network.AWS.DataExchange.Types.ImportAssetsFromS3RequestDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details for the request.
--
-- /See:/ 'newRequestDetails' smart constructor.
data RequestDetails = RequestDetails'
  { -- | Details about the export to Amazon S3 request.
    exportAssetsToS3 :: Prelude.Maybe ExportAssetsToS3RequestDetails,
    -- | Details about the export to Amazon S3 request.
    exportRevisionsToS3 :: Prelude.Maybe ExportRevisionsToS3RequestDetails,
    -- | Details about the import from signed URL request.
    importAssetFromSignedUrl :: Prelude.Maybe ImportAssetFromSignedUrlRequestDetails,
    -- | Details from an import from Amazon Redshift datashare request.
    importAssetsFromRedshiftDataShares :: Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails,
    -- | Details about the import from Amazon S3 request.
    importAssetsFromS3 :: Prelude.Maybe ImportAssetsFromS3RequestDetails,
    -- | Details about the export to signed URL request.
    exportAssetToSignedUrl :: Prelude.Maybe ExportAssetToSignedUrlRequestDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportAssetsToS3', 'requestDetails_exportAssetsToS3' - Details about the export to Amazon S3 request.
--
-- 'exportRevisionsToS3', 'requestDetails_exportRevisionsToS3' - Details about the export to Amazon S3 request.
--
-- 'importAssetFromSignedUrl', 'requestDetails_importAssetFromSignedUrl' - Details about the import from signed URL request.
--
-- 'importAssetsFromRedshiftDataShares', 'requestDetails_importAssetsFromRedshiftDataShares' - Details from an import from Amazon Redshift datashare request.
--
-- 'importAssetsFromS3', 'requestDetails_importAssetsFromS3' - Details about the import from Amazon S3 request.
--
-- 'exportAssetToSignedUrl', 'requestDetails_exportAssetToSignedUrl' - Details about the export to signed URL request.
newRequestDetails ::
  RequestDetails
newRequestDetails =
  RequestDetails'
    { exportAssetsToS3 = Prelude.Nothing,
      exportRevisionsToS3 = Prelude.Nothing,
      importAssetFromSignedUrl = Prelude.Nothing,
      importAssetsFromRedshiftDataShares = Prelude.Nothing,
      importAssetsFromS3 = Prelude.Nothing,
      exportAssetToSignedUrl = Prelude.Nothing
    }

-- | Details about the export to Amazon S3 request.
requestDetails_exportAssetsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetsToS3RequestDetails)
requestDetails_exportAssetsToS3 = Lens.lens (\RequestDetails' {exportAssetsToS3} -> exportAssetsToS3) (\s@RequestDetails' {} a -> s {exportAssetsToS3 = a} :: RequestDetails)

-- | Details about the export to Amazon S3 request.
requestDetails_exportRevisionsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportRevisionsToS3RequestDetails)
requestDetails_exportRevisionsToS3 = Lens.lens (\RequestDetails' {exportRevisionsToS3} -> exportRevisionsToS3) (\s@RequestDetails' {} a -> s {exportRevisionsToS3 = a} :: RequestDetails)

-- | Details about the import from signed URL request.
requestDetails_importAssetFromSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetFromSignedUrlRequestDetails)
requestDetails_importAssetFromSignedUrl = Lens.lens (\RequestDetails' {importAssetFromSignedUrl} -> importAssetFromSignedUrl) (\s@RequestDetails' {} a -> s {importAssetFromSignedUrl = a} :: RequestDetails)

-- | Details from an import from Amazon Redshift datashare request.
requestDetails_importAssetsFromRedshiftDataShares :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails)
requestDetails_importAssetsFromRedshiftDataShares = Lens.lens (\RequestDetails' {importAssetsFromRedshiftDataShares} -> importAssetsFromRedshiftDataShares) (\s@RequestDetails' {} a -> s {importAssetsFromRedshiftDataShares = a} :: RequestDetails)

-- | Details about the import from Amazon S3 request.
requestDetails_importAssetsFromS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromS3RequestDetails)
requestDetails_importAssetsFromS3 = Lens.lens (\RequestDetails' {importAssetsFromS3} -> importAssetsFromS3) (\s@RequestDetails' {} a -> s {importAssetsFromS3 = a} :: RequestDetails)

-- | Details about the export to signed URL request.
requestDetails_exportAssetToSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetToSignedUrlRequestDetails)
requestDetails_exportAssetToSignedUrl = Lens.lens (\RequestDetails' {exportAssetToSignedUrl} -> exportAssetToSignedUrl) (\s@RequestDetails' {} a -> s {exportAssetToSignedUrl = a} :: RequestDetails)

instance Prelude.Hashable RequestDetails

instance Prelude.NFData RequestDetails

instance Core.ToJSON RequestDetails where
  toJSON RequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExportAssetsToS3" Core..=)
              Prelude.<$> exportAssetsToS3,
            ("ExportRevisionsToS3" Core..=)
              Prelude.<$> exportRevisionsToS3,
            ("ImportAssetFromSignedUrl" Core..=)
              Prelude.<$> importAssetFromSignedUrl,
            ("ImportAssetsFromRedshiftDataShares" Core..=)
              Prelude.<$> importAssetsFromRedshiftDataShares,
            ("ImportAssetsFromS3" Core..=)
              Prelude.<$> importAssetsFromS3,
            ("ExportAssetToSignedUrl" Core..=)
              Prelude.<$> exportAssetToSignedUrl
          ]
      )
