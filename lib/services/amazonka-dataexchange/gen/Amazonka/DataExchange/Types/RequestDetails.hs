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
-- Module      : Amazonka.DataExchange.Types.RequestDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
import qualified Amazonka.Prelude as Prelude

-- | The details for the request.
--
-- /See:/ 'newRequestDetails' smart constructor.
data RequestDetails = RequestDetails'
  { -- | Details about the import from Amazon S3 request.
    importAssetFromSignedUrl :: Prelude.Maybe ImportAssetFromSignedUrlRequestDetails,
    -- | Details from an import from Amazon Redshift datashare request.
    importAssetsFromRedshiftDataShares :: Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails,
    -- | Details about the export to signed URL request.
    exportAssetToSignedUrl :: Prelude.Maybe ExportAssetToSignedUrlRequestDetails,
    -- | Details about the export to Amazon S3 request.
    exportRevisionsToS3 :: Prelude.Maybe ExportRevisionsToS3RequestDetails,
    -- | Details about the export to Amazon S3 request.
    exportAssetsToS3 :: Prelude.Maybe ExportAssetsToS3RequestDetails,
    -- | Information about the import asset from API Gateway API request.
    importAssetsFromS3 :: Prelude.Maybe ImportAssetsFromS3RequestDetails,
    -- | Details about the import from signed URL request.
    importAssetFromApiGatewayApi :: Prelude.Maybe ImportAssetFromApiGatewayApiRequestDetails
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
-- 'importAssetFromSignedUrl', 'requestDetails_importAssetFromSignedUrl' - Details about the import from Amazon S3 request.
--
-- 'importAssetsFromRedshiftDataShares', 'requestDetails_importAssetsFromRedshiftDataShares' - Details from an import from Amazon Redshift datashare request.
--
-- 'exportAssetToSignedUrl', 'requestDetails_exportAssetToSignedUrl' - Details about the export to signed URL request.
--
-- 'exportRevisionsToS3', 'requestDetails_exportRevisionsToS3' - Details about the export to Amazon S3 request.
--
-- 'exportAssetsToS3', 'requestDetails_exportAssetsToS3' - Details about the export to Amazon S3 request.
--
-- 'importAssetsFromS3', 'requestDetails_importAssetsFromS3' - Information about the import asset from API Gateway API request.
--
-- 'importAssetFromApiGatewayApi', 'requestDetails_importAssetFromApiGatewayApi' - Details about the import from signed URL request.
newRequestDetails ::
  RequestDetails
newRequestDetails =
  RequestDetails'
    { importAssetFromSignedUrl =
        Prelude.Nothing,
      importAssetsFromRedshiftDataShares = Prelude.Nothing,
      exportAssetToSignedUrl = Prelude.Nothing,
      exportRevisionsToS3 = Prelude.Nothing,
      exportAssetsToS3 = Prelude.Nothing,
      importAssetsFromS3 = Prelude.Nothing,
      importAssetFromApiGatewayApi = Prelude.Nothing
    }

-- | Details about the import from Amazon S3 request.
requestDetails_importAssetFromSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetFromSignedUrlRequestDetails)
requestDetails_importAssetFromSignedUrl = Lens.lens (\RequestDetails' {importAssetFromSignedUrl} -> importAssetFromSignedUrl) (\s@RequestDetails' {} a -> s {importAssetFromSignedUrl = a} :: RequestDetails)

-- | Details from an import from Amazon Redshift datashare request.
requestDetails_importAssetsFromRedshiftDataShares :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails)
requestDetails_importAssetsFromRedshiftDataShares = Lens.lens (\RequestDetails' {importAssetsFromRedshiftDataShares} -> importAssetsFromRedshiftDataShares) (\s@RequestDetails' {} a -> s {importAssetsFromRedshiftDataShares = a} :: RequestDetails)

-- | Details about the export to signed URL request.
requestDetails_exportAssetToSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetToSignedUrlRequestDetails)
requestDetails_exportAssetToSignedUrl = Lens.lens (\RequestDetails' {exportAssetToSignedUrl} -> exportAssetToSignedUrl) (\s@RequestDetails' {} a -> s {exportAssetToSignedUrl = a} :: RequestDetails)

-- | Details about the export to Amazon S3 request.
requestDetails_exportRevisionsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportRevisionsToS3RequestDetails)
requestDetails_exportRevisionsToS3 = Lens.lens (\RequestDetails' {exportRevisionsToS3} -> exportRevisionsToS3) (\s@RequestDetails' {} a -> s {exportRevisionsToS3 = a} :: RequestDetails)

-- | Details about the export to Amazon S3 request.
requestDetails_exportAssetsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetsToS3RequestDetails)
requestDetails_exportAssetsToS3 = Lens.lens (\RequestDetails' {exportAssetsToS3} -> exportAssetsToS3) (\s@RequestDetails' {} a -> s {exportAssetsToS3 = a} :: RequestDetails)

-- | Information about the import asset from API Gateway API request.
requestDetails_importAssetsFromS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromS3RequestDetails)
requestDetails_importAssetsFromS3 = Lens.lens (\RequestDetails' {importAssetsFromS3} -> importAssetsFromS3) (\s@RequestDetails' {} a -> s {importAssetsFromS3 = a} :: RequestDetails)

-- | Details about the import from signed URL request.
requestDetails_importAssetFromApiGatewayApi :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetFromApiGatewayApiRequestDetails)
requestDetails_importAssetFromApiGatewayApi = Lens.lens (\RequestDetails' {importAssetFromApiGatewayApi} -> importAssetFromApiGatewayApi) (\s@RequestDetails' {} a -> s {importAssetFromApiGatewayApi = a} :: RequestDetails)

instance Prelude.Hashable RequestDetails where
  hashWithSalt _salt RequestDetails' {..} =
    _salt
      `Prelude.hashWithSalt` importAssetFromSignedUrl
      `Prelude.hashWithSalt` importAssetsFromRedshiftDataShares
      `Prelude.hashWithSalt` exportAssetToSignedUrl
      `Prelude.hashWithSalt` exportRevisionsToS3
      `Prelude.hashWithSalt` exportAssetsToS3
      `Prelude.hashWithSalt` importAssetsFromS3
      `Prelude.hashWithSalt` importAssetFromApiGatewayApi

instance Prelude.NFData RequestDetails where
  rnf RequestDetails' {..} =
    Prelude.rnf importAssetFromSignedUrl
      `Prelude.seq` Prelude.rnf importAssetsFromRedshiftDataShares
      `Prelude.seq` Prelude.rnf exportAssetToSignedUrl
      `Prelude.seq` Prelude.rnf exportRevisionsToS3
      `Prelude.seq` Prelude.rnf exportAssetsToS3
      `Prelude.seq` Prelude.rnf importAssetsFromS3
      `Prelude.seq` Prelude.rnf importAssetFromApiGatewayApi

instance Data.ToJSON RequestDetails where
  toJSON RequestDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImportAssetFromSignedUrl" Data..=)
              Prelude.<$> importAssetFromSignedUrl,
            ("ImportAssetsFromRedshiftDataShares" Data..=)
              Prelude.<$> importAssetsFromRedshiftDataShares,
            ("ExportAssetToSignedUrl" Data..=)
              Prelude.<$> exportAssetToSignedUrl,
            ("ExportRevisionsToS3" Data..=)
              Prelude.<$> exportRevisionsToS3,
            ("ExportAssetsToS3" Data..=)
              Prelude.<$> exportAssetsToS3,
            ("ImportAssetsFromS3" Data..=)
              Prelude.<$> importAssetsFromS3,
            ("ImportAssetFromApiGatewayApi" Data..=)
              Prelude.<$> importAssetFromApiGatewayApi
          ]
      )
