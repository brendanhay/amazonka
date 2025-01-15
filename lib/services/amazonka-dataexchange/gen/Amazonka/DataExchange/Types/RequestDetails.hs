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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.RequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketRequestDetails
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiRequestDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
import qualified Amazonka.Prelude as Prelude

-- | The details for the request.
--
-- /See:/ 'newRequestDetails' smart constructor.
data RequestDetails = RequestDetails'
  { -- | Details of the request to create S3 data access from the Amazon S3
    -- bucket.
    createS3DataAccessFromS3Bucket :: Prelude.Maybe CreateS3DataAccessFromS3BucketRequestDetails,
    -- | Details about the export to signed URL request.
    exportAssetToSignedUrl :: Prelude.Maybe ExportAssetToSignedUrlRequestDetails,
    -- | Details about the export to Amazon S3 request.
    exportAssetsToS3 :: Prelude.Maybe ExportAssetsToS3RequestDetails,
    -- | Details about the export to Amazon S3 request.
    exportRevisionsToS3 :: Prelude.Maybe ExportRevisionsToS3RequestDetails,
    -- | Details about the import from signed URL request.
    importAssetFromApiGatewayApi :: Prelude.Maybe ImportAssetFromApiGatewayApiRequestDetails,
    -- | Details about the import from Amazon S3 request.
    importAssetFromSignedUrl :: Prelude.Maybe ImportAssetFromSignedUrlRequestDetails,
    -- | Request details for the ImportAssetsFromLakeFormationTagPolicy job.
    importAssetsFromLakeFormationTagPolicy :: Prelude.Maybe ImportAssetsFromLakeFormationTagPolicyRequestDetails,
    -- | Details from an import from Amazon Redshift datashare request.
    importAssetsFromRedshiftDataShares :: Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails,
    -- | Details about the import asset from API Gateway API request.
    importAssetsFromS3 :: Prelude.Maybe ImportAssetsFromS3RequestDetails
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
-- 'createS3DataAccessFromS3Bucket', 'requestDetails_createS3DataAccessFromS3Bucket' - Details of the request to create S3 data access from the Amazon S3
-- bucket.
--
-- 'exportAssetToSignedUrl', 'requestDetails_exportAssetToSignedUrl' - Details about the export to signed URL request.
--
-- 'exportAssetsToS3', 'requestDetails_exportAssetsToS3' - Details about the export to Amazon S3 request.
--
-- 'exportRevisionsToS3', 'requestDetails_exportRevisionsToS3' - Details about the export to Amazon S3 request.
--
-- 'importAssetFromApiGatewayApi', 'requestDetails_importAssetFromApiGatewayApi' - Details about the import from signed URL request.
--
-- 'importAssetFromSignedUrl', 'requestDetails_importAssetFromSignedUrl' - Details about the import from Amazon S3 request.
--
-- 'importAssetsFromLakeFormationTagPolicy', 'requestDetails_importAssetsFromLakeFormationTagPolicy' - Request details for the ImportAssetsFromLakeFormationTagPolicy job.
--
-- 'importAssetsFromRedshiftDataShares', 'requestDetails_importAssetsFromRedshiftDataShares' - Details from an import from Amazon Redshift datashare request.
--
-- 'importAssetsFromS3', 'requestDetails_importAssetsFromS3' - Details about the import asset from API Gateway API request.
newRequestDetails ::
  RequestDetails
newRequestDetails =
  RequestDetails'
    { createS3DataAccessFromS3Bucket =
        Prelude.Nothing,
      exportAssetToSignedUrl = Prelude.Nothing,
      exportAssetsToS3 = Prelude.Nothing,
      exportRevisionsToS3 = Prelude.Nothing,
      importAssetFromApiGatewayApi = Prelude.Nothing,
      importAssetFromSignedUrl = Prelude.Nothing,
      importAssetsFromLakeFormationTagPolicy =
        Prelude.Nothing,
      importAssetsFromRedshiftDataShares = Prelude.Nothing,
      importAssetsFromS3 = Prelude.Nothing
    }

-- | Details of the request to create S3 data access from the Amazon S3
-- bucket.
requestDetails_createS3DataAccessFromS3Bucket :: Lens.Lens' RequestDetails (Prelude.Maybe CreateS3DataAccessFromS3BucketRequestDetails)
requestDetails_createS3DataAccessFromS3Bucket = Lens.lens (\RequestDetails' {createS3DataAccessFromS3Bucket} -> createS3DataAccessFromS3Bucket) (\s@RequestDetails' {} a -> s {createS3DataAccessFromS3Bucket = a} :: RequestDetails)

-- | Details about the export to signed URL request.
requestDetails_exportAssetToSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetToSignedUrlRequestDetails)
requestDetails_exportAssetToSignedUrl = Lens.lens (\RequestDetails' {exportAssetToSignedUrl} -> exportAssetToSignedUrl) (\s@RequestDetails' {} a -> s {exportAssetToSignedUrl = a} :: RequestDetails)

-- | Details about the export to Amazon S3 request.
requestDetails_exportAssetsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportAssetsToS3RequestDetails)
requestDetails_exportAssetsToS3 = Lens.lens (\RequestDetails' {exportAssetsToS3} -> exportAssetsToS3) (\s@RequestDetails' {} a -> s {exportAssetsToS3 = a} :: RequestDetails)

-- | Details about the export to Amazon S3 request.
requestDetails_exportRevisionsToS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ExportRevisionsToS3RequestDetails)
requestDetails_exportRevisionsToS3 = Lens.lens (\RequestDetails' {exportRevisionsToS3} -> exportRevisionsToS3) (\s@RequestDetails' {} a -> s {exportRevisionsToS3 = a} :: RequestDetails)

-- | Details about the import from signed URL request.
requestDetails_importAssetFromApiGatewayApi :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetFromApiGatewayApiRequestDetails)
requestDetails_importAssetFromApiGatewayApi = Lens.lens (\RequestDetails' {importAssetFromApiGatewayApi} -> importAssetFromApiGatewayApi) (\s@RequestDetails' {} a -> s {importAssetFromApiGatewayApi = a} :: RequestDetails)

-- | Details about the import from Amazon S3 request.
requestDetails_importAssetFromSignedUrl :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetFromSignedUrlRequestDetails)
requestDetails_importAssetFromSignedUrl = Lens.lens (\RequestDetails' {importAssetFromSignedUrl} -> importAssetFromSignedUrl) (\s@RequestDetails' {} a -> s {importAssetFromSignedUrl = a} :: RequestDetails)

-- | Request details for the ImportAssetsFromLakeFormationTagPolicy job.
requestDetails_importAssetsFromLakeFormationTagPolicy :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromLakeFormationTagPolicyRequestDetails)
requestDetails_importAssetsFromLakeFormationTagPolicy = Lens.lens (\RequestDetails' {importAssetsFromLakeFormationTagPolicy} -> importAssetsFromLakeFormationTagPolicy) (\s@RequestDetails' {} a -> s {importAssetsFromLakeFormationTagPolicy = a} :: RequestDetails)

-- | Details from an import from Amazon Redshift datashare request.
requestDetails_importAssetsFromRedshiftDataShares :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromRedshiftDataSharesRequestDetails)
requestDetails_importAssetsFromRedshiftDataShares = Lens.lens (\RequestDetails' {importAssetsFromRedshiftDataShares} -> importAssetsFromRedshiftDataShares) (\s@RequestDetails' {} a -> s {importAssetsFromRedshiftDataShares = a} :: RequestDetails)

-- | Details about the import asset from API Gateway API request.
requestDetails_importAssetsFromS3 :: Lens.Lens' RequestDetails (Prelude.Maybe ImportAssetsFromS3RequestDetails)
requestDetails_importAssetsFromS3 = Lens.lens (\RequestDetails' {importAssetsFromS3} -> importAssetsFromS3) (\s@RequestDetails' {} a -> s {importAssetsFromS3 = a} :: RequestDetails)

instance Prelude.Hashable RequestDetails where
  hashWithSalt _salt RequestDetails' {..} =
    _salt
      `Prelude.hashWithSalt` createS3DataAccessFromS3Bucket
      `Prelude.hashWithSalt` exportAssetToSignedUrl
      `Prelude.hashWithSalt` exportAssetsToS3
      `Prelude.hashWithSalt` exportRevisionsToS3
      `Prelude.hashWithSalt` importAssetFromApiGatewayApi
      `Prelude.hashWithSalt` importAssetFromSignedUrl
      `Prelude.hashWithSalt` importAssetsFromLakeFormationTagPolicy
      `Prelude.hashWithSalt` importAssetsFromRedshiftDataShares
      `Prelude.hashWithSalt` importAssetsFromS3

instance Prelude.NFData RequestDetails where
  rnf RequestDetails' {..} =
    Prelude.rnf createS3DataAccessFromS3Bucket `Prelude.seq`
      Prelude.rnf exportAssetToSignedUrl `Prelude.seq`
        Prelude.rnf exportAssetsToS3 `Prelude.seq`
          Prelude.rnf exportRevisionsToS3 `Prelude.seq`
            Prelude.rnf importAssetFromApiGatewayApi `Prelude.seq`
              Prelude.rnf importAssetFromSignedUrl `Prelude.seq`
                Prelude.rnf importAssetsFromLakeFormationTagPolicy `Prelude.seq`
                  Prelude.rnf importAssetsFromRedshiftDataShares `Prelude.seq`
                    Prelude.rnf importAssetsFromS3

instance Data.ToJSON RequestDetails where
  toJSON RequestDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreateS3DataAccessFromS3Bucket" Data..=)
              Prelude.<$> createS3DataAccessFromS3Bucket,
            ("ExportAssetToSignedUrl" Data..=)
              Prelude.<$> exportAssetToSignedUrl,
            ("ExportAssetsToS3" Data..=)
              Prelude.<$> exportAssetsToS3,
            ("ExportRevisionsToS3" Data..=)
              Prelude.<$> exportRevisionsToS3,
            ("ImportAssetFromApiGatewayApi" Data..=)
              Prelude.<$> importAssetFromApiGatewayApi,
            ("ImportAssetFromSignedUrl" Data..=)
              Prelude.<$> importAssetFromSignedUrl,
            ("ImportAssetsFromLakeFormationTagPolicy" Data..=)
              Prelude.<$> importAssetsFromLakeFormationTagPolicy,
            ("ImportAssetsFromRedshiftDataShares" Data..=)
              Prelude.<$> importAssetsFromRedshiftDataShares,
            ("ImportAssetsFromS3" Data..=)
              Prelude.<$> importAssetsFromS3
          ]
      )
