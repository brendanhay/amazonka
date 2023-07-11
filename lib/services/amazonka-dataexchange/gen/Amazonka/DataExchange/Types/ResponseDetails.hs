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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketResponseDetails
import Amazonka.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails
import Amazonka.DataExchange.Types.ExportRevisionsToS3ResponseDetails
import Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiResponseDetails
import Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromLakeFormationTagPolicyResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
import Amazonka.DataExchange.Types.ImportAssetsFromS3ResponseDetails
import qualified Amazonka.Prelude as Prelude

-- | Details for the response.
--
-- /See:/ 'newResponseDetails' smart constructor.
data ResponseDetails = ResponseDetails'
  { -- | Response details from the CreateS3DataAccessFromS3Bucket job.
    createS3DataAccessFromS3Bucket :: Prelude.Maybe CreateS3DataAccessFromS3BucketResponseDetails,
    -- | Details for the export to signed URL response.
    exportAssetToSignedUrl :: Prelude.Maybe ExportAssetToSignedUrlResponseDetails,
    -- | Details for the export to Amazon S3 response.
    exportAssetsToS3 :: Prelude.Maybe ExportAssetsToS3ResponseDetails,
    -- | Details for the export revisions to Amazon S3 response.
    exportRevisionsToS3 :: Prelude.Maybe ExportRevisionsToS3ResponseDetails,
    -- | The response details.
    importAssetFromApiGatewayApi :: Prelude.Maybe ImportAssetFromApiGatewayApiResponseDetails,
    -- | Details for the import from signed URL response.
    importAssetFromSignedUrl :: Prelude.Maybe ImportAssetFromSignedUrlResponseDetails,
    -- | Response details from the ImportAssetsFromLakeFormationTagPolicy job.
    importAssetsFromLakeFormationTagPolicy :: Prelude.Maybe ImportAssetsFromLakeFormationTagPolicyResponseDetails,
    -- | Details from an import from Amazon Redshift datashare response.
    importAssetsFromRedshiftDataShares :: Prelude.Maybe ImportAssetsFromRedshiftDataSharesResponseDetails,
    -- | Details for the import from Amazon S3 response.
    importAssetsFromS3 :: Prelude.Maybe ImportAssetsFromS3ResponseDetails
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
-- 'createS3DataAccessFromS3Bucket', 'responseDetails_createS3DataAccessFromS3Bucket' - Response details from the CreateS3DataAccessFromS3Bucket job.
--
-- 'exportAssetToSignedUrl', 'responseDetails_exportAssetToSignedUrl' - Details for the export to signed URL response.
--
-- 'exportAssetsToS3', 'responseDetails_exportAssetsToS3' - Details for the export to Amazon S3 response.
--
-- 'exportRevisionsToS3', 'responseDetails_exportRevisionsToS3' - Details for the export revisions to Amazon S3 response.
--
-- 'importAssetFromApiGatewayApi', 'responseDetails_importAssetFromApiGatewayApi' - The response details.
--
-- 'importAssetFromSignedUrl', 'responseDetails_importAssetFromSignedUrl' - Details for the import from signed URL response.
--
-- 'importAssetsFromLakeFormationTagPolicy', 'responseDetails_importAssetsFromLakeFormationTagPolicy' - Response details from the ImportAssetsFromLakeFormationTagPolicy job.
--
-- 'importAssetsFromRedshiftDataShares', 'responseDetails_importAssetsFromRedshiftDataShares' - Details from an import from Amazon Redshift datashare response.
--
-- 'importAssetsFromS3', 'responseDetails_importAssetsFromS3' - Details for the import from Amazon S3 response.
newResponseDetails ::
  ResponseDetails
newResponseDetails =
  ResponseDetails'
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

-- | Response details from the CreateS3DataAccessFromS3Bucket job.
responseDetails_createS3DataAccessFromS3Bucket :: Lens.Lens' ResponseDetails (Prelude.Maybe CreateS3DataAccessFromS3BucketResponseDetails)
responseDetails_createS3DataAccessFromS3Bucket = Lens.lens (\ResponseDetails' {createS3DataAccessFromS3Bucket} -> createS3DataAccessFromS3Bucket) (\s@ResponseDetails' {} a -> s {createS3DataAccessFromS3Bucket = a} :: ResponseDetails)

-- | Details for the export to signed URL response.
responseDetails_exportAssetToSignedUrl :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportAssetToSignedUrlResponseDetails)
responseDetails_exportAssetToSignedUrl = Lens.lens (\ResponseDetails' {exportAssetToSignedUrl} -> exportAssetToSignedUrl) (\s@ResponseDetails' {} a -> s {exportAssetToSignedUrl = a} :: ResponseDetails)

-- | Details for the export to Amazon S3 response.
responseDetails_exportAssetsToS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportAssetsToS3ResponseDetails)
responseDetails_exportAssetsToS3 = Lens.lens (\ResponseDetails' {exportAssetsToS3} -> exportAssetsToS3) (\s@ResponseDetails' {} a -> s {exportAssetsToS3 = a} :: ResponseDetails)

-- | Details for the export revisions to Amazon S3 response.
responseDetails_exportRevisionsToS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ExportRevisionsToS3ResponseDetails)
responseDetails_exportRevisionsToS3 = Lens.lens (\ResponseDetails' {exportRevisionsToS3} -> exportRevisionsToS3) (\s@ResponseDetails' {} a -> s {exportRevisionsToS3 = a} :: ResponseDetails)

-- | The response details.
responseDetails_importAssetFromApiGatewayApi :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetFromApiGatewayApiResponseDetails)
responseDetails_importAssetFromApiGatewayApi = Lens.lens (\ResponseDetails' {importAssetFromApiGatewayApi} -> importAssetFromApiGatewayApi) (\s@ResponseDetails' {} a -> s {importAssetFromApiGatewayApi = a} :: ResponseDetails)

-- | Details for the import from signed URL response.
responseDetails_importAssetFromSignedUrl :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetFromSignedUrlResponseDetails)
responseDetails_importAssetFromSignedUrl = Lens.lens (\ResponseDetails' {importAssetFromSignedUrl} -> importAssetFromSignedUrl) (\s@ResponseDetails' {} a -> s {importAssetFromSignedUrl = a} :: ResponseDetails)

-- | Response details from the ImportAssetsFromLakeFormationTagPolicy job.
responseDetails_importAssetsFromLakeFormationTagPolicy :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetsFromLakeFormationTagPolicyResponseDetails)
responseDetails_importAssetsFromLakeFormationTagPolicy = Lens.lens (\ResponseDetails' {importAssetsFromLakeFormationTagPolicy} -> importAssetsFromLakeFormationTagPolicy) (\s@ResponseDetails' {} a -> s {importAssetsFromLakeFormationTagPolicy = a} :: ResponseDetails)

-- | Details from an import from Amazon Redshift datashare response.
responseDetails_importAssetsFromRedshiftDataShares :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetsFromRedshiftDataSharesResponseDetails)
responseDetails_importAssetsFromRedshiftDataShares = Lens.lens (\ResponseDetails' {importAssetsFromRedshiftDataShares} -> importAssetsFromRedshiftDataShares) (\s@ResponseDetails' {} a -> s {importAssetsFromRedshiftDataShares = a} :: ResponseDetails)

-- | Details for the import from Amazon S3 response.
responseDetails_importAssetsFromS3 :: Lens.Lens' ResponseDetails (Prelude.Maybe ImportAssetsFromS3ResponseDetails)
responseDetails_importAssetsFromS3 = Lens.lens (\ResponseDetails' {importAssetsFromS3} -> importAssetsFromS3) (\s@ResponseDetails' {} a -> s {importAssetsFromS3 = a} :: ResponseDetails)

instance Data.FromJSON ResponseDetails where
  parseJSON =
    Data.withObject
      "ResponseDetails"
      ( \x ->
          ResponseDetails'
            Prelude.<$> (x Data..:? "CreateS3DataAccessFromS3Bucket")
            Prelude.<*> (x Data..:? "ExportAssetToSignedUrl")
            Prelude.<*> (x Data..:? "ExportAssetsToS3")
            Prelude.<*> (x Data..:? "ExportRevisionsToS3")
            Prelude.<*> (x Data..:? "ImportAssetFromApiGatewayApi")
            Prelude.<*> (x Data..:? "ImportAssetFromSignedUrl")
            Prelude.<*> (x Data..:? "ImportAssetsFromLakeFormationTagPolicy")
            Prelude.<*> (x Data..:? "ImportAssetsFromRedshiftDataShares")
            Prelude.<*> (x Data..:? "ImportAssetsFromS3")
      )

instance Prelude.Hashable ResponseDetails where
  hashWithSalt _salt ResponseDetails' {..} =
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

instance Prelude.NFData ResponseDetails where
  rnf ResponseDetails' {..} =
    Prelude.rnf createS3DataAccessFromS3Bucket
      `Prelude.seq` Prelude.rnf exportAssetToSignedUrl
      `Prelude.seq` Prelude.rnf exportAssetsToS3
      `Prelude.seq` Prelude.rnf exportRevisionsToS3
      `Prelude.seq` Prelude.rnf importAssetFromApiGatewayApi
      `Prelude.seq` Prelude.rnf importAssetFromSignedUrl
      `Prelude.seq` Prelude.rnf importAssetsFromLakeFormationTagPolicy
      `Prelude.seq` Prelude.rnf importAssetsFromRedshiftDataShares
      `Prelude.seq` Prelude.rnf importAssetsFromS3
