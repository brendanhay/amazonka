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
-- Module      : Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiResponseDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromApiGatewayApiResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.ProtocolType
import qualified Amazonka.Prelude as Prelude

-- | The response details.
--
-- /See:/ 'newImportAssetFromApiGatewayApiResponseDetails' smart constructor.
data ImportAssetFromApiGatewayApiResponseDetails = ImportAssetFromApiGatewayApiResponseDetails'
  { -- | The API description.
    apiDescription :: Prelude.Maybe Prelude.Text,
    -- | The API key.
    apiKey :: Prelude.Maybe Prelude.Text,
    -- | The API ID.
    apiId :: Prelude.Text,
    -- | The API name.
    apiName :: Prelude.Text,
    -- | The Base64-encoded Md5 hash for the API asset, used to ensure the
    -- integrity of the API at that location.
    apiSpecificationMd5Hash :: Prelude.Text,
    -- | The upload URL of the API specification.
    apiSpecificationUploadUrl :: Prelude.Text,
    -- | The date and time that the upload URL expires, in ISO 8601 format.
    apiSpecificationUploadUrlExpiresAt :: Data.ISO8601,
    -- | The data set ID.
    dataSetId :: Prelude.Text,
    -- | The protocol type.
    protocolType :: ProtocolType,
    -- | The revision ID.
    revisionId :: Prelude.Text,
    -- | The API stage.
    stage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetFromApiGatewayApiResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiDescription', 'importAssetFromApiGatewayApiResponseDetails_apiDescription' - The API description.
--
-- 'apiKey', 'importAssetFromApiGatewayApiResponseDetails_apiKey' - The API key.
--
-- 'apiId', 'importAssetFromApiGatewayApiResponseDetails_apiId' - The API ID.
--
-- 'apiName', 'importAssetFromApiGatewayApiResponseDetails_apiName' - The API name.
--
-- 'apiSpecificationMd5Hash', 'importAssetFromApiGatewayApiResponseDetails_apiSpecificationMd5Hash' - The Base64-encoded Md5 hash for the API asset, used to ensure the
-- integrity of the API at that location.
--
-- 'apiSpecificationUploadUrl', 'importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrl' - The upload URL of the API specification.
--
-- 'apiSpecificationUploadUrlExpiresAt', 'importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrlExpiresAt' - The date and time that the upload URL expires, in ISO 8601 format.
--
-- 'dataSetId', 'importAssetFromApiGatewayApiResponseDetails_dataSetId' - The data set ID.
--
-- 'protocolType', 'importAssetFromApiGatewayApiResponseDetails_protocolType' - The protocol type.
--
-- 'revisionId', 'importAssetFromApiGatewayApiResponseDetails_revisionId' - The revision ID.
--
-- 'stage', 'importAssetFromApiGatewayApiResponseDetails_stage' - The API stage.
newImportAssetFromApiGatewayApiResponseDetails ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'apiName'
  Prelude.Text ->
  -- | 'apiSpecificationMd5Hash'
  Prelude.Text ->
  -- | 'apiSpecificationUploadUrl'
  Prelude.Text ->
  -- | 'apiSpecificationUploadUrlExpiresAt'
  Prelude.UTCTime ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'protocolType'
  ProtocolType ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'stage'
  Prelude.Text ->
  ImportAssetFromApiGatewayApiResponseDetails
newImportAssetFromApiGatewayApiResponseDetails
  pApiId_
  pApiName_
  pApiSpecificationMd5Hash_
  pApiSpecificationUploadUrl_
  pApiSpecificationUploadUrlExpiresAt_
  pDataSetId_
  pProtocolType_
  pRevisionId_
  pStage_ =
    ImportAssetFromApiGatewayApiResponseDetails'
      { apiDescription =
          Prelude.Nothing,
        apiKey = Prelude.Nothing,
        apiId = pApiId_,
        apiName = pApiName_,
        apiSpecificationMd5Hash =
          pApiSpecificationMd5Hash_,
        apiSpecificationUploadUrl =
          pApiSpecificationUploadUrl_,
        apiSpecificationUploadUrlExpiresAt =
          Data._Time
            Lens.# pApiSpecificationUploadUrlExpiresAt_,
        dataSetId = pDataSetId_,
        protocolType = pProtocolType_,
        revisionId = pRevisionId_,
        stage = pStage_
      }

-- | The API description.
importAssetFromApiGatewayApiResponseDetails_apiDescription :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromApiGatewayApiResponseDetails_apiDescription = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiDescription} -> apiDescription) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiDescription = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The API key.
importAssetFromApiGatewayApiResponseDetails_apiKey :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromApiGatewayApiResponseDetails_apiKey = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiKey} -> apiKey) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiKey = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The API ID.
importAssetFromApiGatewayApiResponseDetails_apiId :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_apiId = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiId} -> apiId) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiId = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The API name.
importAssetFromApiGatewayApiResponseDetails_apiName :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_apiName = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiName} -> apiName) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiName = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The Base64-encoded Md5 hash for the API asset, used to ensure the
-- integrity of the API at that location.
importAssetFromApiGatewayApiResponseDetails_apiSpecificationMd5Hash :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_apiSpecificationMd5Hash = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiSpecificationMd5Hash} -> apiSpecificationMd5Hash) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiSpecificationMd5Hash = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The upload URL of the API specification.
importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrl :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrl = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiSpecificationUploadUrl} -> apiSpecificationUploadUrl) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiSpecificationUploadUrl = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The date and time that the upload URL expires, in ISO 8601 format.
importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrlExpiresAt :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.UTCTime
importAssetFromApiGatewayApiResponseDetails_apiSpecificationUploadUrlExpiresAt = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {apiSpecificationUploadUrlExpiresAt} -> apiSpecificationUploadUrlExpiresAt) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {apiSpecificationUploadUrlExpiresAt = a} :: ImportAssetFromApiGatewayApiResponseDetails) Prelude.. Data._Time

-- | The data set ID.
importAssetFromApiGatewayApiResponseDetails_dataSetId :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_dataSetId = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {dataSetId} -> dataSetId) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {dataSetId = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The protocol type.
importAssetFromApiGatewayApiResponseDetails_protocolType :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails ProtocolType
importAssetFromApiGatewayApiResponseDetails_protocolType = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {protocolType} -> protocolType) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {protocolType = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The revision ID.
importAssetFromApiGatewayApiResponseDetails_revisionId :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_revisionId = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {revisionId} -> revisionId) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {revisionId = a} :: ImportAssetFromApiGatewayApiResponseDetails)

-- | The API stage.
importAssetFromApiGatewayApiResponseDetails_stage :: Lens.Lens' ImportAssetFromApiGatewayApiResponseDetails Prelude.Text
importAssetFromApiGatewayApiResponseDetails_stage = Lens.lens (\ImportAssetFromApiGatewayApiResponseDetails' {stage} -> stage) (\s@ImportAssetFromApiGatewayApiResponseDetails' {} a -> s {stage = a} :: ImportAssetFromApiGatewayApiResponseDetails)

instance
  Data.FromJSON
    ImportAssetFromApiGatewayApiResponseDetails
  where
  parseJSON =
    Data.withObject
      "ImportAssetFromApiGatewayApiResponseDetails"
      ( \x ->
          ImportAssetFromApiGatewayApiResponseDetails'
            Prelude.<$> (x Data..:? "ApiDescription")
            Prelude.<*> (x Data..:? "ApiKey")
            Prelude.<*> (x Data..: "ApiId")
            Prelude.<*> (x Data..: "ApiName")
            Prelude.<*> (x Data..: "ApiSpecificationMd5Hash")
            Prelude.<*> (x Data..: "ApiSpecificationUploadUrl")
            Prelude.<*> (x Data..: "ApiSpecificationUploadUrlExpiresAt")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "ProtocolType")
            Prelude.<*> (x Data..: "RevisionId")
            Prelude.<*> (x Data..: "Stage")
      )

instance
  Prelude.Hashable
    ImportAssetFromApiGatewayApiResponseDetails
  where
  hashWithSalt
    _salt
    ImportAssetFromApiGatewayApiResponseDetails' {..} =
      _salt
        `Prelude.hashWithSalt` apiDescription
        `Prelude.hashWithSalt` apiKey
        `Prelude.hashWithSalt` apiId
        `Prelude.hashWithSalt` apiName
        `Prelude.hashWithSalt` apiSpecificationMd5Hash
        `Prelude.hashWithSalt` apiSpecificationUploadUrl
        `Prelude.hashWithSalt` apiSpecificationUploadUrlExpiresAt
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` protocolType
        `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` stage

instance
  Prelude.NFData
    ImportAssetFromApiGatewayApiResponseDetails
  where
  rnf ImportAssetFromApiGatewayApiResponseDetails' {..} =
    Prelude.rnf apiDescription
      `Prelude.seq` Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiName
      `Prelude.seq` Prelude.rnf apiSpecificationMd5Hash
      `Prelude.seq` Prelude.rnf apiSpecificationUploadUrl
      `Prelude.seq` Prelude.rnf apiSpecificationUploadUrlExpiresAt
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf stage
