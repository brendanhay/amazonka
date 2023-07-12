{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.GetAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about an asset.
module Amazonka.DataExchange.GetAsset
  ( -- * Creating a Request
    GetAsset (..),
    newGetAsset,

    -- * Request Lenses
    getAsset_assetId,
    getAsset_dataSetId,
    getAsset_revisionId,

    -- * Destructuring the Response
    GetAssetResponse (..),
    newGetAssetResponse,

    -- * Response Lenses
    getAssetResponse_arn,
    getAssetResponse_assetDetails,
    getAssetResponse_assetType,
    getAssetResponse_createdAt,
    getAssetResponse_dataSetId,
    getAssetResponse_id,
    getAssetResponse_name,
    getAssetResponse_revisionId,
    getAssetResponse_sourceId,
    getAssetResponse_updatedAt,
    getAssetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAsset' smart constructor.
data GetAsset = GetAsset'
  { -- | The unique identifier for an asset.
    assetId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'getAsset_assetId' - The unique identifier for an asset.
--
-- 'dataSetId', 'getAsset_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'getAsset_revisionId' - The unique identifier for a revision.
newGetAsset ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  GetAsset
newGetAsset pAssetId_ pDataSetId_ pRevisionId_ =
  GetAsset'
    { assetId = pAssetId_,
      dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | The unique identifier for an asset.
getAsset_assetId :: Lens.Lens' GetAsset Prelude.Text
getAsset_assetId = Lens.lens (\GetAsset' {assetId} -> assetId) (\s@GetAsset' {} a -> s {assetId = a} :: GetAsset)

-- | The unique identifier for a data set.
getAsset_dataSetId :: Lens.Lens' GetAsset Prelude.Text
getAsset_dataSetId = Lens.lens (\GetAsset' {dataSetId} -> dataSetId) (\s@GetAsset' {} a -> s {dataSetId = a} :: GetAsset)

-- | The unique identifier for a revision.
getAsset_revisionId :: Lens.Lens' GetAsset Prelude.Text
getAsset_revisionId = Lens.lens (\GetAsset' {revisionId} -> revisionId) (\s@GetAsset' {} a -> s {revisionId = a} :: GetAsset)

instance Core.AWSRequest GetAsset where
  type AWSResponse GetAsset = GetAssetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssetResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AssetDetails")
            Prelude.<*> (x Data..?> "AssetType")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "DataSetId")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RevisionId")
            Prelude.<*> (x Data..?> "SourceId")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAsset where
  hashWithSalt _salt GetAsset' {..} =
    _salt
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData GetAsset where
  rnf GetAsset' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Data.ToHeaders GetAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAsset where
  toPath GetAsset' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Data.toBS dataSetId,
        "/revisions/",
        Data.toBS revisionId,
        "/assets/",
        Data.toBS assetId
      ]

instance Data.ToQuery GetAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAssetResponse' smart constructor.
data GetAssetResponse = GetAssetResponse'
  { -- | The ARN for the asset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Details about the asset.
    assetDetails :: Prelude.Maybe AssetDetails,
    -- | The type of asset that is added to a data set.
    assetType :: Prelude.Maybe AssetType,
    -- | The date and time that the asset was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The unique identifier for the data set associated with this asset.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the asset.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the Amazon S3
    -- object key is used as the asset name. When exporting to Amazon S3, the
    -- asset name is used as default target Amazon S3 object key. When
    -- importing from Amazon API Gateway API, the API name is used as the asset
    -- name. When importing from Amazon Redshift, the datashare name is used as
    -- the asset name. When importing from AWS Lake Formation, the static
    -- values of \"Database(s) included in the LF-tag policy\" or \"Table(s)
    -- included in the LF-tag policy\" are used as the asset name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the revision associated with this asset.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The asset ID of the owned asset corresponding to the entitled asset
    -- being viewed. This parameter is returned when an asset owner is viewing
    -- the entitled copy of its owned asset.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the asset was last updated, in ISO 8601 format.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getAssetResponse_arn' - The ARN for the asset.
--
-- 'assetDetails', 'getAssetResponse_assetDetails' - Details about the asset.
--
-- 'assetType', 'getAssetResponse_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'getAssetResponse_createdAt' - The date and time that the asset was created, in ISO 8601 format.
--
-- 'dataSetId', 'getAssetResponse_dataSetId' - The unique identifier for the data set associated with this asset.
--
-- 'id', 'getAssetResponse_id' - The unique identifier for the asset.
--
-- 'name', 'getAssetResponse_name' - The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name. When exporting to Amazon S3, the
-- asset name is used as default target Amazon S3 object key. When
-- importing from Amazon API Gateway API, the API name is used as the asset
-- name. When importing from Amazon Redshift, the datashare name is used as
-- the asset name. When importing from AWS Lake Formation, the static
-- values of \"Database(s) included in the LF-tag policy\" or \"Table(s)
-- included in the LF-tag policy\" are used as the asset name.
--
-- 'revisionId', 'getAssetResponse_revisionId' - The unique identifier for the revision associated with this asset.
--
-- 'sourceId', 'getAssetResponse_sourceId' - The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
--
-- 'updatedAt', 'getAssetResponse_updatedAt' - The date and time that the asset was last updated, in ISO 8601 format.
--
-- 'httpStatus', 'getAssetResponse_httpStatus' - The response's http status code.
newGetAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssetResponse
newGetAssetResponse pHttpStatus_ =
  GetAssetResponse'
    { arn = Prelude.Nothing,
      assetDetails = Prelude.Nothing,
      assetType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the asset.
getAssetResponse_arn :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_arn = Lens.lens (\GetAssetResponse' {arn} -> arn) (\s@GetAssetResponse' {} a -> s {arn = a} :: GetAssetResponse)

-- | Details about the asset.
getAssetResponse_assetDetails :: Lens.Lens' GetAssetResponse (Prelude.Maybe AssetDetails)
getAssetResponse_assetDetails = Lens.lens (\GetAssetResponse' {assetDetails} -> assetDetails) (\s@GetAssetResponse' {} a -> s {assetDetails = a} :: GetAssetResponse)

-- | The type of asset that is added to a data set.
getAssetResponse_assetType :: Lens.Lens' GetAssetResponse (Prelude.Maybe AssetType)
getAssetResponse_assetType = Lens.lens (\GetAssetResponse' {assetType} -> assetType) (\s@GetAssetResponse' {} a -> s {assetType = a} :: GetAssetResponse)

-- | The date and time that the asset was created, in ISO 8601 format.
getAssetResponse_createdAt :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.UTCTime)
getAssetResponse_createdAt = Lens.lens (\GetAssetResponse' {createdAt} -> createdAt) (\s@GetAssetResponse' {} a -> s {createdAt = a} :: GetAssetResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the data set associated with this asset.
getAssetResponse_dataSetId :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_dataSetId = Lens.lens (\GetAssetResponse' {dataSetId} -> dataSetId) (\s@GetAssetResponse' {} a -> s {dataSetId = a} :: GetAssetResponse)

-- | The unique identifier for the asset.
getAssetResponse_id :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_id = Lens.lens (\GetAssetResponse' {id} -> id) (\s@GetAssetResponse' {} a -> s {id = a} :: GetAssetResponse)

-- | The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name. When exporting to Amazon S3, the
-- asset name is used as default target Amazon S3 object key. When
-- importing from Amazon API Gateway API, the API name is used as the asset
-- name. When importing from Amazon Redshift, the datashare name is used as
-- the asset name. When importing from AWS Lake Formation, the static
-- values of \"Database(s) included in the LF-tag policy\" or \"Table(s)
-- included in the LF-tag policy\" are used as the asset name.
getAssetResponse_name :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_name = Lens.lens (\GetAssetResponse' {name} -> name) (\s@GetAssetResponse' {} a -> s {name = a} :: GetAssetResponse)

-- | The unique identifier for the revision associated with this asset.
getAssetResponse_revisionId :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_revisionId = Lens.lens (\GetAssetResponse' {revisionId} -> revisionId) (\s@GetAssetResponse' {} a -> s {revisionId = a} :: GetAssetResponse)

-- | The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
getAssetResponse_sourceId :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.Text)
getAssetResponse_sourceId = Lens.lens (\GetAssetResponse' {sourceId} -> sourceId) (\s@GetAssetResponse' {} a -> s {sourceId = a} :: GetAssetResponse)

-- | The date and time that the asset was last updated, in ISO 8601 format.
getAssetResponse_updatedAt :: Lens.Lens' GetAssetResponse (Prelude.Maybe Prelude.UTCTime)
getAssetResponse_updatedAt = Lens.lens (\GetAssetResponse' {updatedAt} -> updatedAt) (\s@GetAssetResponse' {} a -> s {updatedAt = a} :: GetAssetResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getAssetResponse_httpStatus :: Lens.Lens' GetAssetResponse Prelude.Int
getAssetResponse_httpStatus = Lens.lens (\GetAssetResponse' {httpStatus} -> httpStatus) (\s@GetAssetResponse' {} a -> s {httpStatus = a} :: GetAssetResponse)

instance Prelude.NFData GetAssetResponse where
  rnf GetAssetResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetDetails
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
