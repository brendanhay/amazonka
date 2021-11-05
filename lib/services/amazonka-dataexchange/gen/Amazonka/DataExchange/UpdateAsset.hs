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
-- Module      : Amazonka.DataExchange.UpdateAsset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates an asset.
module Amazonka.DataExchange.UpdateAsset
  ( -- * Creating a Request
    UpdateAsset (..),
    newUpdateAsset,

    -- * Request Lenses
    updateAsset_revisionId,
    updateAsset_assetId,
    updateAsset_dataSetId,
    updateAsset_name,

    -- * Destructuring the Response
    UpdateAssetResponse (..),
    newUpdateAssetResponse,

    -- * Response Lenses
    updateAssetResponse_arn,
    updateAssetResponse_createdAt,
    updateAssetResponse_sourceId,
    updateAssetResponse_dataSetId,
    updateAssetResponse_name,
    updateAssetResponse_assetDetails,
    updateAssetResponse_id,
    updateAssetResponse_assetType,
    updateAssetResponse_updatedAt,
    updateAssetResponse_revisionId,
    updateAssetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateAsset.
--
-- /See:/ 'newUpdateAsset' smart constructor.
data UpdateAsset = UpdateAsset'
  { -- | The unique identifier for a revision.
    revisionId :: Prelude.Text,
    -- | The unique identifier for an asset.
    assetId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the S3 object key
    -- is used as the asset name. When exporting to Amazon S3, the asset name
    -- is used as default target S3 object key.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'updateAsset_revisionId' - The unique identifier for a revision.
--
-- 'assetId', 'updateAsset_assetId' - The unique identifier for an asset.
--
-- 'dataSetId', 'updateAsset_dataSetId' - The unique identifier for a data set.
--
-- 'name', 'updateAsset_name' - The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
newUpdateAsset ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateAsset
newUpdateAsset
  pRevisionId_
  pAssetId_
  pDataSetId_
  pName_ =
    UpdateAsset'
      { revisionId = pRevisionId_,
        assetId = pAssetId_,
        dataSetId = pDataSetId_,
        name = pName_
      }

-- | The unique identifier for a revision.
updateAsset_revisionId :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_revisionId = Lens.lens (\UpdateAsset' {revisionId} -> revisionId) (\s@UpdateAsset' {} a -> s {revisionId = a} :: UpdateAsset)

-- | The unique identifier for an asset.
updateAsset_assetId :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_assetId = Lens.lens (\UpdateAsset' {assetId} -> assetId) (\s@UpdateAsset' {} a -> s {assetId = a} :: UpdateAsset)

-- | The unique identifier for a data set.
updateAsset_dataSetId :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_dataSetId = Lens.lens (\UpdateAsset' {dataSetId} -> dataSetId) (\s@UpdateAsset' {} a -> s {dataSetId = a} :: UpdateAsset)

-- | The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
updateAsset_name :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_name = Lens.lens (\UpdateAsset' {name} -> name) (\s@UpdateAsset' {} a -> s {name = a} :: UpdateAsset)

instance Core.AWSRequest UpdateAsset where
  type AWSResponse UpdateAsset = UpdateAssetResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssetResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "SourceId")
            Prelude.<*> (x Core..?> "DataSetId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "AssetDetails")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "AssetType")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "RevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAsset

instance Prelude.NFData UpdateAsset

instance Core.ToHeaders UpdateAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAsset where
  toJSON UpdateAsset' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath UpdateAsset where
  toPath UpdateAsset' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Core.toBS dataSetId,
        "/revisions/",
        Core.toBS revisionId,
        "/assets/",
        Core.toBS assetId
      ]

instance Core.ToQuery UpdateAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssetResponse' smart constructor.
data UpdateAssetResponse = UpdateAssetResponse'
  { -- | The ARN for the asset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the asset was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The asset ID of the owned asset corresponding to the entitled asset
    -- being viewed. This parameter is returned when an asset owner is viewing
    -- the entitled copy of its owned asset.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the data set associated with this asset.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the S3 object key
    -- is used as the asset name. When exporting to Amazon S3, the asset name
    -- is used as default target S3 object key.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the asset.
    assetDetails :: Prelude.Maybe AssetDetails,
    -- | The unique identifier for the asset.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of asset that is added to a data set.
    assetType :: Prelude.Maybe AssetType,
    -- | The date and time that the asset was last updated, in ISO 8601 format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for the revision associated with this asset.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateAssetResponse_arn' - The ARN for the asset.
--
-- 'createdAt', 'updateAssetResponse_createdAt' - The date and time that the asset was created, in ISO 8601 format.
--
-- 'sourceId', 'updateAssetResponse_sourceId' - The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
--
-- 'dataSetId', 'updateAssetResponse_dataSetId' - The unique identifier for the data set associated with this asset.
--
-- 'name', 'updateAssetResponse_name' - The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
--
-- 'assetDetails', 'updateAssetResponse_assetDetails' - Information about the asset.
--
-- 'id', 'updateAssetResponse_id' - The unique identifier for the asset.
--
-- 'assetType', 'updateAssetResponse_assetType' - The type of asset that is added to a data set.
--
-- 'updatedAt', 'updateAssetResponse_updatedAt' - The date and time that the asset was last updated, in ISO 8601 format.
--
-- 'revisionId', 'updateAssetResponse_revisionId' - The unique identifier for the revision associated with this asset.
--
-- 'httpStatus', 'updateAssetResponse_httpStatus' - The response's http status code.
newUpdateAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssetResponse
newUpdateAssetResponse pHttpStatus_ =
  UpdateAssetResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      name = Prelude.Nothing,
      assetDetails = Prelude.Nothing,
      id = Prelude.Nothing,
      assetType = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the asset.
updateAssetResponse_arn :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_arn = Lens.lens (\UpdateAssetResponse' {arn} -> arn) (\s@UpdateAssetResponse' {} a -> s {arn = a} :: UpdateAssetResponse)

-- | The date and time that the asset was created, in ISO 8601 format.
updateAssetResponse_createdAt :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.UTCTime)
updateAssetResponse_createdAt = Lens.lens (\UpdateAssetResponse' {createdAt} -> createdAt) (\s@UpdateAssetResponse' {} a -> s {createdAt = a} :: UpdateAssetResponse) Prelude.. Lens.mapping Core._Time

-- | The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
updateAssetResponse_sourceId :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_sourceId = Lens.lens (\UpdateAssetResponse' {sourceId} -> sourceId) (\s@UpdateAssetResponse' {} a -> s {sourceId = a} :: UpdateAssetResponse)

-- | The unique identifier for the data set associated with this asset.
updateAssetResponse_dataSetId :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_dataSetId = Lens.lens (\UpdateAssetResponse' {dataSetId} -> dataSetId) (\s@UpdateAssetResponse' {} a -> s {dataSetId = a} :: UpdateAssetResponse)

-- | The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
updateAssetResponse_name :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_name = Lens.lens (\UpdateAssetResponse' {name} -> name) (\s@UpdateAssetResponse' {} a -> s {name = a} :: UpdateAssetResponse)

-- | Information about the asset.
updateAssetResponse_assetDetails :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe AssetDetails)
updateAssetResponse_assetDetails = Lens.lens (\UpdateAssetResponse' {assetDetails} -> assetDetails) (\s@UpdateAssetResponse' {} a -> s {assetDetails = a} :: UpdateAssetResponse)

-- | The unique identifier for the asset.
updateAssetResponse_id :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_id = Lens.lens (\UpdateAssetResponse' {id} -> id) (\s@UpdateAssetResponse' {} a -> s {id = a} :: UpdateAssetResponse)

-- | The type of asset that is added to a data set.
updateAssetResponse_assetType :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe AssetType)
updateAssetResponse_assetType = Lens.lens (\UpdateAssetResponse' {assetType} -> assetType) (\s@UpdateAssetResponse' {} a -> s {assetType = a} :: UpdateAssetResponse)

-- | The date and time that the asset was last updated, in ISO 8601 format.
updateAssetResponse_updatedAt :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.UTCTime)
updateAssetResponse_updatedAt = Lens.lens (\UpdateAssetResponse' {updatedAt} -> updatedAt) (\s@UpdateAssetResponse' {} a -> s {updatedAt = a} :: UpdateAssetResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the revision associated with this asset.
updateAssetResponse_revisionId :: Lens.Lens' UpdateAssetResponse (Prelude.Maybe Prelude.Text)
updateAssetResponse_revisionId = Lens.lens (\UpdateAssetResponse' {revisionId} -> revisionId) (\s@UpdateAssetResponse' {} a -> s {revisionId = a} :: UpdateAssetResponse)

-- | The response's http status code.
updateAssetResponse_httpStatus :: Lens.Lens' UpdateAssetResponse Prelude.Int
updateAssetResponse_httpStatus = Lens.lens (\UpdateAssetResponse' {httpStatus} -> httpStatus) (\s@UpdateAssetResponse' {} a -> s {httpStatus = a} :: UpdateAssetResponse)

instance Prelude.NFData UpdateAssetResponse
