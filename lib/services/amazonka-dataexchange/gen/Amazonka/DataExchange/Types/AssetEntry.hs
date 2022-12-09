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
-- Module      : Amazonka.DataExchange.Types.AssetEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.AssetDetails
import Amazonka.DataExchange.Types.AssetType
import qualified Amazonka.Prelude as Prelude

-- | An asset in AWS Data Exchange is a piece of data (Amazon S3 object) or a
-- means of fulfilling data (Amazon Redshift datashare or Amazon API
-- Gateway API, AWS Lake Formation data permission, or Amazon S3 data
-- access). The asset can be a structured data file, an image file, or some
-- other data file that can be stored as an Amazon S3 object, an Amazon API
-- Gateway API, or an Amazon Redshift datashare, an AWS Lake Formation data
-- permission, or an Amazon S3 data access. When you create an import job
-- for your files, API Gateway APIs, Amazon Redshift datashares, AWS Lake
-- Formation data permission, or Amazon S3 data access, you create an asset
-- in AWS Data Exchange.
--
-- /See:/ 'newAssetEntry' smart constructor.
data AssetEntry = AssetEntry'
  { -- | The asset ID of the owned asset corresponding to the entitled asset
    -- being viewed. This parameter is returned when an asset owner is viewing
    -- the entitled copy of its owned asset.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the asset.
    arn :: Prelude.Text,
    -- | Details about the asset.
    assetDetails :: AssetDetails,
    -- | The type of asset that is added to a data set.
    assetType :: AssetType,
    -- | The date and time that the asset was created, in ISO 8601 format.
    createdAt :: Data.POSIX,
    -- | The unique identifier for the data set associated with this asset.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the asset.
    id :: Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the Amazon S3
    -- object key is used as the asset name. When exporting to Amazon S3, the
    -- asset name is used as default target Amazon S3 object key. When
    -- importing from Amazon API Gateway API, the API name is used as the asset
    -- name. When importing from Amazon Redshift, the datashare name is used as
    -- the asset name. When importing from AWS Lake Formation, the static
    -- values of \"Database(s) included in LF-tag policy\" or \"Table(s)
    -- included in LF-tag policy\" are used as the asset name.
    name :: Prelude.Text,
    -- | The unique identifier for the revision associated with this asset.
    revisionId :: Prelude.Text,
    -- | The date and time that the asset was last updated, in ISO 8601 format.
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'assetEntry_sourceId' - The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
--
-- 'arn', 'assetEntry_arn' - The ARN for the asset.
--
-- 'assetDetails', 'assetEntry_assetDetails' - Details about the asset.
--
-- 'assetType', 'assetEntry_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'assetEntry_createdAt' - The date and time that the asset was created, in ISO 8601 format.
--
-- 'dataSetId', 'assetEntry_dataSetId' - The unique identifier for the data set associated with this asset.
--
-- 'id', 'assetEntry_id' - The unique identifier for the asset.
--
-- 'name', 'assetEntry_name' - The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name. When exporting to Amazon S3, the
-- asset name is used as default target Amazon S3 object key. When
-- importing from Amazon API Gateway API, the API name is used as the asset
-- name. When importing from Amazon Redshift, the datashare name is used as
-- the asset name. When importing from AWS Lake Formation, the static
-- values of \"Database(s) included in LF-tag policy\" or \"Table(s)
-- included in LF-tag policy\" are used as the asset name.
--
-- 'revisionId', 'assetEntry_revisionId' - The unique identifier for the revision associated with this asset.
--
-- 'updatedAt', 'assetEntry_updatedAt' - The date and time that the asset was last updated, in ISO 8601 format.
newAssetEntry ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'assetDetails'
  AssetDetails ->
  -- | 'assetType'
  AssetType ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  AssetEntry
newAssetEntry
  pArn_
  pAssetDetails_
  pAssetType_
  pCreatedAt_
  pDataSetId_
  pId_
  pName_
  pRevisionId_
  pUpdatedAt_ =
    AssetEntry'
      { sourceId = Prelude.Nothing,
        arn = pArn_,
        assetDetails = pAssetDetails_,
        assetType = pAssetType_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        dataSetId = pDataSetId_,
        id = pId_,
        name = pName_,
        revisionId = pRevisionId_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
assetEntry_sourceId :: Lens.Lens' AssetEntry (Prelude.Maybe Prelude.Text)
assetEntry_sourceId = Lens.lens (\AssetEntry' {sourceId} -> sourceId) (\s@AssetEntry' {} a -> s {sourceId = a} :: AssetEntry)

-- | The ARN for the asset.
assetEntry_arn :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_arn = Lens.lens (\AssetEntry' {arn} -> arn) (\s@AssetEntry' {} a -> s {arn = a} :: AssetEntry)

-- | Details about the asset.
assetEntry_assetDetails :: Lens.Lens' AssetEntry AssetDetails
assetEntry_assetDetails = Lens.lens (\AssetEntry' {assetDetails} -> assetDetails) (\s@AssetEntry' {} a -> s {assetDetails = a} :: AssetEntry)

-- | The type of asset that is added to a data set.
assetEntry_assetType :: Lens.Lens' AssetEntry AssetType
assetEntry_assetType = Lens.lens (\AssetEntry' {assetType} -> assetType) (\s@AssetEntry' {} a -> s {assetType = a} :: AssetEntry)

-- | The date and time that the asset was created, in ISO 8601 format.
assetEntry_createdAt :: Lens.Lens' AssetEntry Prelude.UTCTime
assetEntry_createdAt = Lens.lens (\AssetEntry' {createdAt} -> createdAt) (\s@AssetEntry' {} a -> s {createdAt = a} :: AssetEntry) Prelude.. Data._Time

-- | The unique identifier for the data set associated with this asset.
assetEntry_dataSetId :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_dataSetId = Lens.lens (\AssetEntry' {dataSetId} -> dataSetId) (\s@AssetEntry' {} a -> s {dataSetId = a} :: AssetEntry)

-- | The unique identifier for the asset.
assetEntry_id :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_id = Lens.lens (\AssetEntry' {id} -> id) (\s@AssetEntry' {} a -> s {id = a} :: AssetEntry)

-- | The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name. When exporting to Amazon S3, the
-- asset name is used as default target Amazon S3 object key. When
-- importing from Amazon API Gateway API, the API name is used as the asset
-- name. When importing from Amazon Redshift, the datashare name is used as
-- the asset name. When importing from AWS Lake Formation, the static
-- values of \"Database(s) included in LF-tag policy\" or \"Table(s)
-- included in LF-tag policy\" are used as the asset name.
assetEntry_name :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_name = Lens.lens (\AssetEntry' {name} -> name) (\s@AssetEntry' {} a -> s {name = a} :: AssetEntry)

-- | The unique identifier for the revision associated with this asset.
assetEntry_revisionId :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_revisionId = Lens.lens (\AssetEntry' {revisionId} -> revisionId) (\s@AssetEntry' {} a -> s {revisionId = a} :: AssetEntry)

-- | The date and time that the asset was last updated, in ISO 8601 format.
assetEntry_updatedAt :: Lens.Lens' AssetEntry Prelude.UTCTime
assetEntry_updatedAt = Lens.lens (\AssetEntry' {updatedAt} -> updatedAt) (\s@AssetEntry' {} a -> s {updatedAt = a} :: AssetEntry) Prelude.. Data._Time

instance Data.FromJSON AssetEntry where
  parseJSON =
    Data.withObject
      "AssetEntry"
      ( \x ->
          AssetEntry'
            Prelude.<$> (x Data..:? "SourceId")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "AssetDetails")
            Prelude.<*> (x Data..: "AssetType")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "RevisionId")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable AssetEntry where
  hashWithSalt _salt AssetEntry' {..} =
    _salt `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assetDetails
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData AssetEntry where
  rnf AssetEntry' {..} =
    Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assetDetails
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf updatedAt
