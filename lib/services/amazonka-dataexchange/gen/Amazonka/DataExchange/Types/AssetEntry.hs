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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.AssetEntry where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.AssetDetails
import Amazonka.DataExchange.Types.AssetType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An asset in AWS Data Exchange is a piece of data. The asset can be a
-- structured data file, an image file, or some other data file that can be
-- stored as an S3 object, or an Amazon Redshift datashare (Preview). When
-- you create an import job for your files, you create an asset in AWS Data
-- Exchange for each of those files.
--
-- /See:/ 'newAssetEntry' smart constructor.
data AssetEntry = AssetEntry'
  { -- | The asset ID of the owned asset corresponding to the entitled asset
    -- being viewed. This parameter is returned when an asset owner is viewing
    -- the entitled copy of its owned asset.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of asset that is added to a data set.
    assetType :: AssetType,
    -- | The date and time that the asset was created, in ISO 8601 format.
    createdAt :: Core.POSIX,
    -- | The unique identifier for the data set associated with this asset.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the asset.
    id :: Prelude.Text,
    -- | The ARN for the asset.
    arn :: Prelude.Text,
    -- | Information about the asset.
    assetDetails :: AssetDetails,
    -- | The date and time that the asset was last updated, in ISO 8601 format.
    updatedAt :: Core.POSIX,
    -- | The unique identifier for the revision associated with this asset.
    revisionId :: Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the S3 object key
    -- is used as the asset name. When exporting to Amazon S3, the asset name
    -- is used as default target S3 object key.
    name :: Prelude.Text
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
-- 'assetType', 'assetEntry_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'assetEntry_createdAt' - The date and time that the asset was created, in ISO 8601 format.
--
-- 'dataSetId', 'assetEntry_dataSetId' - The unique identifier for the data set associated with this asset.
--
-- 'id', 'assetEntry_id' - The unique identifier for the asset.
--
-- 'arn', 'assetEntry_arn' - The ARN for the asset.
--
-- 'assetDetails', 'assetEntry_assetDetails' - Information about the asset.
--
-- 'updatedAt', 'assetEntry_updatedAt' - The date and time that the asset was last updated, in ISO 8601 format.
--
-- 'revisionId', 'assetEntry_revisionId' - The unique identifier for the revision associated with this asset.
--
-- 'name', 'assetEntry_name' - The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
newAssetEntry ::
  -- | 'assetType'
  AssetType ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'assetDetails'
  AssetDetails ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  AssetEntry
newAssetEntry
  pAssetType_
  pCreatedAt_
  pDataSetId_
  pId_
  pArn_
  pAssetDetails_
  pUpdatedAt_
  pRevisionId_
  pName_ =
    AssetEntry'
      { sourceId = Prelude.Nothing,
        assetType = pAssetType_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        dataSetId = pDataSetId_,
        id = pId_,
        arn = pArn_,
        assetDetails = pAssetDetails_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        revisionId = pRevisionId_,
        name = pName_
      }

-- | The asset ID of the owned asset corresponding to the entitled asset
-- being viewed. This parameter is returned when an asset owner is viewing
-- the entitled copy of its owned asset.
assetEntry_sourceId :: Lens.Lens' AssetEntry (Prelude.Maybe Prelude.Text)
assetEntry_sourceId = Lens.lens (\AssetEntry' {sourceId} -> sourceId) (\s@AssetEntry' {} a -> s {sourceId = a} :: AssetEntry)

-- | The type of asset that is added to a data set.
assetEntry_assetType :: Lens.Lens' AssetEntry AssetType
assetEntry_assetType = Lens.lens (\AssetEntry' {assetType} -> assetType) (\s@AssetEntry' {} a -> s {assetType = a} :: AssetEntry)

-- | The date and time that the asset was created, in ISO 8601 format.
assetEntry_createdAt :: Lens.Lens' AssetEntry Prelude.UTCTime
assetEntry_createdAt = Lens.lens (\AssetEntry' {createdAt} -> createdAt) (\s@AssetEntry' {} a -> s {createdAt = a} :: AssetEntry) Prelude.. Core._Time

-- | The unique identifier for the data set associated with this asset.
assetEntry_dataSetId :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_dataSetId = Lens.lens (\AssetEntry' {dataSetId} -> dataSetId) (\s@AssetEntry' {} a -> s {dataSetId = a} :: AssetEntry)

-- | The unique identifier for the asset.
assetEntry_id :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_id = Lens.lens (\AssetEntry' {id} -> id) (\s@AssetEntry' {} a -> s {id = a} :: AssetEntry)

-- | The ARN for the asset.
assetEntry_arn :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_arn = Lens.lens (\AssetEntry' {arn} -> arn) (\s@AssetEntry' {} a -> s {arn = a} :: AssetEntry)

-- | Information about the asset.
assetEntry_assetDetails :: Lens.Lens' AssetEntry AssetDetails
assetEntry_assetDetails = Lens.lens (\AssetEntry' {assetDetails} -> assetDetails) (\s@AssetEntry' {} a -> s {assetDetails = a} :: AssetEntry)

-- | The date and time that the asset was last updated, in ISO 8601 format.
assetEntry_updatedAt :: Lens.Lens' AssetEntry Prelude.UTCTime
assetEntry_updatedAt = Lens.lens (\AssetEntry' {updatedAt} -> updatedAt) (\s@AssetEntry' {} a -> s {updatedAt = a} :: AssetEntry) Prelude.. Core._Time

-- | The unique identifier for the revision associated with this asset.
assetEntry_revisionId :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_revisionId = Lens.lens (\AssetEntry' {revisionId} -> revisionId) (\s@AssetEntry' {} a -> s {revisionId = a} :: AssetEntry)

-- | The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name. When exporting to Amazon S3, the asset name
-- is used as default target S3 object key.
assetEntry_name :: Lens.Lens' AssetEntry Prelude.Text
assetEntry_name = Lens.lens (\AssetEntry' {name} -> name) (\s@AssetEntry' {} a -> s {name = a} :: AssetEntry)

instance Core.FromJSON AssetEntry where
  parseJSON =
    Core.withObject
      "AssetEntry"
      ( \x ->
          AssetEntry'
            Prelude.<$> (x Core..:? "SourceId")
            Prelude.<*> (x Core..: "AssetType")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "DataSetId")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "AssetDetails")
            Prelude.<*> (x Core..: "UpdatedAt")
            Prelude.<*> (x Core..: "RevisionId")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable AssetEntry where
  hashWithSalt salt' AssetEntry' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` assetDetails
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` sourceId

instance Prelude.NFData AssetEntry where
  rnf AssetEntry' {..} =
    Prelude.rnf sourceId `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf assetDetails
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf assetType
