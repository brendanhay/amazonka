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
-- Module      : Amazonka.DataExchange.Types.DataSetEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.DataSetEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.AssetType
import Amazonka.DataExchange.Types.Origin
import Amazonka.DataExchange.Types.OriginDetails
import qualified Amazonka.Prelude as Prelude

-- | A data set is an AWS resource with one or more revisions.
--
-- /See:/ 'newDataSetEntry' smart constructor.
data DataSetEntry = DataSetEntry'
  { -- | If the origin of this data set is ENTITLED, includes the details for the
    -- product on AWS Marketplace.
    originDetails :: Prelude.Maybe OriginDetails,
    -- | The data set ID of the owned data set corresponding to the entitled data
    -- set being viewed. This parameter is returned when a data set owner is
    -- viewing the entitled copy of its owned data set.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the data set.
    arn :: Prelude.Text,
    -- | The type of asset that is added to a data set.
    assetType :: AssetType,
    -- | The date and time that the data set was created, in ISO 8601 format.
    createdAt :: Data.ISO8601,
    -- | The description for the data set.
    description :: Prelude.Text,
    -- | The unique identifier for the data set.
    id :: Prelude.Text,
    -- | The name of the data set.
    name :: Prelude.Text,
    -- | A property that defines the data set as OWNED by the account (for
    -- providers) or ENTITLED to the account (for subscribers).
    origin :: Origin,
    -- | The date and time that the data set was last updated, in ISO 8601
    -- format.
    updatedAt :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originDetails', 'dataSetEntry_originDetails' - If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
--
-- 'sourceId', 'dataSetEntry_sourceId' - The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
--
-- 'arn', 'dataSetEntry_arn' - The ARN for the data set.
--
-- 'assetType', 'dataSetEntry_assetType' - The type of asset that is added to a data set.
--
-- 'createdAt', 'dataSetEntry_createdAt' - The date and time that the data set was created, in ISO 8601 format.
--
-- 'description', 'dataSetEntry_description' - The description for the data set.
--
-- 'id', 'dataSetEntry_id' - The unique identifier for the data set.
--
-- 'name', 'dataSetEntry_name' - The name of the data set.
--
-- 'origin', 'dataSetEntry_origin' - A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
--
-- 'updatedAt', 'dataSetEntry_updatedAt' - The date and time that the data set was last updated, in ISO 8601
-- format.
newDataSetEntry ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'assetType'
  AssetType ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'origin'
  Origin ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  DataSetEntry
newDataSetEntry
  pArn_
  pAssetType_
  pCreatedAt_
  pDescription_
  pId_
  pName_
  pOrigin_
  pUpdatedAt_ =
    DataSetEntry'
      { originDetails = Prelude.Nothing,
        sourceId = Prelude.Nothing,
        arn = pArn_,
        assetType = pAssetType_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        description = pDescription_,
        id = pId_,
        name = pName_,
        origin = pOrigin_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
dataSetEntry_originDetails :: Lens.Lens' DataSetEntry (Prelude.Maybe OriginDetails)
dataSetEntry_originDetails = Lens.lens (\DataSetEntry' {originDetails} -> originDetails) (\s@DataSetEntry' {} a -> s {originDetails = a} :: DataSetEntry)

-- | The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
dataSetEntry_sourceId :: Lens.Lens' DataSetEntry (Prelude.Maybe Prelude.Text)
dataSetEntry_sourceId = Lens.lens (\DataSetEntry' {sourceId} -> sourceId) (\s@DataSetEntry' {} a -> s {sourceId = a} :: DataSetEntry)

-- | The ARN for the data set.
dataSetEntry_arn :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_arn = Lens.lens (\DataSetEntry' {arn} -> arn) (\s@DataSetEntry' {} a -> s {arn = a} :: DataSetEntry)

-- | The type of asset that is added to a data set.
dataSetEntry_assetType :: Lens.Lens' DataSetEntry AssetType
dataSetEntry_assetType = Lens.lens (\DataSetEntry' {assetType} -> assetType) (\s@DataSetEntry' {} a -> s {assetType = a} :: DataSetEntry)

-- | The date and time that the data set was created, in ISO 8601 format.
dataSetEntry_createdAt :: Lens.Lens' DataSetEntry Prelude.UTCTime
dataSetEntry_createdAt = Lens.lens (\DataSetEntry' {createdAt} -> createdAt) (\s@DataSetEntry' {} a -> s {createdAt = a} :: DataSetEntry) Prelude.. Data._Time

-- | The description for the data set.
dataSetEntry_description :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_description = Lens.lens (\DataSetEntry' {description} -> description) (\s@DataSetEntry' {} a -> s {description = a} :: DataSetEntry)

-- | The unique identifier for the data set.
dataSetEntry_id :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_id = Lens.lens (\DataSetEntry' {id} -> id) (\s@DataSetEntry' {} a -> s {id = a} :: DataSetEntry)

-- | The name of the data set.
dataSetEntry_name :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_name = Lens.lens (\DataSetEntry' {name} -> name) (\s@DataSetEntry' {} a -> s {name = a} :: DataSetEntry)

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
dataSetEntry_origin :: Lens.Lens' DataSetEntry Origin
dataSetEntry_origin = Lens.lens (\DataSetEntry' {origin} -> origin) (\s@DataSetEntry' {} a -> s {origin = a} :: DataSetEntry)

-- | The date and time that the data set was last updated, in ISO 8601
-- format.
dataSetEntry_updatedAt :: Lens.Lens' DataSetEntry Prelude.UTCTime
dataSetEntry_updatedAt = Lens.lens (\DataSetEntry' {updatedAt} -> updatedAt) (\s@DataSetEntry' {} a -> s {updatedAt = a} :: DataSetEntry) Prelude.. Data._Time

instance Data.FromJSON DataSetEntry where
  parseJSON =
    Data.withObject
      "DataSetEntry"
      ( \x ->
          DataSetEntry'
            Prelude.<$> (x Data..:? "OriginDetails")
            Prelude.<*> (x Data..:? "SourceId")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "AssetType")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Origin")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable DataSetEntry where
  hashWithSalt _salt DataSetEntry' {..} =
    _salt
      `Prelude.hashWithSalt` originDetails
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData DataSetEntry where
  rnf DataSetEntry' {..} =
    Prelude.rnf originDetails `Prelude.seq`
      Prelude.rnf sourceId `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf assetType `Prelude.seq`
            Prelude.rnf createdAt `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf id `Prelude.seq`
                  Prelude.rnf name `Prelude.seq`
                    Prelude.rnf origin `Prelude.seq`
                      Prelude.rnf updatedAt
