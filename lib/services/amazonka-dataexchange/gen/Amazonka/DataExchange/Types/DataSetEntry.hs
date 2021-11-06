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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.DataSetEntry where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.AssetType
import Amazonka.DataExchange.Types.Origin
import Amazonka.DataExchange.Types.OriginDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A data set is an AWS resource with one or more revisions.
--
-- /See:/ 'newDataSetEntry' smart constructor.
data DataSetEntry = DataSetEntry'
  { -- | The data set ID of the owned data set corresponding to the entitled data
    -- set being viewed. This parameter is returned when a data set owner is
    -- viewing the entitled copy of its owned data set.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | If the origin of this data set is ENTITLED, includes the details for the
    -- product on AWS Marketplace.
    originDetails :: Prelude.Maybe OriginDetails,
    -- | A property that defines the data set as OWNED by the account (for
    -- providers) or ENTITLED to the account (for subscribers).
    origin :: Origin,
    -- | The type of asset that is added to a data set.
    assetType :: AssetType,
    -- | The description for the data set.
    description :: Prelude.Text,
    -- | The date and time that the data set was created, in ISO 8601 format.
    createdAt :: Core.POSIX,
    -- | The unique identifier for the data set.
    id :: Prelude.Text,
    -- | The ARN for the data set.
    arn :: Prelude.Text,
    -- | The date and time that the data set was last updated, in ISO 8601
    -- format.
    updatedAt :: Core.POSIX,
    -- | The name of the data set.
    name :: Prelude.Text
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
-- 'sourceId', 'dataSetEntry_sourceId' - The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
--
-- 'originDetails', 'dataSetEntry_originDetails' - If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
--
-- 'origin', 'dataSetEntry_origin' - A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
--
-- 'assetType', 'dataSetEntry_assetType' - The type of asset that is added to a data set.
--
-- 'description', 'dataSetEntry_description' - The description for the data set.
--
-- 'createdAt', 'dataSetEntry_createdAt' - The date and time that the data set was created, in ISO 8601 format.
--
-- 'id', 'dataSetEntry_id' - The unique identifier for the data set.
--
-- 'arn', 'dataSetEntry_arn' - The ARN for the data set.
--
-- 'updatedAt', 'dataSetEntry_updatedAt' - The date and time that the data set was last updated, in ISO 8601
-- format.
--
-- 'name', 'dataSetEntry_name' - The name of the data set.
newDataSetEntry ::
  -- | 'origin'
  Origin ->
  -- | 'assetType'
  AssetType ->
  -- | 'description'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  DataSetEntry
newDataSetEntry
  pOrigin_
  pAssetType_
  pDescription_
  pCreatedAt_
  pId_
  pArn_
  pUpdatedAt_
  pName_ =
    DataSetEntry'
      { sourceId = Prelude.Nothing,
        originDetails = Prelude.Nothing,
        origin = pOrigin_,
        assetType = pAssetType_,
        description = pDescription_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        id = pId_,
        arn = pArn_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        name = pName_
      }

-- | The data set ID of the owned data set corresponding to the entitled data
-- set being viewed. This parameter is returned when a data set owner is
-- viewing the entitled copy of its owned data set.
dataSetEntry_sourceId :: Lens.Lens' DataSetEntry (Prelude.Maybe Prelude.Text)
dataSetEntry_sourceId = Lens.lens (\DataSetEntry' {sourceId} -> sourceId) (\s@DataSetEntry' {} a -> s {sourceId = a} :: DataSetEntry)

-- | If the origin of this data set is ENTITLED, includes the details for the
-- product on AWS Marketplace.
dataSetEntry_originDetails :: Lens.Lens' DataSetEntry (Prelude.Maybe OriginDetails)
dataSetEntry_originDetails = Lens.lens (\DataSetEntry' {originDetails} -> originDetails) (\s@DataSetEntry' {} a -> s {originDetails = a} :: DataSetEntry)

-- | A property that defines the data set as OWNED by the account (for
-- providers) or ENTITLED to the account (for subscribers).
dataSetEntry_origin :: Lens.Lens' DataSetEntry Origin
dataSetEntry_origin = Lens.lens (\DataSetEntry' {origin} -> origin) (\s@DataSetEntry' {} a -> s {origin = a} :: DataSetEntry)

-- | The type of asset that is added to a data set.
dataSetEntry_assetType :: Lens.Lens' DataSetEntry AssetType
dataSetEntry_assetType = Lens.lens (\DataSetEntry' {assetType} -> assetType) (\s@DataSetEntry' {} a -> s {assetType = a} :: DataSetEntry)

-- | The description for the data set.
dataSetEntry_description :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_description = Lens.lens (\DataSetEntry' {description} -> description) (\s@DataSetEntry' {} a -> s {description = a} :: DataSetEntry)

-- | The date and time that the data set was created, in ISO 8601 format.
dataSetEntry_createdAt :: Lens.Lens' DataSetEntry Prelude.UTCTime
dataSetEntry_createdAt = Lens.lens (\DataSetEntry' {createdAt} -> createdAt) (\s@DataSetEntry' {} a -> s {createdAt = a} :: DataSetEntry) Prelude.. Core._Time

-- | The unique identifier for the data set.
dataSetEntry_id :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_id = Lens.lens (\DataSetEntry' {id} -> id) (\s@DataSetEntry' {} a -> s {id = a} :: DataSetEntry)

-- | The ARN for the data set.
dataSetEntry_arn :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_arn = Lens.lens (\DataSetEntry' {arn} -> arn) (\s@DataSetEntry' {} a -> s {arn = a} :: DataSetEntry)

-- | The date and time that the data set was last updated, in ISO 8601
-- format.
dataSetEntry_updatedAt :: Lens.Lens' DataSetEntry Prelude.UTCTime
dataSetEntry_updatedAt = Lens.lens (\DataSetEntry' {updatedAt} -> updatedAt) (\s@DataSetEntry' {} a -> s {updatedAt = a} :: DataSetEntry) Prelude.. Core._Time

-- | The name of the data set.
dataSetEntry_name :: Lens.Lens' DataSetEntry Prelude.Text
dataSetEntry_name = Lens.lens (\DataSetEntry' {name} -> name) (\s@DataSetEntry' {} a -> s {name = a} :: DataSetEntry)

instance Core.FromJSON DataSetEntry where
  parseJSON =
    Core.withObject
      "DataSetEntry"
      ( \x ->
          DataSetEntry'
            Prelude.<$> (x Core..:? "SourceId")
            Prelude.<*> (x Core..:? "OriginDetails")
            Prelude.<*> (x Core..: "Origin")
            Prelude.<*> (x Core..: "AssetType")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "UpdatedAt")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable DataSetEntry

instance Prelude.NFData DataSetEntry
