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
-- Module      : Amazonka.Location.Types.ListPlaceIndexesResponseEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListPlaceIndexesResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | A place index resource listed in your AWS account.
--
-- /See:/ 'newListPlaceIndexesResponseEntry' smart constructor.
data ListPlaceIndexesResponseEntry = ListPlaceIndexesResponseEntry'
  { -- | No longer used. Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The timestamp for when the place index resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The data provider of geospatial data. Values can be one of the
    -- following:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The optional description for the place index resource.
    description :: Prelude.Text,
    -- | The name of the place index resource.
    indexName :: Prelude.Text,
    -- | The timestamp for when the place index resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaceIndexesResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'listPlaceIndexesResponseEntry_pricingPlan' - No longer used. Always returns @RequestBasedUsage@.
--
-- 'createTime', 'listPlaceIndexesResponseEntry_createTime' - The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'dataSource', 'listPlaceIndexesResponseEntry_dataSource' - The data provider of geospatial data. Values can be one of the
-- following:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'description', 'listPlaceIndexesResponseEntry_description' - The optional description for the place index resource.
--
-- 'indexName', 'listPlaceIndexesResponseEntry_indexName' - The name of the place index resource.
--
-- 'updateTime', 'listPlaceIndexesResponseEntry_updateTime' - The timestamp for when the place index resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListPlaceIndexesResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListPlaceIndexesResponseEntry
newListPlaceIndexesResponseEntry
  pCreateTime_
  pDataSource_
  pDescription_
  pIndexName_
  pUpdateTime_ =
    ListPlaceIndexesResponseEntry'
      { pricingPlan =
          Prelude.Nothing,
        createTime = Data._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        indexName = pIndexName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | No longer used. Always returns @RequestBasedUsage@.
listPlaceIndexesResponseEntry_pricingPlan :: Lens.Lens' ListPlaceIndexesResponseEntry (Prelude.Maybe PricingPlan)
listPlaceIndexesResponseEntry_pricingPlan = Lens.lens (\ListPlaceIndexesResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListPlaceIndexesResponseEntry' {} a -> s {pricingPlan = a} :: ListPlaceIndexesResponseEntry)

-- | The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listPlaceIndexesResponseEntry_createTime :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.UTCTime
listPlaceIndexesResponseEntry_createTime = Lens.lens (\ListPlaceIndexesResponseEntry' {createTime} -> createTime) (\s@ListPlaceIndexesResponseEntry' {} a -> s {createTime = a} :: ListPlaceIndexesResponseEntry) Prelude.. Data._Time

-- | The data provider of geospatial data. Values can be one of the
-- following:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
listPlaceIndexesResponseEntry_dataSource :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_dataSource = Lens.lens (\ListPlaceIndexesResponseEntry' {dataSource} -> dataSource) (\s@ListPlaceIndexesResponseEntry' {} a -> s {dataSource = a} :: ListPlaceIndexesResponseEntry)

-- | The optional description for the place index resource.
listPlaceIndexesResponseEntry_description :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_description = Lens.lens (\ListPlaceIndexesResponseEntry' {description} -> description) (\s@ListPlaceIndexesResponseEntry' {} a -> s {description = a} :: ListPlaceIndexesResponseEntry)

-- | The name of the place index resource.
listPlaceIndexesResponseEntry_indexName :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_indexName = Lens.lens (\ListPlaceIndexesResponseEntry' {indexName} -> indexName) (\s@ListPlaceIndexesResponseEntry' {} a -> s {indexName = a} :: ListPlaceIndexesResponseEntry)

-- | The timestamp for when the place index resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listPlaceIndexesResponseEntry_updateTime :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.UTCTime
listPlaceIndexesResponseEntry_updateTime = Lens.lens (\ListPlaceIndexesResponseEntry' {updateTime} -> updateTime) (\s@ListPlaceIndexesResponseEntry' {} a -> s {updateTime = a} :: ListPlaceIndexesResponseEntry) Prelude.. Data._Time

instance Data.FromJSON ListPlaceIndexesResponseEntry where
  parseJSON =
    Data.withObject
      "ListPlaceIndexesResponseEntry"
      ( \x ->
          ListPlaceIndexesResponseEntry'
            Prelude.<$> (x Data..:? "PricingPlan")
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "IndexName")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListPlaceIndexesResponseEntry
  where
  hashWithSalt _salt ListPlaceIndexesResponseEntry' {..} =
    _salt `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ListPlaceIndexesResponseEntry where
  rnf ListPlaceIndexesResponseEntry' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf updateTime
