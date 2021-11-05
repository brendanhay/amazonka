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
-- Module      : Network.AWS.Location.Types.ListPlaceIndexesResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.ListPlaceIndexesResponseEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types.PricingPlan
import qualified Network.AWS.Prelude as Prelude

-- | A place index resource listed in your AWS account.
--
-- /See:/ 'newListPlaceIndexesResponseEntry' smart constructor.
data ListPlaceIndexesResponseEntry = ListPlaceIndexesResponseEntry'
  { -- | The timestamp for when the place index resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Core.POSIX,
    -- | The data provider of geospatial data. Indicates one of the available
    -- providers:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For additional details on data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The optional description for the place index resource.
    description :: Prelude.Text,
    -- | The name of the place index resource.
    indexName :: Prelude.Text,
    -- | The pricing plan for the specified place index resource.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: PricingPlan,
    -- | The timestamp for when the place index resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Core.POSIX
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
-- 'createTime', 'listPlaceIndexesResponseEntry_createTime' - The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'dataSource', 'listPlaceIndexesResponseEntry_dataSource' - The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'description', 'listPlaceIndexesResponseEntry_description' - The optional description for the place index resource.
--
-- 'indexName', 'listPlaceIndexesResponseEntry_indexName' - The name of the place index resource.
--
-- 'pricingPlan', 'listPlaceIndexesResponseEntry_pricingPlan' - The pricing plan for the specified place index resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
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
  -- | 'pricingPlan'
  PricingPlan ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListPlaceIndexesResponseEntry
newListPlaceIndexesResponseEntry
  pCreateTime_
  pDataSource_
  pDescription_
  pIndexName_
  pPricingPlan_
  pUpdateTime_ =
    ListPlaceIndexesResponseEntry'
      { createTime =
          Core._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        indexName = pIndexName_,
        pricingPlan = pPricingPlan_,
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | The timestamp for when the place index resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listPlaceIndexesResponseEntry_createTime :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.UTCTime
listPlaceIndexesResponseEntry_createTime = Lens.lens (\ListPlaceIndexesResponseEntry' {createTime} -> createTime) (\s@ListPlaceIndexesResponseEntry' {} a -> s {createTime = a} :: ListPlaceIndexesResponseEntry) Prelude.. Core._Time

-- | The data provider of geospatial data. Indicates one of the available
-- providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For additional details on data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
listPlaceIndexesResponseEntry_dataSource :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_dataSource = Lens.lens (\ListPlaceIndexesResponseEntry' {dataSource} -> dataSource) (\s@ListPlaceIndexesResponseEntry' {} a -> s {dataSource = a} :: ListPlaceIndexesResponseEntry)

-- | The optional description for the place index resource.
listPlaceIndexesResponseEntry_description :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_description = Lens.lens (\ListPlaceIndexesResponseEntry' {description} -> description) (\s@ListPlaceIndexesResponseEntry' {} a -> s {description = a} :: ListPlaceIndexesResponseEntry)

-- | The name of the place index resource.
listPlaceIndexesResponseEntry_indexName :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.Text
listPlaceIndexesResponseEntry_indexName = Lens.lens (\ListPlaceIndexesResponseEntry' {indexName} -> indexName) (\s@ListPlaceIndexesResponseEntry' {} a -> s {indexName = a} :: ListPlaceIndexesResponseEntry)

-- | The pricing plan for the specified place index resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
listPlaceIndexesResponseEntry_pricingPlan :: Lens.Lens' ListPlaceIndexesResponseEntry PricingPlan
listPlaceIndexesResponseEntry_pricingPlan = Lens.lens (\ListPlaceIndexesResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListPlaceIndexesResponseEntry' {} a -> s {pricingPlan = a} :: ListPlaceIndexesResponseEntry)

-- | The timestamp for when the place index resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listPlaceIndexesResponseEntry_updateTime :: Lens.Lens' ListPlaceIndexesResponseEntry Prelude.UTCTime
listPlaceIndexesResponseEntry_updateTime = Lens.lens (\ListPlaceIndexesResponseEntry' {updateTime} -> updateTime) (\s@ListPlaceIndexesResponseEntry' {} a -> s {updateTime = a} :: ListPlaceIndexesResponseEntry) Prelude.. Core._Time

instance Core.FromJSON ListPlaceIndexesResponseEntry where
  parseJSON =
    Core.withObject
      "ListPlaceIndexesResponseEntry"
      ( \x ->
          ListPlaceIndexesResponseEntry'
            Prelude.<$> (x Core..: "CreateTime")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "IndexName")
            Prelude.<*> (x Core..: "PricingPlan")
            Prelude.<*> (x Core..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListPlaceIndexesResponseEntry

instance Prelude.NFData ListPlaceIndexesResponseEntry
