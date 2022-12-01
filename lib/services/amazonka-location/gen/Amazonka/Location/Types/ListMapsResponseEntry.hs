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
-- Module      : Amazonka.Location.Types.ListMapsResponseEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListMapsResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | Contains details of an existing map resource in your AWS account.
--
-- /See:/ 'newListMapsResponseEntry' smart constructor.
data ListMapsResponseEntry = ListMapsResponseEntry'
  { -- | No longer used. Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The timestamp for when the map resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Core.POSIX,
    -- | Specifies the data provider for the associated map tiles.
    dataSource :: Prelude.Text,
    -- | The description for the map resource.
    description :: Prelude.Text,
    -- | The name of the associated map resource.
    mapName :: Prelude.Text,
    -- | The timestamp for when the map resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMapsResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'listMapsResponseEntry_pricingPlan' - No longer used. Always returns @RequestBasedUsage@.
--
-- 'createTime', 'listMapsResponseEntry_createTime' - The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'dataSource', 'listMapsResponseEntry_dataSource' - Specifies the data provider for the associated map tiles.
--
-- 'description', 'listMapsResponseEntry_description' - The description for the map resource.
--
-- 'mapName', 'listMapsResponseEntry_mapName' - The name of the associated map resource.
--
-- 'updateTime', 'listMapsResponseEntry_updateTime' - The timestamp for when the map resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListMapsResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListMapsResponseEntry
newListMapsResponseEntry
  pCreateTime_
  pDataSource_
  pDescription_
  pMapName_
  pUpdateTime_ =
    ListMapsResponseEntry'
      { pricingPlan =
          Prelude.Nothing,
        createTime = Core._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        mapName = pMapName_,
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | No longer used. Always returns @RequestBasedUsage@.
listMapsResponseEntry_pricingPlan :: Lens.Lens' ListMapsResponseEntry (Prelude.Maybe PricingPlan)
listMapsResponseEntry_pricingPlan = Lens.lens (\ListMapsResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListMapsResponseEntry' {} a -> s {pricingPlan = a} :: ListMapsResponseEntry)

-- | The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listMapsResponseEntry_createTime :: Lens.Lens' ListMapsResponseEntry Prelude.UTCTime
listMapsResponseEntry_createTime = Lens.lens (\ListMapsResponseEntry' {createTime} -> createTime) (\s@ListMapsResponseEntry' {} a -> s {createTime = a} :: ListMapsResponseEntry) Prelude.. Core._Time

-- | Specifies the data provider for the associated map tiles.
listMapsResponseEntry_dataSource :: Lens.Lens' ListMapsResponseEntry Prelude.Text
listMapsResponseEntry_dataSource = Lens.lens (\ListMapsResponseEntry' {dataSource} -> dataSource) (\s@ListMapsResponseEntry' {} a -> s {dataSource = a} :: ListMapsResponseEntry)

-- | The description for the map resource.
listMapsResponseEntry_description :: Lens.Lens' ListMapsResponseEntry Prelude.Text
listMapsResponseEntry_description = Lens.lens (\ListMapsResponseEntry' {description} -> description) (\s@ListMapsResponseEntry' {} a -> s {description = a} :: ListMapsResponseEntry)

-- | The name of the associated map resource.
listMapsResponseEntry_mapName :: Lens.Lens' ListMapsResponseEntry Prelude.Text
listMapsResponseEntry_mapName = Lens.lens (\ListMapsResponseEntry' {mapName} -> mapName) (\s@ListMapsResponseEntry' {} a -> s {mapName = a} :: ListMapsResponseEntry)

-- | The timestamp for when the map resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listMapsResponseEntry_updateTime :: Lens.Lens' ListMapsResponseEntry Prelude.UTCTime
listMapsResponseEntry_updateTime = Lens.lens (\ListMapsResponseEntry' {updateTime} -> updateTime) (\s@ListMapsResponseEntry' {} a -> s {updateTime = a} :: ListMapsResponseEntry) Prelude.. Core._Time

instance Core.FromJSON ListMapsResponseEntry where
  parseJSON =
    Core.withObject
      "ListMapsResponseEntry"
      ( \x ->
          ListMapsResponseEntry'
            Prelude.<$> (x Core..:? "PricingPlan")
            Prelude.<*> (x Core..: "CreateTime")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "MapName")
            Prelude.<*> (x Core..: "UpdateTime")
      )

instance Prelude.Hashable ListMapsResponseEntry where
  hashWithSalt _salt ListMapsResponseEntry' {..} =
    _salt `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` mapName
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ListMapsResponseEntry where
  rnf ListMapsResponseEntry' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf mapName
      `Prelude.seq` Prelude.rnf updateTime
