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
-- Module      : Amazonka.Location.Types.ListRouteCalculatorsResponseEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListRouteCalculatorsResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PricingPlan
import qualified Amazonka.Prelude as Prelude

-- | A route calculator resource listed in your AWS account.
--
-- /See:/ 'newListRouteCalculatorsResponseEntry' smart constructor.
data ListRouteCalculatorsResponseEntry = ListRouteCalculatorsResponseEntry'
  { -- | Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The name of the route calculator resource.
    calculatorName :: Prelude.Text,
    -- | The timestamp when the route calculator resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    createTime :: Data.POSIX,
    -- | The data provider of traffic and road network data. Indicates one of the
    -- available providers:
    --
    -- -   @Esri@
    --
    -- -   @Here@
    --
    -- For more information about data providers, see
    -- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
    dataSource :: Prelude.Text,
    -- | The optional description of the route calculator resource.
    description :: Prelude.Text,
    -- | The timestamp when the route calculator resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRouteCalculatorsResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pricingPlan', 'listRouteCalculatorsResponseEntry_pricingPlan' - Always returns @RequestBasedUsage@.
--
-- 'calculatorName', 'listRouteCalculatorsResponseEntry_calculatorName' - The name of the route calculator resource.
--
-- 'createTime', 'listRouteCalculatorsResponseEntry_createTime' - The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
--
-- 'dataSource', 'listRouteCalculatorsResponseEntry_dataSource' - The data provider of traffic and road network data. Indicates one of the
-- available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
--
-- 'description', 'listRouteCalculatorsResponseEntry_description' - The optional description of the route calculator resource.
--
-- 'updateTime', 'listRouteCalculatorsResponseEntry_updateTime' - The timestamp when the route calculator resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
newListRouteCalculatorsResponseEntry ::
  -- | 'calculatorName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'dataSource'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListRouteCalculatorsResponseEntry
newListRouteCalculatorsResponseEntry
  pCalculatorName_
  pCreateTime_
  pDataSource_
  pDescription_
  pUpdateTime_ =
    ListRouteCalculatorsResponseEntry'
      { pricingPlan =
          Prelude.Nothing,
        calculatorName = pCalculatorName_,
        createTime =
          Data._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        updateTime =
          Data._Time Lens.# pUpdateTime_
      }

-- | Always returns @RequestBasedUsage@.
listRouteCalculatorsResponseEntry_pricingPlan :: Lens.Lens' ListRouteCalculatorsResponseEntry (Prelude.Maybe PricingPlan)
listRouteCalculatorsResponseEntry_pricingPlan = Lens.lens (\ListRouteCalculatorsResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {pricingPlan = a} :: ListRouteCalculatorsResponseEntry)

-- | The name of the route calculator resource.
listRouteCalculatorsResponseEntry_calculatorName :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.Text
listRouteCalculatorsResponseEntry_calculatorName = Lens.lens (\ListRouteCalculatorsResponseEntry' {calculatorName} -> calculatorName) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {calculatorName = a} :: ListRouteCalculatorsResponseEntry)

-- | The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
listRouteCalculatorsResponseEntry_createTime :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.UTCTime
listRouteCalculatorsResponseEntry_createTime = Lens.lens (\ListRouteCalculatorsResponseEntry' {createTime} -> createTime) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {createTime = a} :: ListRouteCalculatorsResponseEntry) Prelude.. Data._Time

-- | The data provider of traffic and road network data. Indicates one of the
-- available providers:
--
-- -   @Esri@
--
-- -   @Here@
--
-- For more information about data providers, see
-- <https://docs.aws.amazon.com/location/latest/developerguide/what-is-data-provider.html Amazon Location Service data providers>.
listRouteCalculatorsResponseEntry_dataSource :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.Text
listRouteCalculatorsResponseEntry_dataSource = Lens.lens (\ListRouteCalculatorsResponseEntry' {dataSource} -> dataSource) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {dataSource = a} :: ListRouteCalculatorsResponseEntry)

-- | The optional description of the route calculator resource.
listRouteCalculatorsResponseEntry_description :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.Text
listRouteCalculatorsResponseEntry_description = Lens.lens (\ListRouteCalculatorsResponseEntry' {description} -> description) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {description = a} :: ListRouteCalculatorsResponseEntry)

-- | The timestamp when the route calculator resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
listRouteCalculatorsResponseEntry_updateTime :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.UTCTime
listRouteCalculatorsResponseEntry_updateTime = Lens.lens (\ListRouteCalculatorsResponseEntry' {updateTime} -> updateTime) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {updateTime = a} :: ListRouteCalculatorsResponseEntry) Prelude.. Data._Time

instance
  Data.FromJSON
    ListRouteCalculatorsResponseEntry
  where
  parseJSON =
    Data.withObject
      "ListRouteCalculatorsResponseEntry"
      ( \x ->
          ListRouteCalculatorsResponseEntry'
            Prelude.<$> (x Data..:? "PricingPlan")
            Prelude.<*> (x Data..: "CalculatorName")
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "DataSource")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListRouteCalculatorsResponseEntry
  where
  hashWithSalt
    _salt
    ListRouteCalculatorsResponseEntry' {..} =
      _salt `Prelude.hashWithSalt` pricingPlan
        `Prelude.hashWithSalt` calculatorName
        `Prelude.hashWithSalt` createTime
        `Prelude.hashWithSalt` dataSource
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` updateTime

instance
  Prelude.NFData
    ListRouteCalculatorsResponseEntry
  where
  rnf ListRouteCalculatorsResponseEntry' {..} =
    Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf calculatorName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf updateTime
