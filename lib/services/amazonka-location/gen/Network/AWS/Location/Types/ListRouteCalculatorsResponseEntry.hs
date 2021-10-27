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
-- Module      : Network.AWS.Location.Types.ListRouteCalculatorsResponseEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.ListRouteCalculatorsResponseEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types.PricingPlan
import qualified Network.AWS.Prelude as Prelude

-- | A route calculator resource listed in your AWS account.
--
-- /See:/ 'newListRouteCalculatorsResponseEntry' smart constructor.
data ListRouteCalculatorsResponseEntry = ListRouteCalculatorsResponseEntry'
  { -- | The name of the route calculator resource.
    calculatorName :: Prelude.Text,
    -- | The timestamp when the route calculator resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    createTime :: Core.POSIX,
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
    -- | The pricing plan for the specified route calculator resource.
    --
    -- For additional details and restrictions on each pricing plan option, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: PricingPlan,
    -- | The timestamp when the route calculator resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    --
    -- -   For example, @2020–07-2T12:15:20.000Z+01:00@
    updateTime :: Core.POSIX
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
-- 'pricingPlan', 'listRouteCalculatorsResponseEntry_pricingPlan' - The pricing plan for the specified route calculator resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
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
  -- | 'pricingPlan'
  PricingPlan ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListRouteCalculatorsResponseEntry
newListRouteCalculatorsResponseEntry
  pCalculatorName_
  pCreateTime_
  pDataSource_
  pDescription_
  pPricingPlan_
  pUpdateTime_ =
    ListRouteCalculatorsResponseEntry'
      { calculatorName =
          pCalculatorName_,
        createTime =
          Core._Time Lens.# pCreateTime_,
        dataSource = pDataSource_,
        description = pDescription_,
        pricingPlan = pPricingPlan_,
        updateTime =
          Core._Time Lens.# pUpdateTime_
      }

-- | The name of the route calculator resource.
listRouteCalculatorsResponseEntry_calculatorName :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.Text
listRouteCalculatorsResponseEntry_calculatorName = Lens.lens (\ListRouteCalculatorsResponseEntry' {calculatorName} -> calculatorName) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {calculatorName = a} :: ListRouteCalculatorsResponseEntry)

-- | The timestamp when the route calculator resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
listRouteCalculatorsResponseEntry_createTime :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.UTCTime
listRouteCalculatorsResponseEntry_createTime = Lens.lens (\ListRouteCalculatorsResponseEntry' {createTime} -> createTime) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {createTime = a} :: ListRouteCalculatorsResponseEntry) Prelude.. Core._Time

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

-- | The pricing plan for the specified route calculator resource.
--
-- For additional details and restrictions on each pricing plan option, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
listRouteCalculatorsResponseEntry_pricingPlan :: Lens.Lens' ListRouteCalculatorsResponseEntry PricingPlan
listRouteCalculatorsResponseEntry_pricingPlan = Lens.lens (\ListRouteCalculatorsResponseEntry' {pricingPlan} -> pricingPlan) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {pricingPlan = a} :: ListRouteCalculatorsResponseEntry)

-- | The timestamp when the route calculator resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- -   For example, @2020–07-2T12:15:20.000Z+01:00@
listRouteCalculatorsResponseEntry_updateTime :: Lens.Lens' ListRouteCalculatorsResponseEntry Prelude.UTCTime
listRouteCalculatorsResponseEntry_updateTime = Lens.lens (\ListRouteCalculatorsResponseEntry' {updateTime} -> updateTime) (\s@ListRouteCalculatorsResponseEntry' {} a -> s {updateTime = a} :: ListRouteCalculatorsResponseEntry) Prelude.. Core._Time

instance
  Core.FromJSON
    ListRouteCalculatorsResponseEntry
  where
  parseJSON =
    Core.withObject
      "ListRouteCalculatorsResponseEntry"
      ( \x ->
          ListRouteCalculatorsResponseEntry'
            Prelude.<$> (x Core..: "CalculatorName")
            Prelude.<*> (x Core..: "CreateTime")
            Prelude.<*> (x Core..: "DataSource")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..: "PricingPlan")
            Prelude.<*> (x Core..: "UpdateTime")
      )

instance
  Prelude.Hashable
    ListRouteCalculatorsResponseEntry

instance
  Prelude.NFData
    ListRouteCalculatorsResponseEntry
