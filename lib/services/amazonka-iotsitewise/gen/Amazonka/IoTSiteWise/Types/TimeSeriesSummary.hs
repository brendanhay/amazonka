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
-- Module      : Amazonka.IoTSiteWise.Types.TimeSeriesSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.TimeSeriesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyDataType
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a time series (data stream).
--
-- /See:/ 'newTimeSeriesSummary' smart constructor.
data TimeSeriesSummary = TimeSeriesSummary'
  { -- | The alias that identifies the time series.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The data type of the structure for this time series. This parameter is
    -- required for time series that have the @STRUCT@ data type.
    --
    -- The options for this parameter depend on the type of the composite model
    -- in which you created the asset property that is associated with your
    -- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
    -- models.
    dataTypeSpec :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the time series.
    timeSeriesId :: Prelude.Text,
    -- | The data type of the time series.
    --
    -- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
    -- identify the type of the structure for this time series.
    dataType :: PropertyDataType,
    -- | The date that the time series was created, in Unix epoch time.
    timeSeriesCreationDate :: Data.POSIX,
    -- | The date that the time series was last updated, in Unix epoch time.
    timeSeriesLastUpdateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'timeSeriesSummary_alias' - The alias that identifies the time series.
--
-- 'assetId', 'timeSeriesSummary_assetId' - The ID of the asset in which the asset property was created.
--
-- 'dataTypeSpec', 'timeSeriesSummary_dataTypeSpec' - The data type of the structure for this time series. This parameter is
-- required for time series that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you created the asset property that is associated with your
-- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
-- models.
--
-- 'propertyId', 'timeSeriesSummary_propertyId' - The ID of the asset property.
--
-- 'timeSeriesId', 'timeSeriesSummary_timeSeriesId' - The ID of the time series.
--
-- 'dataType', 'timeSeriesSummary_dataType' - The data type of the time series.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this time series.
--
-- 'timeSeriesCreationDate', 'timeSeriesSummary_timeSeriesCreationDate' - The date that the time series was created, in Unix epoch time.
--
-- 'timeSeriesLastUpdateDate', 'timeSeriesSummary_timeSeriesLastUpdateDate' - The date that the time series was last updated, in Unix epoch time.
newTimeSeriesSummary ::
  -- | 'timeSeriesId'
  Prelude.Text ->
  -- | 'dataType'
  PropertyDataType ->
  -- | 'timeSeriesCreationDate'
  Prelude.UTCTime ->
  -- | 'timeSeriesLastUpdateDate'
  Prelude.UTCTime ->
  TimeSeriesSummary
newTimeSeriesSummary
  pTimeSeriesId_
  pDataType_
  pTimeSeriesCreationDate_
  pTimeSeriesLastUpdateDate_ =
    TimeSeriesSummary'
      { alias = Prelude.Nothing,
        assetId = Prelude.Nothing,
        dataTypeSpec = Prelude.Nothing,
        propertyId = Prelude.Nothing,
        timeSeriesId = pTimeSeriesId_,
        dataType = pDataType_,
        timeSeriesCreationDate =
          Data._Time Lens.# pTimeSeriesCreationDate_,
        timeSeriesLastUpdateDate =
          Data._Time Lens.# pTimeSeriesLastUpdateDate_
      }

-- | The alias that identifies the time series.
timeSeriesSummary_alias :: Lens.Lens' TimeSeriesSummary (Prelude.Maybe Prelude.Text)
timeSeriesSummary_alias = Lens.lens (\TimeSeriesSummary' {alias} -> alias) (\s@TimeSeriesSummary' {} a -> s {alias = a} :: TimeSeriesSummary)

-- | The ID of the asset in which the asset property was created.
timeSeriesSummary_assetId :: Lens.Lens' TimeSeriesSummary (Prelude.Maybe Prelude.Text)
timeSeriesSummary_assetId = Lens.lens (\TimeSeriesSummary' {assetId} -> assetId) (\s@TimeSeriesSummary' {} a -> s {assetId = a} :: TimeSeriesSummary)

-- | The data type of the structure for this time series. This parameter is
-- required for time series that have the @STRUCT@ data type.
--
-- The options for this parameter depend on the type of the composite model
-- in which you created the asset property that is associated with your
-- time series. Use @AWS\/ALARM_STATE@ for alarm state in alarm composite
-- models.
timeSeriesSummary_dataTypeSpec :: Lens.Lens' TimeSeriesSummary (Prelude.Maybe Prelude.Text)
timeSeriesSummary_dataTypeSpec = Lens.lens (\TimeSeriesSummary' {dataTypeSpec} -> dataTypeSpec) (\s@TimeSeriesSummary' {} a -> s {dataTypeSpec = a} :: TimeSeriesSummary)

-- | The ID of the asset property.
timeSeriesSummary_propertyId :: Lens.Lens' TimeSeriesSummary (Prelude.Maybe Prelude.Text)
timeSeriesSummary_propertyId = Lens.lens (\TimeSeriesSummary' {propertyId} -> propertyId) (\s@TimeSeriesSummary' {} a -> s {propertyId = a} :: TimeSeriesSummary)

-- | The ID of the time series.
timeSeriesSummary_timeSeriesId :: Lens.Lens' TimeSeriesSummary Prelude.Text
timeSeriesSummary_timeSeriesId = Lens.lens (\TimeSeriesSummary' {timeSeriesId} -> timeSeriesId) (\s@TimeSeriesSummary' {} a -> s {timeSeriesId = a} :: TimeSeriesSummary)

-- | The data type of the time series.
--
-- If you specify @STRUCT@, you must also specify @dataTypeSpec@ to
-- identify the type of the structure for this time series.
timeSeriesSummary_dataType :: Lens.Lens' TimeSeriesSummary PropertyDataType
timeSeriesSummary_dataType = Lens.lens (\TimeSeriesSummary' {dataType} -> dataType) (\s@TimeSeriesSummary' {} a -> s {dataType = a} :: TimeSeriesSummary)

-- | The date that the time series was created, in Unix epoch time.
timeSeriesSummary_timeSeriesCreationDate :: Lens.Lens' TimeSeriesSummary Prelude.UTCTime
timeSeriesSummary_timeSeriesCreationDate = Lens.lens (\TimeSeriesSummary' {timeSeriesCreationDate} -> timeSeriesCreationDate) (\s@TimeSeriesSummary' {} a -> s {timeSeriesCreationDate = a} :: TimeSeriesSummary) Prelude.. Data._Time

-- | The date that the time series was last updated, in Unix epoch time.
timeSeriesSummary_timeSeriesLastUpdateDate :: Lens.Lens' TimeSeriesSummary Prelude.UTCTime
timeSeriesSummary_timeSeriesLastUpdateDate = Lens.lens (\TimeSeriesSummary' {timeSeriesLastUpdateDate} -> timeSeriesLastUpdateDate) (\s@TimeSeriesSummary' {} a -> s {timeSeriesLastUpdateDate = a} :: TimeSeriesSummary) Prelude.. Data._Time

instance Data.FromJSON TimeSeriesSummary where
  parseJSON =
    Data.withObject
      "TimeSeriesSummary"
      ( \x ->
          TimeSeriesSummary'
            Prelude.<$> (x Data..:? "alias")
            Prelude.<*> (x Data..:? "assetId")
            Prelude.<*> (x Data..:? "dataTypeSpec")
            Prelude.<*> (x Data..:? "propertyId")
            Prelude.<*> (x Data..: "timeSeriesId")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "timeSeriesCreationDate")
            Prelude.<*> (x Data..: "timeSeriesLastUpdateDate")
      )

instance Prelude.Hashable TimeSeriesSummary where
  hashWithSalt _salt TimeSeriesSummary' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` dataTypeSpec
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` timeSeriesId
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` timeSeriesCreationDate
      `Prelude.hashWithSalt` timeSeriesLastUpdateDate

instance Prelude.NFData TimeSeriesSummary where
  rnf TimeSeriesSummary' {..} =
    Prelude.rnf alias `Prelude.seq`
      Prelude.rnf assetId `Prelude.seq`
        Prelude.rnf dataTypeSpec `Prelude.seq`
          Prelude.rnf propertyId `Prelude.seq`
            Prelude.rnf timeSeriesId `Prelude.seq`
              Prelude.rnf dataType `Prelude.seq`
                Prelude.rnf timeSeriesCreationDate `Prelude.seq`
                  Prelude.rnf timeSeriesLastUpdateDate
