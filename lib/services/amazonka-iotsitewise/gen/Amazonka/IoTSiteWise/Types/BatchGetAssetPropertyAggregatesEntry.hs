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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.AggregateType
import Amazonka.IoTSiteWise.Types.Quality
import Amazonka.IoTSiteWise.Types.TimeOrdering
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an asset property aggregate entry that is
-- associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyAggregates.html BatchGetAssetPropertyAggregates>
-- API.
--
-- To identify an asset property, you must specify one of the following:
--
-- -   The @assetId@ and @propertyId@ of an asset property.
--
-- -   A @propertyAlias@, which is a data stream alias (for example,
--     @\/company\/windfarm\/3\/turbine\/7\/temperature@). To define an
--     asset property\'s alias, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
--
-- /See:/ 'newBatchGetAssetPropertyAggregatesEntry' smart constructor.
data BatchGetAssetPropertyAggregatesEntry = BatchGetAssetPropertyAggregatesEntry'
  { -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The quality by which to filter asset data.
    qualities :: Prelude.Maybe (Prelude.NonEmpty Quality),
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The chronological sorting order of the requested information.
    --
    -- Default: @ASCENDING@
    timeOrdering :: Prelude.Maybe TimeOrdering,
    -- | The ID of the entry.
    entryId :: Prelude.Text,
    -- | The data aggregating function.
    aggregateTypes :: Prelude.NonEmpty AggregateType,
    -- | The time interval over which to aggregate data.
    resolution :: Prelude.Text,
    -- | The exclusive start of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    startDate :: Core.POSIX,
    -- | The inclusive end of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    endDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyAggregatesEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyAlias', 'batchGetAssetPropertyAggregatesEntry_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetId', 'batchGetAssetPropertyAggregatesEntry_assetId' - The ID of the asset in which the asset property was created.
--
-- 'qualities', 'batchGetAssetPropertyAggregatesEntry_qualities' - The quality by which to filter asset data.
--
-- 'propertyId', 'batchGetAssetPropertyAggregatesEntry_propertyId' - The ID of the asset property.
--
-- 'timeOrdering', 'batchGetAssetPropertyAggregatesEntry_timeOrdering' - The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
--
-- 'entryId', 'batchGetAssetPropertyAggregatesEntry_entryId' - The ID of the entry.
--
-- 'aggregateTypes', 'batchGetAssetPropertyAggregatesEntry_aggregateTypes' - The data aggregating function.
--
-- 'resolution', 'batchGetAssetPropertyAggregatesEntry_resolution' - The time interval over which to aggregate data.
--
-- 'startDate', 'batchGetAssetPropertyAggregatesEntry_startDate' - The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'endDate', 'batchGetAssetPropertyAggregatesEntry_endDate' - The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
newBatchGetAssetPropertyAggregatesEntry ::
  -- | 'entryId'
  Prelude.Text ->
  -- | 'aggregateTypes'
  Prelude.NonEmpty AggregateType ->
  -- | 'resolution'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  BatchGetAssetPropertyAggregatesEntry
newBatchGetAssetPropertyAggregatesEntry
  pEntryId_
  pAggregateTypes_
  pResolution_
  pStartDate_
  pEndDate_ =
    BatchGetAssetPropertyAggregatesEntry'
      { propertyAlias =
          Prelude.Nothing,
        assetId = Prelude.Nothing,
        qualities = Prelude.Nothing,
        propertyId = Prelude.Nothing,
        timeOrdering = Prelude.Nothing,
        entryId = pEntryId_,
        aggregateTypes =
          Lens.coerced
            Lens.# pAggregateTypes_,
        resolution = pResolution_,
        startDate =
          Core._Time Lens.# pStartDate_,
        endDate = Core._Time Lens.# pEndDate_
      }

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
batchGetAssetPropertyAggregatesEntry_propertyAlias :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyAggregatesEntry_propertyAlias = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {propertyAlias} -> propertyAlias) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {propertyAlias = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The ID of the asset in which the asset property was created.
batchGetAssetPropertyAggregatesEntry_assetId :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyAggregatesEntry_assetId = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {assetId} -> assetId) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {assetId = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The quality by which to filter asset data.
batchGetAssetPropertyAggregatesEntry_qualities :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.Maybe (Prelude.NonEmpty Quality))
batchGetAssetPropertyAggregatesEntry_qualities = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {qualities} -> qualities) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {qualities = a} :: BatchGetAssetPropertyAggregatesEntry) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the asset property.
batchGetAssetPropertyAggregatesEntry_propertyId :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyAggregatesEntry_propertyId = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {propertyId} -> propertyId) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {propertyId = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
batchGetAssetPropertyAggregatesEntry_timeOrdering :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.Maybe TimeOrdering)
batchGetAssetPropertyAggregatesEntry_timeOrdering = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {timeOrdering} -> timeOrdering) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {timeOrdering = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The ID of the entry.
batchGetAssetPropertyAggregatesEntry_entryId :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry Prelude.Text
batchGetAssetPropertyAggregatesEntry_entryId = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The data aggregating function.
batchGetAssetPropertyAggregatesEntry_aggregateTypes :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry (Prelude.NonEmpty AggregateType)
batchGetAssetPropertyAggregatesEntry_aggregateTypes = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {aggregateTypes} -> aggregateTypes) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {aggregateTypes = a} :: BatchGetAssetPropertyAggregatesEntry) Prelude.. Lens.coerced

-- | The time interval over which to aggregate data.
batchGetAssetPropertyAggregatesEntry_resolution :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry Prelude.Text
batchGetAssetPropertyAggregatesEntry_resolution = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {resolution} -> resolution) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {resolution = a} :: BatchGetAssetPropertyAggregatesEntry)

-- | The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
batchGetAssetPropertyAggregatesEntry_startDate :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry Prelude.UTCTime
batchGetAssetPropertyAggregatesEntry_startDate = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {startDate} -> startDate) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {startDate = a} :: BatchGetAssetPropertyAggregatesEntry) Prelude.. Core._Time

-- | The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
batchGetAssetPropertyAggregatesEntry_endDate :: Lens.Lens' BatchGetAssetPropertyAggregatesEntry Prelude.UTCTime
batchGetAssetPropertyAggregatesEntry_endDate = Lens.lens (\BatchGetAssetPropertyAggregatesEntry' {endDate} -> endDate) (\s@BatchGetAssetPropertyAggregatesEntry' {} a -> s {endDate = a} :: BatchGetAssetPropertyAggregatesEntry) Prelude.. Core._Time

instance
  Prelude.Hashable
    BatchGetAssetPropertyAggregatesEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyAggregatesEntry' {..} =
      _salt `Prelude.hashWithSalt` propertyAlias
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` qualities
        `Prelude.hashWithSalt` propertyId
        `Prelude.hashWithSalt` timeOrdering
        `Prelude.hashWithSalt` entryId
        `Prelude.hashWithSalt` aggregateTypes
        `Prelude.hashWithSalt` resolution
        `Prelude.hashWithSalt` startDate
        `Prelude.hashWithSalt` endDate

instance
  Prelude.NFData
    BatchGetAssetPropertyAggregatesEntry
  where
  rnf BatchGetAssetPropertyAggregatesEntry' {..} =
    Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf qualities
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf timeOrdering
      `Prelude.seq` Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf aggregateTypes
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf endDate

instance
  Core.ToJSON
    BatchGetAssetPropertyAggregatesEntry
  where
  toJSON BatchGetAssetPropertyAggregatesEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("propertyAlias" Core..=) Prelude.<$> propertyAlias,
            ("assetId" Core..=) Prelude.<$> assetId,
            ("qualities" Core..=) Prelude.<$> qualities,
            ("propertyId" Core..=) Prelude.<$> propertyId,
            ("timeOrdering" Core..=) Prelude.<$> timeOrdering,
            Prelude.Just ("entryId" Core..= entryId),
            Prelude.Just
              ("aggregateTypes" Core..= aggregateTypes),
            Prelude.Just ("resolution" Core..= resolution),
            Prelude.Just ("startDate" Core..= startDate),
            Prelude.Just ("endDate" Core..= endDate)
          ]
      )
