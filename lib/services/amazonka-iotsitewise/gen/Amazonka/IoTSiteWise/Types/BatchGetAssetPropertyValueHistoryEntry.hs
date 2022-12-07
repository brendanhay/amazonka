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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Quality
import Amazonka.IoTSiteWise.Types.TimeOrdering
import qualified Amazonka.Prelude as Prelude

-- | Contains information for an asset property historical value entry that
-- is associated with the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_BatchGetAssetPropertyValue.html BatchGetAssetPropertyValueHistory>
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
-- /See:/ 'newBatchGetAssetPropertyValueHistoryEntry' smart constructor.
data BatchGetAssetPropertyValueHistoryEntry = BatchGetAssetPropertyValueHistoryEntry'
  { -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The inclusive end of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    endDate :: Prelude.Maybe Data.POSIX,
    -- | The quality by which to filter asset data.
    qualities :: Prelude.Maybe (Prelude.NonEmpty Quality),
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The exclusive start of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    startDate :: Prelude.Maybe Data.POSIX,
    -- | The chronological sorting order of the requested information.
    --
    -- Default: @ASCENDING@
    timeOrdering :: Prelude.Maybe TimeOrdering,
    -- | The ID of the entry.
    entryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAssetPropertyValueHistoryEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyAlias', 'batchGetAssetPropertyValueHistoryEntry_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetId', 'batchGetAssetPropertyValueHistoryEntry_assetId' - The ID of the asset in which the asset property was created.
--
-- 'endDate', 'batchGetAssetPropertyValueHistoryEntry_endDate' - The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'qualities', 'batchGetAssetPropertyValueHistoryEntry_qualities' - The quality by which to filter asset data.
--
-- 'propertyId', 'batchGetAssetPropertyValueHistoryEntry_propertyId' - The ID of the asset property.
--
-- 'startDate', 'batchGetAssetPropertyValueHistoryEntry_startDate' - The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'timeOrdering', 'batchGetAssetPropertyValueHistoryEntry_timeOrdering' - The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
--
-- 'entryId', 'batchGetAssetPropertyValueHistoryEntry_entryId' - The ID of the entry.
newBatchGetAssetPropertyValueHistoryEntry ::
  -- | 'entryId'
  Prelude.Text ->
  BatchGetAssetPropertyValueHistoryEntry
newBatchGetAssetPropertyValueHistoryEntry pEntryId_ =
  BatchGetAssetPropertyValueHistoryEntry'
    { propertyAlias =
        Prelude.Nothing,
      assetId = Prelude.Nothing,
      endDate = Prelude.Nothing,
      qualities = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      timeOrdering = Prelude.Nothing,
      entryId = pEntryId_
    }

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
batchGetAssetPropertyValueHistoryEntry_propertyAlias :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueHistoryEntry_propertyAlias = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {propertyAlias} -> propertyAlias) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {propertyAlias = a} :: BatchGetAssetPropertyValueHistoryEntry)

-- | The ID of the asset in which the asset property was created.
batchGetAssetPropertyValueHistoryEntry_assetId :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueHistoryEntry_assetId = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {assetId} -> assetId) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {assetId = a} :: BatchGetAssetPropertyValueHistoryEntry)

-- | The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
batchGetAssetPropertyValueHistoryEntry_endDate :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe Prelude.UTCTime)
batchGetAssetPropertyValueHistoryEntry_endDate = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {endDate} -> endDate) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {endDate = a} :: BatchGetAssetPropertyValueHistoryEntry) Prelude.. Lens.mapping Data._Time

-- | The quality by which to filter asset data.
batchGetAssetPropertyValueHistoryEntry_qualities :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe (Prelude.NonEmpty Quality))
batchGetAssetPropertyValueHistoryEntry_qualities = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {qualities} -> qualities) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {qualities = a} :: BatchGetAssetPropertyValueHistoryEntry) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the asset property.
batchGetAssetPropertyValueHistoryEntry_propertyId :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe Prelude.Text)
batchGetAssetPropertyValueHistoryEntry_propertyId = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {propertyId} -> propertyId) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {propertyId = a} :: BatchGetAssetPropertyValueHistoryEntry)

-- | The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
batchGetAssetPropertyValueHistoryEntry_startDate :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe Prelude.UTCTime)
batchGetAssetPropertyValueHistoryEntry_startDate = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {startDate} -> startDate) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {startDate = a} :: BatchGetAssetPropertyValueHistoryEntry) Prelude.. Lens.mapping Data._Time

-- | The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
batchGetAssetPropertyValueHistoryEntry_timeOrdering :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry (Prelude.Maybe TimeOrdering)
batchGetAssetPropertyValueHistoryEntry_timeOrdering = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {timeOrdering} -> timeOrdering) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {timeOrdering = a} :: BatchGetAssetPropertyValueHistoryEntry)

-- | The ID of the entry.
batchGetAssetPropertyValueHistoryEntry_entryId :: Lens.Lens' BatchGetAssetPropertyValueHistoryEntry Prelude.Text
batchGetAssetPropertyValueHistoryEntry_entryId = Lens.lens (\BatchGetAssetPropertyValueHistoryEntry' {entryId} -> entryId) (\s@BatchGetAssetPropertyValueHistoryEntry' {} a -> s {entryId = a} :: BatchGetAssetPropertyValueHistoryEntry)

instance
  Prelude.Hashable
    BatchGetAssetPropertyValueHistoryEntry
  where
  hashWithSalt
    _salt
    BatchGetAssetPropertyValueHistoryEntry' {..} =
      _salt `Prelude.hashWithSalt` propertyAlias
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` endDate
        `Prelude.hashWithSalt` qualities
        `Prelude.hashWithSalt` propertyId
        `Prelude.hashWithSalt` startDate
        `Prelude.hashWithSalt` timeOrdering
        `Prelude.hashWithSalt` entryId

instance
  Prelude.NFData
    BatchGetAssetPropertyValueHistoryEntry
  where
  rnf BatchGetAssetPropertyValueHistoryEntry' {..} =
    Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf qualities
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf timeOrdering
      `Prelude.seq` Prelude.rnf entryId

instance
  Data.ToJSON
    BatchGetAssetPropertyValueHistoryEntry
  where
  toJSON BatchGetAssetPropertyValueHistoryEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("propertyAlias" Data..=) Prelude.<$> propertyAlias,
            ("assetId" Data..=) Prelude.<$> assetId,
            ("endDate" Data..=) Prelude.<$> endDate,
            ("qualities" Data..=) Prelude.<$> qualities,
            ("propertyId" Data..=) Prelude.<$> propertyId,
            ("startDate" Data..=) Prelude.<$> startDate,
            ("timeOrdering" Data..=) Prelude.<$> timeOrdering,
            Prelude.Just ("entryId" Data..= entryId)
          ]
      )
