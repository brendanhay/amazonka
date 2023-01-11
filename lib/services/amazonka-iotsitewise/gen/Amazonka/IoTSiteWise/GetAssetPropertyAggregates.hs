{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.GetAssetPropertyAggregates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets aggregated values for an asset property. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#aggregates Querying aggregates>
-- in the /IoT SiteWise User Guide/.
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
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.GetAssetPropertyAggregates
  ( -- * Creating a Request
    GetAssetPropertyAggregates (..),
    newGetAssetPropertyAggregates,

    -- * Request Lenses
    getAssetPropertyAggregates_assetId,
    getAssetPropertyAggregates_maxResults,
    getAssetPropertyAggregates_nextToken,
    getAssetPropertyAggregates_propertyAlias,
    getAssetPropertyAggregates_propertyId,
    getAssetPropertyAggregates_qualities,
    getAssetPropertyAggregates_timeOrdering,
    getAssetPropertyAggregates_aggregateTypes,
    getAssetPropertyAggregates_resolution,
    getAssetPropertyAggregates_startDate,
    getAssetPropertyAggregates_endDate,

    -- * Destructuring the Response
    GetAssetPropertyAggregatesResponse (..),
    newGetAssetPropertyAggregatesResponse,

    -- * Response Lenses
    getAssetPropertyAggregatesResponse_nextToken,
    getAssetPropertyAggregatesResponse_httpStatus,
    getAssetPropertyAggregatesResponse_aggregatedValues,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssetPropertyAggregates' smart constructor.
data GetAssetPropertyAggregates = GetAssetPropertyAggregates'
  { -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 100
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The quality by which to filter asset data.
    qualities :: Prelude.Maybe (Prelude.NonEmpty Quality),
    -- | The chronological sorting order of the requested information.
    --
    -- Default: @ASCENDING@
    timeOrdering :: Prelude.Maybe TimeOrdering,
    -- | The data aggregating function.
    aggregateTypes :: Prelude.NonEmpty AggregateType,
    -- | The time interval over which to aggregate data.
    resolution :: Prelude.Text,
    -- | The exclusive start of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    startDate :: Data.POSIX,
    -- | The inclusive end of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    endDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'getAssetPropertyAggregates_assetId' - The ID of the asset.
--
-- 'maxResults', 'getAssetPropertyAggregates_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 100
--
-- 'nextToken', 'getAssetPropertyAggregates_nextToken' - The token to be used for the next set of paginated results.
--
-- 'propertyAlias', 'getAssetPropertyAggregates_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'propertyId', 'getAssetPropertyAggregates_propertyId' - The ID of the asset property.
--
-- 'qualities', 'getAssetPropertyAggregates_qualities' - The quality by which to filter asset data.
--
-- 'timeOrdering', 'getAssetPropertyAggregates_timeOrdering' - The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
--
-- 'aggregateTypes', 'getAssetPropertyAggregates_aggregateTypes' - The data aggregating function.
--
-- 'resolution', 'getAssetPropertyAggregates_resolution' - The time interval over which to aggregate data.
--
-- 'startDate', 'getAssetPropertyAggregates_startDate' - The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'endDate', 'getAssetPropertyAggregates_endDate' - The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
newGetAssetPropertyAggregates ::
  -- | 'aggregateTypes'
  Prelude.NonEmpty AggregateType ->
  -- | 'resolution'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  -- | 'endDate'
  Prelude.UTCTime ->
  GetAssetPropertyAggregates
newGetAssetPropertyAggregates
  pAggregateTypes_
  pResolution_
  pStartDate_
  pEndDate_ =
    GetAssetPropertyAggregates'
      { assetId =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        propertyAlias = Prelude.Nothing,
        propertyId = Prelude.Nothing,
        qualities = Prelude.Nothing,
        timeOrdering = Prelude.Nothing,
        aggregateTypes =
          Lens.coerced Lens.# pAggregateTypes_,
        resolution = pResolution_,
        startDate = Data._Time Lens.# pStartDate_,
        endDate = Data._Time Lens.# pEndDate_
      }

-- | The ID of the asset.
getAssetPropertyAggregates_assetId :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe Prelude.Text)
getAssetPropertyAggregates_assetId = Lens.lens (\GetAssetPropertyAggregates' {assetId} -> assetId) (\s@GetAssetPropertyAggregates' {} a -> s {assetId = a} :: GetAssetPropertyAggregates)

-- | The maximum number of results to return for each paginated request.
--
-- Default: 100
getAssetPropertyAggregates_maxResults :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe Prelude.Natural)
getAssetPropertyAggregates_maxResults = Lens.lens (\GetAssetPropertyAggregates' {maxResults} -> maxResults) (\s@GetAssetPropertyAggregates' {} a -> s {maxResults = a} :: GetAssetPropertyAggregates)

-- | The token to be used for the next set of paginated results.
getAssetPropertyAggregates_nextToken :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe Prelude.Text)
getAssetPropertyAggregates_nextToken = Lens.lens (\GetAssetPropertyAggregates' {nextToken} -> nextToken) (\s@GetAssetPropertyAggregates' {} a -> s {nextToken = a} :: GetAssetPropertyAggregates)

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
getAssetPropertyAggregates_propertyAlias :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe Prelude.Text)
getAssetPropertyAggregates_propertyAlias = Lens.lens (\GetAssetPropertyAggregates' {propertyAlias} -> propertyAlias) (\s@GetAssetPropertyAggregates' {} a -> s {propertyAlias = a} :: GetAssetPropertyAggregates)

-- | The ID of the asset property.
getAssetPropertyAggregates_propertyId :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe Prelude.Text)
getAssetPropertyAggregates_propertyId = Lens.lens (\GetAssetPropertyAggregates' {propertyId} -> propertyId) (\s@GetAssetPropertyAggregates' {} a -> s {propertyId = a} :: GetAssetPropertyAggregates)

-- | The quality by which to filter asset data.
getAssetPropertyAggregates_qualities :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe (Prelude.NonEmpty Quality))
getAssetPropertyAggregates_qualities = Lens.lens (\GetAssetPropertyAggregates' {qualities} -> qualities) (\s@GetAssetPropertyAggregates' {} a -> s {qualities = a} :: GetAssetPropertyAggregates) Prelude.. Lens.mapping Lens.coerced

-- | The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
getAssetPropertyAggregates_timeOrdering :: Lens.Lens' GetAssetPropertyAggregates (Prelude.Maybe TimeOrdering)
getAssetPropertyAggregates_timeOrdering = Lens.lens (\GetAssetPropertyAggregates' {timeOrdering} -> timeOrdering) (\s@GetAssetPropertyAggregates' {} a -> s {timeOrdering = a} :: GetAssetPropertyAggregates)

-- | The data aggregating function.
getAssetPropertyAggregates_aggregateTypes :: Lens.Lens' GetAssetPropertyAggregates (Prelude.NonEmpty AggregateType)
getAssetPropertyAggregates_aggregateTypes = Lens.lens (\GetAssetPropertyAggregates' {aggregateTypes} -> aggregateTypes) (\s@GetAssetPropertyAggregates' {} a -> s {aggregateTypes = a} :: GetAssetPropertyAggregates) Prelude.. Lens.coerced

-- | The time interval over which to aggregate data.
getAssetPropertyAggregates_resolution :: Lens.Lens' GetAssetPropertyAggregates Prelude.Text
getAssetPropertyAggregates_resolution = Lens.lens (\GetAssetPropertyAggregates' {resolution} -> resolution) (\s@GetAssetPropertyAggregates' {} a -> s {resolution = a} :: GetAssetPropertyAggregates)

-- | The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
getAssetPropertyAggregates_startDate :: Lens.Lens' GetAssetPropertyAggregates Prelude.UTCTime
getAssetPropertyAggregates_startDate = Lens.lens (\GetAssetPropertyAggregates' {startDate} -> startDate) (\s@GetAssetPropertyAggregates' {} a -> s {startDate = a} :: GetAssetPropertyAggregates) Prelude.. Data._Time

-- | The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
getAssetPropertyAggregates_endDate :: Lens.Lens' GetAssetPropertyAggregates Prelude.UTCTime
getAssetPropertyAggregates_endDate = Lens.lens (\GetAssetPropertyAggregates' {endDate} -> endDate) (\s@GetAssetPropertyAggregates' {} a -> s {endDate = a} :: GetAssetPropertyAggregates) Prelude.. Data._Time

instance Core.AWSPager GetAssetPropertyAggregates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAssetPropertyAggregatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getAssetPropertyAggregatesResponse_aggregatedValues
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getAssetPropertyAggregates_nextToken
          Lens..~ rs
          Lens.^? getAssetPropertyAggregatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetAssetPropertyAggregates where
  type
    AWSResponse GetAssetPropertyAggregates =
      GetAssetPropertyAggregatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssetPropertyAggregatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "aggregatedValues"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetAssetPropertyAggregates where
  hashWithSalt _salt GetAssetPropertyAggregates' {..} =
    _salt `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` propertyAlias
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` qualities
      `Prelude.hashWithSalt` timeOrdering
      `Prelude.hashWithSalt` aggregateTypes
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` endDate

instance Prelude.NFData GetAssetPropertyAggregates where
  rnf GetAssetPropertyAggregates' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf qualities
      `Prelude.seq` Prelude.rnf timeOrdering
      `Prelude.seq` Prelude.rnf aggregateTypes
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf endDate

instance Data.ToHeaders GetAssetPropertyAggregates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAssetPropertyAggregates where
  toPath = Prelude.const "/properties/aggregates"

instance Data.ToQuery GetAssetPropertyAggregates where
  toQuery GetAssetPropertyAggregates' {..} =
    Prelude.mconcat
      [ "assetId" Data.=: assetId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "propertyAlias" Data.=: propertyAlias,
        "propertyId" Data.=: propertyId,
        "qualities"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> qualities),
        "timeOrdering" Data.=: timeOrdering,
        "aggregateTypes"
          Data.=: Data.toQueryList "member" aggregateTypes,
        "resolution" Data.=: resolution,
        "startDate" Data.=: startDate,
        "endDate" Data.=: endDate
      ]

-- | /See:/ 'newGetAssetPropertyAggregatesResponse' smart constructor.
data GetAssetPropertyAggregatesResponse = GetAssetPropertyAggregatesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The requested aggregated values.
    aggregatedValues :: [AggregatedValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyAggregatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAssetPropertyAggregatesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'getAssetPropertyAggregatesResponse_httpStatus' - The response's http status code.
--
-- 'aggregatedValues', 'getAssetPropertyAggregatesResponse_aggregatedValues' - The requested aggregated values.
newGetAssetPropertyAggregatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssetPropertyAggregatesResponse
newGetAssetPropertyAggregatesResponse pHttpStatus_ =
  GetAssetPropertyAggregatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      aggregatedValues = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
getAssetPropertyAggregatesResponse_nextToken :: Lens.Lens' GetAssetPropertyAggregatesResponse (Prelude.Maybe Prelude.Text)
getAssetPropertyAggregatesResponse_nextToken = Lens.lens (\GetAssetPropertyAggregatesResponse' {nextToken} -> nextToken) (\s@GetAssetPropertyAggregatesResponse' {} a -> s {nextToken = a} :: GetAssetPropertyAggregatesResponse)

-- | The response's http status code.
getAssetPropertyAggregatesResponse_httpStatus :: Lens.Lens' GetAssetPropertyAggregatesResponse Prelude.Int
getAssetPropertyAggregatesResponse_httpStatus = Lens.lens (\GetAssetPropertyAggregatesResponse' {httpStatus} -> httpStatus) (\s@GetAssetPropertyAggregatesResponse' {} a -> s {httpStatus = a} :: GetAssetPropertyAggregatesResponse)

-- | The requested aggregated values.
getAssetPropertyAggregatesResponse_aggregatedValues :: Lens.Lens' GetAssetPropertyAggregatesResponse [AggregatedValue]
getAssetPropertyAggregatesResponse_aggregatedValues = Lens.lens (\GetAssetPropertyAggregatesResponse' {aggregatedValues} -> aggregatedValues) (\s@GetAssetPropertyAggregatesResponse' {} a -> s {aggregatedValues = a} :: GetAssetPropertyAggregatesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetAssetPropertyAggregatesResponse
  where
  rnf GetAssetPropertyAggregatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aggregatedValues
