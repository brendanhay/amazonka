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
-- Module      : Amazonka.IoTSiteWise.GetInterpolatedAssetPropertyValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get interpolated values for an asset property for a specified time
-- interval, during a period of time. If your time series is missing data
-- points during the specified time interval, you can use interpolation to
-- estimate the missing data.
--
-- For example, you can use this operation to return the interpolated
-- temperature values for a wind turbine every 24 hours over a duration of
-- 7 days.
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
module Amazonka.IoTSiteWise.GetInterpolatedAssetPropertyValues
  ( -- * Creating a Request
    GetInterpolatedAssetPropertyValues (..),
    newGetInterpolatedAssetPropertyValues,

    -- * Request Lenses
    getInterpolatedAssetPropertyValues_assetId,
    getInterpolatedAssetPropertyValues_endTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_intervalWindowInSeconds,
    getInterpolatedAssetPropertyValues_maxResults,
    getInterpolatedAssetPropertyValues_nextToken,
    getInterpolatedAssetPropertyValues_propertyAlias,
    getInterpolatedAssetPropertyValues_propertyId,
    getInterpolatedAssetPropertyValues_startTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_startTimeInSeconds,
    getInterpolatedAssetPropertyValues_endTimeInSeconds,
    getInterpolatedAssetPropertyValues_quality,
    getInterpolatedAssetPropertyValues_intervalInSeconds,
    getInterpolatedAssetPropertyValues_type,

    -- * Destructuring the Response
    GetInterpolatedAssetPropertyValuesResponse (..),
    newGetInterpolatedAssetPropertyValuesResponse,

    -- * Response Lenses
    getInterpolatedAssetPropertyValuesResponse_nextToken,
    getInterpolatedAssetPropertyValuesResponse_httpStatus,
    getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInterpolatedAssetPropertyValues' smart constructor.
data GetInterpolatedAssetPropertyValues = GetInterpolatedAssetPropertyValues'
  { -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The nanosecond offset converted from @endTimeInSeconds@.
    endTimeOffsetInNanos :: Prelude.Maybe Prelude.Natural,
    -- | The query interval for the window, in seconds. IoT SiteWise computes
    -- each interpolated value by using data points from the timestamp of each
    -- interval, minus the window to the timestamp of each interval plus the
    -- window. If not specified, the window ranges between the start time minus
    -- the interval and the end time plus the interval.
    --
    -- -   If you specify a value for the @intervalWindowInSeconds@ parameter,
    --     the value for the @type@ parameter must be @LINEAR_INTERPOLATION@.
    --
    -- -   If a data point isn\'t found during the specified query window, IoT
    --     SiteWise won\'t return an interpolated value for the interval. This
    --     indicates that there\'s a gap in the ingested data points.
    --
    -- For example, you can get the interpolated temperature values for a wind
    -- turbine every 24 hours over a duration of 7 days. If the interpolation
    -- starts on July 1, 2021, at 9 AM with a window of 2 hours, IoT SiteWise
    -- uses the data points from 7 AM (9 AM minus 2 hours) to 11 AM (9 AM plus
    -- 2 hours) on July 2, 2021 to compute the first interpolated value. Next,
    -- IoT SiteWise uses the data points from 7 AM (9 AM minus 2 hours) to 11
    -- AM (9 AM plus 2 hours) on July 3, 2021 to compute the second
    -- interpolated value, and so on.
    intervalWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of results to return for each paginated request. If
    -- not specified, the default value is 10.
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
    -- | The nanosecond offset converted from @startTimeInSeconds@.
    startTimeOffsetInNanos :: Prelude.Maybe Prelude.Natural,
    -- | The exclusive start of the range from which to interpolate data,
    -- expressed in seconds in Unix epoch time.
    startTimeInSeconds :: Prelude.Natural,
    -- | The inclusive end of the range from which to interpolate data, expressed
    -- in seconds in Unix epoch time.
    endTimeInSeconds :: Prelude.Natural,
    -- | The quality of the asset property value. You can use this parameter as a
    -- filter to choose only the asset property values that have a specific
    -- quality.
    quality :: Quality,
    -- | The time interval in seconds over which to interpolate data. Each
    -- interval starts when the previous one ends.
    intervalInSeconds :: Prelude.Natural,
    -- | The interpolation type.
    --
    -- Valid values: @LINEAR_INTERPOLATION | LOCF_INTERPOLATION@
    --
    -- -   @LINEAR_INTERPOLATION@ – Estimates missing data using
    --     <https://en.wikipedia.org/wiki/Linear_interpolation linear interpolation>.
    --
    --     For example, you can use this operation to return the interpolated
    --     temperature values for a wind turbine every 24 hours over a duration
    --     of 7 days. If the interpolation starts July 1, 2021, at 9 AM, IoT
    --     SiteWise returns the first interpolated value on July 2, 2021, at 9
    --     AM, the second interpolated value on July 3, 2021, at 9 AM, and so
    --     on.
    --
    -- -   @LOCF_INTERPOLATION@ – Estimates missing data using last observation
    --     carried forward interpolation
    --
    --     If no data point is found for an interval, IoT SiteWise returns the
    --     last observed data point for the previous interval and carries
    --     forward this interpolated value until a new data point is found.
    --
    --     For example, you can get the state of an on-off valve every 24 hours
    --     over a duration of 7 days. If the interpolation starts July 1, 2021,
    --     at 9 AM, IoT SiteWise returns the last observed data point between
    --     July 1, 2021, at 9 AM and July 2, 2021, at 9 AM as the first
    --     interpolated value. If a data point isn\'t found after 9 AM on July
    --     2, 2021, IoT SiteWise uses the same interpolated value for the rest
    --     of the days.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInterpolatedAssetPropertyValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'getInterpolatedAssetPropertyValues_assetId' - The ID of the asset.
--
-- 'endTimeOffsetInNanos', 'getInterpolatedAssetPropertyValues_endTimeOffsetInNanos' - The nanosecond offset converted from @endTimeInSeconds@.
--
-- 'intervalWindowInSeconds', 'getInterpolatedAssetPropertyValues_intervalWindowInSeconds' - The query interval for the window, in seconds. IoT SiteWise computes
-- each interpolated value by using data points from the timestamp of each
-- interval, minus the window to the timestamp of each interval plus the
-- window. If not specified, the window ranges between the start time minus
-- the interval and the end time plus the interval.
--
-- -   If you specify a value for the @intervalWindowInSeconds@ parameter,
--     the value for the @type@ parameter must be @LINEAR_INTERPOLATION@.
--
-- -   If a data point isn\'t found during the specified query window, IoT
--     SiteWise won\'t return an interpolated value for the interval. This
--     indicates that there\'s a gap in the ingested data points.
--
-- For example, you can get the interpolated temperature values for a wind
-- turbine every 24 hours over a duration of 7 days. If the interpolation
-- starts on July 1, 2021, at 9 AM with a window of 2 hours, IoT SiteWise
-- uses the data points from 7 AM (9 AM minus 2 hours) to 11 AM (9 AM plus
-- 2 hours) on July 2, 2021 to compute the first interpolated value. Next,
-- IoT SiteWise uses the data points from 7 AM (9 AM minus 2 hours) to 11
-- AM (9 AM plus 2 hours) on July 3, 2021 to compute the second
-- interpolated value, and so on.
--
-- 'maxResults', 'getInterpolatedAssetPropertyValues_maxResults' - The maximum number of results to return for each paginated request. If
-- not specified, the default value is 10.
--
-- 'nextToken', 'getInterpolatedAssetPropertyValues_nextToken' - The token to be used for the next set of paginated results.
--
-- 'propertyAlias', 'getInterpolatedAssetPropertyValues_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'propertyId', 'getInterpolatedAssetPropertyValues_propertyId' - The ID of the asset property.
--
-- 'startTimeOffsetInNanos', 'getInterpolatedAssetPropertyValues_startTimeOffsetInNanos' - The nanosecond offset converted from @startTimeInSeconds@.
--
-- 'startTimeInSeconds', 'getInterpolatedAssetPropertyValues_startTimeInSeconds' - The exclusive start of the range from which to interpolate data,
-- expressed in seconds in Unix epoch time.
--
-- 'endTimeInSeconds', 'getInterpolatedAssetPropertyValues_endTimeInSeconds' - The inclusive end of the range from which to interpolate data, expressed
-- in seconds in Unix epoch time.
--
-- 'quality', 'getInterpolatedAssetPropertyValues_quality' - The quality of the asset property value. You can use this parameter as a
-- filter to choose only the asset property values that have a specific
-- quality.
--
-- 'intervalInSeconds', 'getInterpolatedAssetPropertyValues_intervalInSeconds' - The time interval in seconds over which to interpolate data. Each
-- interval starts when the previous one ends.
--
-- 'type'', 'getInterpolatedAssetPropertyValues_type' - The interpolation type.
--
-- Valid values: @LINEAR_INTERPOLATION | LOCF_INTERPOLATION@
--
-- -   @LINEAR_INTERPOLATION@ – Estimates missing data using
--     <https://en.wikipedia.org/wiki/Linear_interpolation linear interpolation>.
--
--     For example, you can use this operation to return the interpolated
--     temperature values for a wind turbine every 24 hours over a duration
--     of 7 days. If the interpolation starts July 1, 2021, at 9 AM, IoT
--     SiteWise returns the first interpolated value on July 2, 2021, at 9
--     AM, the second interpolated value on July 3, 2021, at 9 AM, and so
--     on.
--
-- -   @LOCF_INTERPOLATION@ – Estimates missing data using last observation
--     carried forward interpolation
--
--     If no data point is found for an interval, IoT SiteWise returns the
--     last observed data point for the previous interval and carries
--     forward this interpolated value until a new data point is found.
--
--     For example, you can get the state of an on-off valve every 24 hours
--     over a duration of 7 days. If the interpolation starts July 1, 2021,
--     at 9 AM, IoT SiteWise returns the last observed data point between
--     July 1, 2021, at 9 AM and July 2, 2021, at 9 AM as the first
--     interpolated value. If a data point isn\'t found after 9 AM on July
--     2, 2021, IoT SiteWise uses the same interpolated value for the rest
--     of the days.
newGetInterpolatedAssetPropertyValues ::
  -- | 'startTimeInSeconds'
  Prelude.Natural ->
  -- | 'endTimeInSeconds'
  Prelude.Natural ->
  -- | 'quality'
  Quality ->
  -- | 'intervalInSeconds'
  Prelude.Natural ->
  -- | 'type''
  Prelude.Text ->
  GetInterpolatedAssetPropertyValues
newGetInterpolatedAssetPropertyValues
  pStartTimeInSeconds_
  pEndTimeInSeconds_
  pQuality_
  pIntervalInSeconds_
  pType_ =
    GetInterpolatedAssetPropertyValues'
      { assetId =
          Prelude.Nothing,
        endTimeOffsetInNanos = Prelude.Nothing,
        intervalWindowInSeconds =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        propertyAlias = Prelude.Nothing,
        propertyId = Prelude.Nothing,
        startTimeOffsetInNanos =
          Prelude.Nothing,
        startTimeInSeconds =
          pStartTimeInSeconds_,
        endTimeInSeconds = pEndTimeInSeconds_,
        quality = pQuality_,
        intervalInSeconds = pIntervalInSeconds_,
        type' = pType_
      }

-- | The ID of the asset.
getInterpolatedAssetPropertyValues_assetId :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Text)
getInterpolatedAssetPropertyValues_assetId = Lens.lens (\GetInterpolatedAssetPropertyValues' {assetId} -> assetId) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {assetId = a} :: GetInterpolatedAssetPropertyValues)

-- | The nanosecond offset converted from @endTimeInSeconds@.
getInterpolatedAssetPropertyValues_endTimeOffsetInNanos :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Natural)
getInterpolatedAssetPropertyValues_endTimeOffsetInNanos = Lens.lens (\GetInterpolatedAssetPropertyValues' {endTimeOffsetInNanos} -> endTimeOffsetInNanos) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {endTimeOffsetInNanos = a} :: GetInterpolatedAssetPropertyValues)

-- | The query interval for the window, in seconds. IoT SiteWise computes
-- each interpolated value by using data points from the timestamp of each
-- interval, minus the window to the timestamp of each interval plus the
-- window. If not specified, the window ranges between the start time minus
-- the interval and the end time plus the interval.
--
-- -   If you specify a value for the @intervalWindowInSeconds@ parameter,
--     the value for the @type@ parameter must be @LINEAR_INTERPOLATION@.
--
-- -   If a data point isn\'t found during the specified query window, IoT
--     SiteWise won\'t return an interpolated value for the interval. This
--     indicates that there\'s a gap in the ingested data points.
--
-- For example, you can get the interpolated temperature values for a wind
-- turbine every 24 hours over a duration of 7 days. If the interpolation
-- starts on July 1, 2021, at 9 AM with a window of 2 hours, IoT SiteWise
-- uses the data points from 7 AM (9 AM minus 2 hours) to 11 AM (9 AM plus
-- 2 hours) on July 2, 2021 to compute the first interpolated value. Next,
-- IoT SiteWise uses the data points from 7 AM (9 AM minus 2 hours) to 11
-- AM (9 AM plus 2 hours) on July 3, 2021 to compute the second
-- interpolated value, and so on.
getInterpolatedAssetPropertyValues_intervalWindowInSeconds :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Natural)
getInterpolatedAssetPropertyValues_intervalWindowInSeconds = Lens.lens (\GetInterpolatedAssetPropertyValues' {intervalWindowInSeconds} -> intervalWindowInSeconds) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {intervalWindowInSeconds = a} :: GetInterpolatedAssetPropertyValues)

-- | The maximum number of results to return for each paginated request. If
-- not specified, the default value is 10.
getInterpolatedAssetPropertyValues_maxResults :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Natural)
getInterpolatedAssetPropertyValues_maxResults = Lens.lens (\GetInterpolatedAssetPropertyValues' {maxResults} -> maxResults) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {maxResults = a} :: GetInterpolatedAssetPropertyValues)

-- | The token to be used for the next set of paginated results.
getInterpolatedAssetPropertyValues_nextToken :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Text)
getInterpolatedAssetPropertyValues_nextToken = Lens.lens (\GetInterpolatedAssetPropertyValues' {nextToken} -> nextToken) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {nextToken = a} :: GetInterpolatedAssetPropertyValues)

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
getInterpolatedAssetPropertyValues_propertyAlias :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Text)
getInterpolatedAssetPropertyValues_propertyAlias = Lens.lens (\GetInterpolatedAssetPropertyValues' {propertyAlias} -> propertyAlias) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {propertyAlias = a} :: GetInterpolatedAssetPropertyValues)

-- | The ID of the asset property.
getInterpolatedAssetPropertyValues_propertyId :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Text)
getInterpolatedAssetPropertyValues_propertyId = Lens.lens (\GetInterpolatedAssetPropertyValues' {propertyId} -> propertyId) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {propertyId = a} :: GetInterpolatedAssetPropertyValues)

-- | The nanosecond offset converted from @startTimeInSeconds@.
getInterpolatedAssetPropertyValues_startTimeOffsetInNanos :: Lens.Lens' GetInterpolatedAssetPropertyValues (Prelude.Maybe Prelude.Natural)
getInterpolatedAssetPropertyValues_startTimeOffsetInNanos = Lens.lens (\GetInterpolatedAssetPropertyValues' {startTimeOffsetInNanos} -> startTimeOffsetInNanos) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {startTimeOffsetInNanos = a} :: GetInterpolatedAssetPropertyValues)

-- | The exclusive start of the range from which to interpolate data,
-- expressed in seconds in Unix epoch time.
getInterpolatedAssetPropertyValues_startTimeInSeconds :: Lens.Lens' GetInterpolatedAssetPropertyValues Prelude.Natural
getInterpolatedAssetPropertyValues_startTimeInSeconds = Lens.lens (\GetInterpolatedAssetPropertyValues' {startTimeInSeconds} -> startTimeInSeconds) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {startTimeInSeconds = a} :: GetInterpolatedAssetPropertyValues)

-- | The inclusive end of the range from which to interpolate data, expressed
-- in seconds in Unix epoch time.
getInterpolatedAssetPropertyValues_endTimeInSeconds :: Lens.Lens' GetInterpolatedAssetPropertyValues Prelude.Natural
getInterpolatedAssetPropertyValues_endTimeInSeconds = Lens.lens (\GetInterpolatedAssetPropertyValues' {endTimeInSeconds} -> endTimeInSeconds) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {endTimeInSeconds = a} :: GetInterpolatedAssetPropertyValues)

-- | The quality of the asset property value. You can use this parameter as a
-- filter to choose only the asset property values that have a specific
-- quality.
getInterpolatedAssetPropertyValues_quality :: Lens.Lens' GetInterpolatedAssetPropertyValues Quality
getInterpolatedAssetPropertyValues_quality = Lens.lens (\GetInterpolatedAssetPropertyValues' {quality} -> quality) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {quality = a} :: GetInterpolatedAssetPropertyValues)

-- | The time interval in seconds over which to interpolate data. Each
-- interval starts when the previous one ends.
getInterpolatedAssetPropertyValues_intervalInSeconds :: Lens.Lens' GetInterpolatedAssetPropertyValues Prelude.Natural
getInterpolatedAssetPropertyValues_intervalInSeconds = Lens.lens (\GetInterpolatedAssetPropertyValues' {intervalInSeconds} -> intervalInSeconds) (\s@GetInterpolatedAssetPropertyValues' {} a -> s {intervalInSeconds = a} :: GetInterpolatedAssetPropertyValues)

-- | The interpolation type.
--
-- Valid values: @LINEAR_INTERPOLATION | LOCF_INTERPOLATION@
--
-- -   @LINEAR_INTERPOLATION@ – Estimates missing data using
--     <https://en.wikipedia.org/wiki/Linear_interpolation linear interpolation>.
--
--     For example, you can use this operation to return the interpolated
--     temperature values for a wind turbine every 24 hours over a duration
--     of 7 days. If the interpolation starts July 1, 2021, at 9 AM, IoT
--     SiteWise returns the first interpolated value on July 2, 2021, at 9
--     AM, the second interpolated value on July 3, 2021, at 9 AM, and so
--     on.
--
-- -   @LOCF_INTERPOLATION@ – Estimates missing data using last observation
--     carried forward interpolation
--
--     If no data point is found for an interval, IoT SiteWise returns the
--     last observed data point for the previous interval and carries
--     forward this interpolated value until a new data point is found.
--
--     For example, you can get the state of an on-off valve every 24 hours
--     over a duration of 7 days. If the interpolation starts July 1, 2021,
--     at 9 AM, IoT SiteWise returns the last observed data point between
--     July 1, 2021, at 9 AM and July 2, 2021, at 9 AM as the first
--     interpolated value. If a data point isn\'t found after 9 AM on July
--     2, 2021, IoT SiteWise uses the same interpolated value for the rest
--     of the days.
getInterpolatedAssetPropertyValues_type :: Lens.Lens' GetInterpolatedAssetPropertyValues Prelude.Text
getInterpolatedAssetPropertyValues_type = Lens.lens (\GetInterpolatedAssetPropertyValues' {type'} -> type') (\s@GetInterpolatedAssetPropertyValues' {} a -> s {type' = a} :: GetInterpolatedAssetPropertyValues)

instance
  Core.AWSPager
    GetInterpolatedAssetPropertyValues
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInterpolatedAssetPropertyValuesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getInterpolatedAssetPropertyValues_nextToken
          Lens..~ rs
          Lens.^? getInterpolatedAssetPropertyValuesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetInterpolatedAssetPropertyValues
  where
  type
    AWSResponse GetInterpolatedAssetPropertyValues =
      GetInterpolatedAssetPropertyValuesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInterpolatedAssetPropertyValuesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "interpolatedAssetPropertyValues"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    GetInterpolatedAssetPropertyValues
  where
  hashWithSalt
    _salt
    GetInterpolatedAssetPropertyValues' {..} =
      _salt
        `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` endTimeOffsetInNanos
        `Prelude.hashWithSalt` intervalWindowInSeconds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` propertyAlias
        `Prelude.hashWithSalt` propertyId
        `Prelude.hashWithSalt` startTimeOffsetInNanos
        `Prelude.hashWithSalt` startTimeInSeconds
        `Prelude.hashWithSalt` endTimeInSeconds
        `Prelude.hashWithSalt` quality
        `Prelude.hashWithSalt` intervalInSeconds
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    GetInterpolatedAssetPropertyValues
  where
  rnf GetInterpolatedAssetPropertyValues' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf endTimeOffsetInNanos
      `Prelude.seq` Prelude.rnf intervalWindowInSeconds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf startTimeOffsetInNanos
      `Prelude.seq` Prelude.rnf startTimeInSeconds
      `Prelude.seq` Prelude.rnf endTimeInSeconds
      `Prelude.seq` Prelude.rnf quality
      `Prelude.seq` Prelude.rnf intervalInSeconds
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToHeaders
    GetInterpolatedAssetPropertyValues
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    GetInterpolatedAssetPropertyValues
  where
  toPath = Prelude.const "/properties/interpolated"

instance
  Data.ToQuery
    GetInterpolatedAssetPropertyValues
  where
  toQuery GetInterpolatedAssetPropertyValues' {..} =
    Prelude.mconcat
      [ "assetId" Data.=: assetId,
        "endTimeOffsetInNanos" Data.=: endTimeOffsetInNanos,
        "intervalWindowInSeconds"
          Data.=: intervalWindowInSeconds,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "propertyAlias" Data.=: propertyAlias,
        "propertyId" Data.=: propertyId,
        "startTimeOffsetInNanos"
          Data.=: startTimeOffsetInNanos,
        "startTimeInSeconds" Data.=: startTimeInSeconds,
        "endTimeInSeconds" Data.=: endTimeInSeconds,
        "quality" Data.=: quality,
        "intervalInSeconds" Data.=: intervalInSeconds,
        "type" Data.=: type'
      ]

-- | /See:/ 'newGetInterpolatedAssetPropertyValuesResponse' smart constructor.
data GetInterpolatedAssetPropertyValuesResponse = GetInterpolatedAssetPropertyValuesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The requested interpolated values.
    interpolatedAssetPropertyValues :: [InterpolatedAssetPropertyValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInterpolatedAssetPropertyValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInterpolatedAssetPropertyValuesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'getInterpolatedAssetPropertyValuesResponse_httpStatus' - The response's http status code.
--
-- 'interpolatedAssetPropertyValues', 'getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues' - The requested interpolated values.
newGetInterpolatedAssetPropertyValuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInterpolatedAssetPropertyValuesResponse
newGetInterpolatedAssetPropertyValuesResponse
  pHttpStatus_ =
    GetInterpolatedAssetPropertyValuesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        interpolatedAssetPropertyValues =
          Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no
-- additional results.
getInterpolatedAssetPropertyValuesResponse_nextToken :: Lens.Lens' GetInterpolatedAssetPropertyValuesResponse (Prelude.Maybe Prelude.Text)
getInterpolatedAssetPropertyValuesResponse_nextToken = Lens.lens (\GetInterpolatedAssetPropertyValuesResponse' {nextToken} -> nextToken) (\s@GetInterpolatedAssetPropertyValuesResponse' {} a -> s {nextToken = a} :: GetInterpolatedAssetPropertyValuesResponse)

-- | The response's http status code.
getInterpolatedAssetPropertyValuesResponse_httpStatus :: Lens.Lens' GetInterpolatedAssetPropertyValuesResponse Prelude.Int
getInterpolatedAssetPropertyValuesResponse_httpStatus = Lens.lens (\GetInterpolatedAssetPropertyValuesResponse' {httpStatus} -> httpStatus) (\s@GetInterpolatedAssetPropertyValuesResponse' {} a -> s {httpStatus = a} :: GetInterpolatedAssetPropertyValuesResponse)

-- | The requested interpolated values.
getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues :: Lens.Lens' GetInterpolatedAssetPropertyValuesResponse [InterpolatedAssetPropertyValue]
getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues = Lens.lens (\GetInterpolatedAssetPropertyValuesResponse' {interpolatedAssetPropertyValues} -> interpolatedAssetPropertyValues) (\s@GetInterpolatedAssetPropertyValuesResponse' {} a -> s {interpolatedAssetPropertyValues = a} :: GetInterpolatedAssetPropertyValuesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetInterpolatedAssetPropertyValuesResponse
  where
  rnf GetInterpolatedAssetPropertyValuesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf interpolatedAssetPropertyValues
