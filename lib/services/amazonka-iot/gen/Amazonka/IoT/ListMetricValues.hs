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
-- Module      : Amazonka.IoT.ListMetricValues
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the values reported for an IoT Device Defender metric (device-side
-- metric, cloud-side metric, or custom metric) by the given thing during
-- the specified time period.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListMetricValues
  ( -- * Creating a Request
    ListMetricValues (..),
    newListMetricValues,

    -- * Request Lenses
    listMetricValues_dimensionName,
    listMetricValues_dimensionValueOperator,
    listMetricValues_maxResults,
    listMetricValues_nextToken,
    listMetricValues_thingName,
    listMetricValues_metricName,
    listMetricValues_startTime,
    listMetricValues_endTime,

    -- * Destructuring the Response
    ListMetricValuesResponse (..),
    newListMetricValuesResponse,

    -- * Response Lenses
    listMetricValuesResponse_metricDatumList,
    listMetricValuesResponse_nextToken,
    listMetricValuesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetricValues' smart constructor.
data ListMetricValues = ListMetricValues'
  { -- | The dimension name.
    dimensionName :: Prelude.Maybe Prelude.Text,
    -- | The dimension value operator.
    dimensionValueOperator :: Prelude.Maybe DimensionValueOperator,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing for which security profile metric values are
    -- returned.
    thingName :: Prelude.Text,
    -- | The name of the security profile metric for which values are returned.
    metricName :: Prelude.Text,
    -- | The start of the time period for which metric values are returned.
    startTime :: Data.POSIX,
    -- | The end of the time period for which metric values are returned.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionName', 'listMetricValues_dimensionName' - The dimension name.
--
-- 'dimensionValueOperator', 'listMetricValues_dimensionValueOperator' - The dimension value operator.
--
-- 'maxResults', 'listMetricValues_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listMetricValues_nextToken' - The token for the next set of results.
--
-- 'thingName', 'listMetricValues_thingName' - The name of the thing for which security profile metric values are
-- returned.
--
-- 'metricName', 'listMetricValues_metricName' - The name of the security profile metric for which values are returned.
--
-- 'startTime', 'listMetricValues_startTime' - The start of the time period for which metric values are returned.
--
-- 'endTime', 'listMetricValues_endTime' - The end of the time period for which metric values are returned.
newListMetricValues ::
  -- | 'thingName'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ListMetricValues
newListMetricValues
  pThingName_
  pMetricName_
  pStartTime_
  pEndTime_ =
    ListMetricValues'
      { dimensionName = Prelude.Nothing,
        dimensionValueOperator = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        thingName = pThingName_,
        metricName = pMetricName_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | The dimension name.
listMetricValues_dimensionName :: Lens.Lens' ListMetricValues (Prelude.Maybe Prelude.Text)
listMetricValues_dimensionName = Lens.lens (\ListMetricValues' {dimensionName} -> dimensionName) (\s@ListMetricValues' {} a -> s {dimensionName = a} :: ListMetricValues)

-- | The dimension value operator.
listMetricValues_dimensionValueOperator :: Lens.Lens' ListMetricValues (Prelude.Maybe DimensionValueOperator)
listMetricValues_dimensionValueOperator = Lens.lens (\ListMetricValues' {dimensionValueOperator} -> dimensionValueOperator) (\s@ListMetricValues' {} a -> s {dimensionValueOperator = a} :: ListMetricValues)

-- | The maximum number of results to return at one time.
listMetricValues_maxResults :: Lens.Lens' ListMetricValues (Prelude.Maybe Prelude.Natural)
listMetricValues_maxResults = Lens.lens (\ListMetricValues' {maxResults} -> maxResults) (\s@ListMetricValues' {} a -> s {maxResults = a} :: ListMetricValues)

-- | The token for the next set of results.
listMetricValues_nextToken :: Lens.Lens' ListMetricValues (Prelude.Maybe Prelude.Text)
listMetricValues_nextToken = Lens.lens (\ListMetricValues' {nextToken} -> nextToken) (\s@ListMetricValues' {} a -> s {nextToken = a} :: ListMetricValues)

-- | The name of the thing for which security profile metric values are
-- returned.
listMetricValues_thingName :: Lens.Lens' ListMetricValues Prelude.Text
listMetricValues_thingName = Lens.lens (\ListMetricValues' {thingName} -> thingName) (\s@ListMetricValues' {} a -> s {thingName = a} :: ListMetricValues)

-- | The name of the security profile metric for which values are returned.
listMetricValues_metricName :: Lens.Lens' ListMetricValues Prelude.Text
listMetricValues_metricName = Lens.lens (\ListMetricValues' {metricName} -> metricName) (\s@ListMetricValues' {} a -> s {metricName = a} :: ListMetricValues)

-- | The start of the time period for which metric values are returned.
listMetricValues_startTime :: Lens.Lens' ListMetricValues Prelude.UTCTime
listMetricValues_startTime = Lens.lens (\ListMetricValues' {startTime} -> startTime) (\s@ListMetricValues' {} a -> s {startTime = a} :: ListMetricValues) Prelude.. Data._Time

-- | The end of the time period for which metric values are returned.
listMetricValues_endTime :: Lens.Lens' ListMetricValues Prelude.UTCTime
listMetricValues_endTime = Lens.lens (\ListMetricValues' {endTime} -> endTime) (\s@ListMetricValues' {} a -> s {endTime = a} :: ListMetricValues) Prelude.. Data._Time

instance Core.AWSPager ListMetricValues where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMetricValuesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMetricValuesResponse_metricDatumList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMetricValues_nextToken
          Lens..~ rs
          Lens.^? listMetricValuesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMetricValues where
  type
    AWSResponse ListMetricValues =
      ListMetricValuesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMetricValuesResponse'
            Prelude.<$> ( x Data..?> "metricDatumList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetricValues where
  hashWithSalt _salt ListMetricValues' {..} =
    _salt `Prelude.hashWithSalt` dimensionName
      `Prelude.hashWithSalt` dimensionValueOperator
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData ListMetricValues where
  rnf ListMetricValues' {..} =
    Prelude.rnf dimensionName
      `Prelude.seq` Prelude.rnf dimensionValueOperator
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders ListMetricValues where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMetricValues where
  toPath = Prelude.const "/metric-values"

instance Data.ToQuery ListMetricValues where
  toQuery ListMetricValues' {..} =
    Prelude.mconcat
      [ "dimensionName" Data.=: dimensionName,
        "dimensionValueOperator"
          Data.=: dimensionValueOperator,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "thingName" Data.=: thingName,
        "metricName" Data.=: metricName,
        "startTime" Data.=: startTime,
        "endTime" Data.=: endTime
      ]

-- | /See:/ 'newListMetricValuesResponse' smart constructor.
data ListMetricValuesResponse = ListMetricValuesResponse'
  { -- | The data the thing reports for the metric during the specified time
    -- period.
    metricDatumList :: Prelude.Maybe [MetricDatum],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricValuesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricDatumList', 'listMetricValuesResponse_metricDatumList' - The data the thing reports for the metric during the specified time
-- period.
--
-- 'nextToken', 'listMetricValuesResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'httpStatus', 'listMetricValuesResponse_httpStatus' - The response's http status code.
newListMetricValuesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricValuesResponse
newListMetricValuesResponse pHttpStatus_ =
  ListMetricValuesResponse'
    { metricDatumList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The data the thing reports for the metric during the specified time
-- period.
listMetricValuesResponse_metricDatumList :: Lens.Lens' ListMetricValuesResponse (Prelude.Maybe [MetricDatum])
listMetricValuesResponse_metricDatumList = Lens.lens (\ListMetricValuesResponse' {metricDatumList} -> metricDatumList) (\s@ListMetricValuesResponse' {} a -> s {metricDatumList = a} :: ListMetricValuesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listMetricValuesResponse_nextToken :: Lens.Lens' ListMetricValuesResponse (Prelude.Maybe Prelude.Text)
listMetricValuesResponse_nextToken = Lens.lens (\ListMetricValuesResponse' {nextToken} -> nextToken) (\s@ListMetricValuesResponse' {} a -> s {nextToken = a} :: ListMetricValuesResponse)

-- | The response's http status code.
listMetricValuesResponse_httpStatus :: Lens.Lens' ListMetricValuesResponse Prelude.Int
listMetricValuesResponse_httpStatus = Lens.lens (\ListMetricValuesResponse' {httpStatus} -> httpStatus) (\s@ListMetricValuesResponse' {} a -> s {httpStatus = a} :: ListMetricValuesResponse)

instance Prelude.NFData ListMetricValuesResponse where
  rnf ListMetricValuesResponse' {..} =
    Prelude.rnf metricDatumList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
