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
-- Module      : Amazonka.IoTSiteWise.ListTimeSeries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of time series (data streams).
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListTimeSeries
  ( -- * Creating a Request
    ListTimeSeries (..),
    newListTimeSeries,

    -- * Request Lenses
    listTimeSeries_aliasPrefix,
    listTimeSeries_assetId,
    listTimeSeries_maxResults,
    listTimeSeries_nextToken,
    listTimeSeries_timeSeriesType,

    -- * Destructuring the Response
    ListTimeSeriesResponse (..),
    newListTimeSeriesResponse,

    -- * Response Lenses
    listTimeSeriesResponse_nextToken,
    listTimeSeriesResponse_httpStatus,
    listTimeSeriesResponse_timeSeriesSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTimeSeries' smart constructor.
data ListTimeSeries = ListTimeSeries'
  { -- | The alias prefix of the time series.
    aliasPrefix :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset in which the asset property was created.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of the time series. The time series type can be one of the
    -- following values:
    --
    -- -   @ASSOCIATED@ – The time series is associated with an asset property.
    --
    -- -   @DISASSOCIATED@ – The time series isn\'t associated with any asset
    --     property.
    timeSeriesType :: Prelude.Maybe ListTimeSeriesType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTimeSeries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasPrefix', 'listTimeSeries_aliasPrefix' - The alias prefix of the time series.
--
-- 'assetId', 'listTimeSeries_assetId' - The ID of the asset in which the asset property was created.
--
-- 'maxResults', 'listTimeSeries_maxResults' - The maximum number of results to return for each paginated request.
--
-- 'nextToken', 'listTimeSeries_nextToken' - The token to be used for the next set of paginated results.
--
-- 'timeSeriesType', 'listTimeSeries_timeSeriesType' - The type of the time series. The time series type can be one of the
-- following values:
--
-- -   @ASSOCIATED@ – The time series is associated with an asset property.
--
-- -   @DISASSOCIATED@ – The time series isn\'t associated with any asset
--     property.
newListTimeSeries ::
  ListTimeSeries
newListTimeSeries =
  ListTimeSeries'
    { aliasPrefix = Prelude.Nothing,
      assetId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      timeSeriesType = Prelude.Nothing
    }

-- | The alias prefix of the time series.
listTimeSeries_aliasPrefix :: Lens.Lens' ListTimeSeries (Prelude.Maybe Prelude.Text)
listTimeSeries_aliasPrefix = Lens.lens (\ListTimeSeries' {aliasPrefix} -> aliasPrefix) (\s@ListTimeSeries' {} a -> s {aliasPrefix = a} :: ListTimeSeries)

-- | The ID of the asset in which the asset property was created.
listTimeSeries_assetId :: Lens.Lens' ListTimeSeries (Prelude.Maybe Prelude.Text)
listTimeSeries_assetId = Lens.lens (\ListTimeSeries' {assetId} -> assetId) (\s@ListTimeSeries' {} a -> s {assetId = a} :: ListTimeSeries)

-- | The maximum number of results to return for each paginated request.
listTimeSeries_maxResults :: Lens.Lens' ListTimeSeries (Prelude.Maybe Prelude.Natural)
listTimeSeries_maxResults = Lens.lens (\ListTimeSeries' {maxResults} -> maxResults) (\s@ListTimeSeries' {} a -> s {maxResults = a} :: ListTimeSeries)

-- | The token to be used for the next set of paginated results.
listTimeSeries_nextToken :: Lens.Lens' ListTimeSeries (Prelude.Maybe Prelude.Text)
listTimeSeries_nextToken = Lens.lens (\ListTimeSeries' {nextToken} -> nextToken) (\s@ListTimeSeries' {} a -> s {nextToken = a} :: ListTimeSeries)

-- | The type of the time series. The time series type can be one of the
-- following values:
--
-- -   @ASSOCIATED@ – The time series is associated with an asset property.
--
-- -   @DISASSOCIATED@ – The time series isn\'t associated with any asset
--     property.
listTimeSeries_timeSeriesType :: Lens.Lens' ListTimeSeries (Prelude.Maybe ListTimeSeriesType)
listTimeSeries_timeSeriesType = Lens.lens (\ListTimeSeries' {timeSeriesType} -> timeSeriesType) (\s@ListTimeSeries' {} a -> s {timeSeriesType = a} :: ListTimeSeries)

instance Core.AWSPager ListTimeSeries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTimeSeriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTimeSeriesResponse_timeSeriesSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTimeSeries_nextToken
          Lens..~ rs
          Lens.^? listTimeSeriesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTimeSeries where
  type
    AWSResponse ListTimeSeries =
      ListTimeSeriesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTimeSeriesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "TimeSeriesSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTimeSeries where
  hashWithSalt _salt ListTimeSeries' {..} =
    _salt `Prelude.hashWithSalt` aliasPrefix
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` timeSeriesType

instance Prelude.NFData ListTimeSeries where
  rnf ListTimeSeries' {..} =
    Prelude.rnf aliasPrefix
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeSeriesType

instance Data.ToHeaders ListTimeSeries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTimeSeries where
  toPath = Prelude.const "/timeseries/"

instance Data.ToQuery ListTimeSeries where
  toQuery ListTimeSeries' {..} =
    Prelude.mconcat
      [ "aliasPrefix" Data.=: aliasPrefix,
        "assetId" Data.=: assetId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "timeSeriesType" Data.=: timeSeriesType
      ]

-- | /See:/ 'newListTimeSeriesResponse' smart constructor.
data ListTimeSeriesResponse = ListTimeSeriesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | One or more time series summaries to list.
    timeSeriesSummaries :: [TimeSeriesSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTimeSeriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTimeSeriesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listTimeSeriesResponse_httpStatus' - The response's http status code.
--
-- 'timeSeriesSummaries', 'listTimeSeriesResponse_timeSeriesSummaries' - One or more time series summaries to list.
newListTimeSeriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTimeSeriesResponse
newListTimeSeriesResponse pHttpStatus_ =
  ListTimeSeriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      timeSeriesSummaries = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listTimeSeriesResponse_nextToken :: Lens.Lens' ListTimeSeriesResponse (Prelude.Maybe Prelude.Text)
listTimeSeriesResponse_nextToken = Lens.lens (\ListTimeSeriesResponse' {nextToken} -> nextToken) (\s@ListTimeSeriesResponse' {} a -> s {nextToken = a} :: ListTimeSeriesResponse)

-- | The response's http status code.
listTimeSeriesResponse_httpStatus :: Lens.Lens' ListTimeSeriesResponse Prelude.Int
listTimeSeriesResponse_httpStatus = Lens.lens (\ListTimeSeriesResponse' {httpStatus} -> httpStatus) (\s@ListTimeSeriesResponse' {} a -> s {httpStatus = a} :: ListTimeSeriesResponse)

-- | One or more time series summaries to list.
listTimeSeriesResponse_timeSeriesSummaries :: Lens.Lens' ListTimeSeriesResponse [TimeSeriesSummary]
listTimeSeriesResponse_timeSeriesSummaries = Lens.lens (\ListTimeSeriesResponse' {timeSeriesSummaries} -> timeSeriesSummaries) (\s@ListTimeSeriesResponse' {} a -> s {timeSeriesSummaries = a} :: ListTimeSeriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTimeSeriesResponse where
  rnf ListTimeSeriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf timeSeriesSummaries
