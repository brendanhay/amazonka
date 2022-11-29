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
-- Module      : Amazonka.IoTSiteWise.GetAssetPropertyValueHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the history of an asset property\'s values. For more information,
-- see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#historical-values Querying historical values>
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
module Amazonka.IoTSiteWise.GetAssetPropertyValueHistory
  ( -- * Creating a Request
    GetAssetPropertyValueHistory (..),
    newGetAssetPropertyValueHistory,

    -- * Request Lenses
    getAssetPropertyValueHistory_nextToken,
    getAssetPropertyValueHistory_propertyAlias,
    getAssetPropertyValueHistory_assetId,
    getAssetPropertyValueHistory_endDate,
    getAssetPropertyValueHistory_qualities,
    getAssetPropertyValueHistory_propertyId,
    getAssetPropertyValueHistory_startDate,
    getAssetPropertyValueHistory_maxResults,
    getAssetPropertyValueHistory_timeOrdering,

    -- * Destructuring the Response
    GetAssetPropertyValueHistoryResponse (..),
    newGetAssetPropertyValueHistoryResponse,

    -- * Response Lenses
    getAssetPropertyValueHistoryResponse_nextToken,
    getAssetPropertyValueHistoryResponse_httpStatus,
    getAssetPropertyValueHistoryResponse_assetPropertyValueHistory,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssetPropertyValueHistory' smart constructor.
data GetAssetPropertyValueHistory = GetAssetPropertyValueHistory'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The inclusive end of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    endDate :: Prelude.Maybe Core.POSIX,
    -- | The quality by which to filter asset data.
    qualities :: Prelude.Maybe (Prelude.NonEmpty Quality),
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The exclusive start of the range from which to query historical data,
    -- expressed in seconds in Unix epoch time.
    startDate :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 100
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The chronological sorting order of the requested information.
    --
    -- Default: @ASCENDING@
    timeOrdering :: Prelude.Maybe TimeOrdering
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyValueHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAssetPropertyValueHistory_nextToken' - The token to be used for the next set of paginated results.
--
-- 'propertyAlias', 'getAssetPropertyValueHistory_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetId', 'getAssetPropertyValueHistory_assetId' - The ID of the asset.
--
-- 'endDate', 'getAssetPropertyValueHistory_endDate' - The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'qualities', 'getAssetPropertyValueHistory_qualities' - The quality by which to filter asset data.
--
-- 'propertyId', 'getAssetPropertyValueHistory_propertyId' - The ID of the asset property.
--
-- 'startDate', 'getAssetPropertyValueHistory_startDate' - The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
--
-- 'maxResults', 'getAssetPropertyValueHistory_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 100
--
-- 'timeOrdering', 'getAssetPropertyValueHistory_timeOrdering' - The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
newGetAssetPropertyValueHistory ::
  GetAssetPropertyValueHistory
newGetAssetPropertyValueHistory =
  GetAssetPropertyValueHistory'
    { nextToken =
        Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      assetId = Prelude.Nothing,
      endDate = Prelude.Nothing,
      qualities = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      timeOrdering = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results.
getAssetPropertyValueHistory_nextToken :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getAssetPropertyValueHistory_nextToken = Lens.lens (\GetAssetPropertyValueHistory' {nextToken} -> nextToken) (\s@GetAssetPropertyValueHistory' {} a -> s {nextToken = a} :: GetAssetPropertyValueHistory)

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
getAssetPropertyValueHistory_propertyAlias :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getAssetPropertyValueHistory_propertyAlias = Lens.lens (\GetAssetPropertyValueHistory' {propertyAlias} -> propertyAlias) (\s@GetAssetPropertyValueHistory' {} a -> s {propertyAlias = a} :: GetAssetPropertyValueHistory)

-- | The ID of the asset.
getAssetPropertyValueHistory_assetId :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getAssetPropertyValueHistory_assetId = Lens.lens (\GetAssetPropertyValueHistory' {assetId} -> assetId) (\s@GetAssetPropertyValueHistory' {} a -> s {assetId = a} :: GetAssetPropertyValueHistory)

-- | The inclusive end of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
getAssetPropertyValueHistory_endDate :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.UTCTime)
getAssetPropertyValueHistory_endDate = Lens.lens (\GetAssetPropertyValueHistory' {endDate} -> endDate) (\s@GetAssetPropertyValueHistory' {} a -> s {endDate = a} :: GetAssetPropertyValueHistory) Prelude.. Lens.mapping Core._Time

-- | The quality by which to filter asset data.
getAssetPropertyValueHistory_qualities :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe (Prelude.NonEmpty Quality))
getAssetPropertyValueHistory_qualities = Lens.lens (\GetAssetPropertyValueHistory' {qualities} -> qualities) (\s@GetAssetPropertyValueHistory' {} a -> s {qualities = a} :: GetAssetPropertyValueHistory) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the asset property.
getAssetPropertyValueHistory_propertyId :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getAssetPropertyValueHistory_propertyId = Lens.lens (\GetAssetPropertyValueHistory' {propertyId} -> propertyId) (\s@GetAssetPropertyValueHistory' {} a -> s {propertyId = a} :: GetAssetPropertyValueHistory)

-- | The exclusive start of the range from which to query historical data,
-- expressed in seconds in Unix epoch time.
getAssetPropertyValueHistory_startDate :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.UTCTime)
getAssetPropertyValueHistory_startDate = Lens.lens (\GetAssetPropertyValueHistory' {startDate} -> startDate) (\s@GetAssetPropertyValueHistory' {} a -> s {startDate = a} :: GetAssetPropertyValueHistory) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return for each paginated request.
--
-- Default: 100
getAssetPropertyValueHistory_maxResults :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe Prelude.Natural)
getAssetPropertyValueHistory_maxResults = Lens.lens (\GetAssetPropertyValueHistory' {maxResults} -> maxResults) (\s@GetAssetPropertyValueHistory' {} a -> s {maxResults = a} :: GetAssetPropertyValueHistory)

-- | The chronological sorting order of the requested information.
--
-- Default: @ASCENDING@
getAssetPropertyValueHistory_timeOrdering :: Lens.Lens' GetAssetPropertyValueHistory (Prelude.Maybe TimeOrdering)
getAssetPropertyValueHistory_timeOrdering = Lens.lens (\GetAssetPropertyValueHistory' {timeOrdering} -> timeOrdering) (\s@GetAssetPropertyValueHistory' {} a -> s {timeOrdering = a} :: GetAssetPropertyValueHistory)

instance Core.AWSPager GetAssetPropertyValueHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAssetPropertyValueHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getAssetPropertyValueHistoryResponse_assetPropertyValueHistory
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getAssetPropertyValueHistory_nextToken
          Lens..~ rs
          Lens.^? getAssetPropertyValueHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetAssetPropertyValueHistory where
  type
    AWSResponse GetAssetPropertyValueHistory =
      GetAssetPropertyValueHistoryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssetPropertyValueHistoryResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "assetPropertyValueHistory"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    GetAssetPropertyValueHistory
  where
  hashWithSalt _salt GetAssetPropertyValueHistory' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` propertyAlias
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` qualities
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` timeOrdering

instance Prelude.NFData GetAssetPropertyValueHistory where
  rnf GetAssetPropertyValueHistory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf qualities
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf timeOrdering

instance Core.ToHeaders GetAssetPropertyValueHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssetPropertyValueHistory where
  toPath = Prelude.const "/properties/history"

instance Core.ToQuery GetAssetPropertyValueHistory where
  toQuery GetAssetPropertyValueHistory' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "propertyAlias" Core.=: propertyAlias,
        "assetId" Core.=: assetId,
        "endDate" Core.=: endDate,
        "qualities"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> qualities),
        "propertyId" Core.=: propertyId,
        "startDate" Core.=: startDate,
        "maxResults" Core.=: maxResults,
        "timeOrdering" Core.=: timeOrdering
      ]

-- | /See:/ 'newGetAssetPropertyValueHistoryResponse' smart constructor.
data GetAssetPropertyValueHistoryResponse = GetAssetPropertyValueHistoryResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The asset property\'s value history.
    assetPropertyValueHistory :: [AssetPropertyValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyValueHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAssetPropertyValueHistoryResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'getAssetPropertyValueHistoryResponse_httpStatus' - The response's http status code.
--
-- 'assetPropertyValueHistory', 'getAssetPropertyValueHistoryResponse_assetPropertyValueHistory' - The asset property\'s value history.
newGetAssetPropertyValueHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssetPropertyValueHistoryResponse
newGetAssetPropertyValueHistoryResponse pHttpStatus_ =
  GetAssetPropertyValueHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assetPropertyValueHistory =
        Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
getAssetPropertyValueHistoryResponse_nextToken :: Lens.Lens' GetAssetPropertyValueHistoryResponse (Prelude.Maybe Prelude.Text)
getAssetPropertyValueHistoryResponse_nextToken = Lens.lens (\GetAssetPropertyValueHistoryResponse' {nextToken} -> nextToken) (\s@GetAssetPropertyValueHistoryResponse' {} a -> s {nextToken = a} :: GetAssetPropertyValueHistoryResponse)

-- | The response's http status code.
getAssetPropertyValueHistoryResponse_httpStatus :: Lens.Lens' GetAssetPropertyValueHistoryResponse Prelude.Int
getAssetPropertyValueHistoryResponse_httpStatus = Lens.lens (\GetAssetPropertyValueHistoryResponse' {httpStatus} -> httpStatus) (\s@GetAssetPropertyValueHistoryResponse' {} a -> s {httpStatus = a} :: GetAssetPropertyValueHistoryResponse)

-- | The asset property\'s value history.
getAssetPropertyValueHistoryResponse_assetPropertyValueHistory :: Lens.Lens' GetAssetPropertyValueHistoryResponse [AssetPropertyValue]
getAssetPropertyValueHistoryResponse_assetPropertyValueHistory = Lens.lens (\GetAssetPropertyValueHistoryResponse' {assetPropertyValueHistory} -> assetPropertyValueHistory) (\s@GetAssetPropertyValueHistoryResponse' {} a -> s {assetPropertyValueHistory = a} :: GetAssetPropertyValueHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetAssetPropertyValueHistoryResponse
  where
  rnf GetAssetPropertyValueHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetPropertyValueHistory
