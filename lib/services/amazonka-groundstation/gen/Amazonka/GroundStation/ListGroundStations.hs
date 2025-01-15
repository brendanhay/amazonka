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
-- Module      : Amazonka.GroundStation.ListGroundStations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ground stations.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListGroundStations
  ( -- * Creating a Request
    ListGroundStations (..),
    newListGroundStations,

    -- * Request Lenses
    listGroundStations_maxResults,
    listGroundStations_nextToken,
    listGroundStations_satelliteId,

    -- * Destructuring the Response
    ListGroundStationsResponse (..),
    newListGroundStationsResponse,

    -- * Response Lenses
    listGroundStationsResponse_groundStationList,
    listGroundStationsResponse_nextToken,
    listGroundStationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListGroundStations' smart constructor.
data ListGroundStations = ListGroundStations'
  { -- | Maximum number of ground stations returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token that can be supplied in the next call to get the next page of
    -- ground stations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Satellite ID to retrieve on-boarded ground stations.
    satelliteId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroundStations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGroundStations_maxResults' - Maximum number of ground stations returned.
--
-- 'nextToken', 'listGroundStations_nextToken' - Next token that can be supplied in the next call to get the next page of
-- ground stations.
--
-- 'satelliteId', 'listGroundStations_satelliteId' - Satellite ID to retrieve on-boarded ground stations.
newListGroundStations ::
  ListGroundStations
newListGroundStations =
  ListGroundStations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      satelliteId = Prelude.Nothing
    }

-- | Maximum number of ground stations returned.
listGroundStations_maxResults :: Lens.Lens' ListGroundStations (Prelude.Maybe Prelude.Natural)
listGroundStations_maxResults = Lens.lens (\ListGroundStations' {maxResults} -> maxResults) (\s@ListGroundStations' {} a -> s {maxResults = a} :: ListGroundStations)

-- | Next token that can be supplied in the next call to get the next page of
-- ground stations.
listGroundStations_nextToken :: Lens.Lens' ListGroundStations (Prelude.Maybe Prelude.Text)
listGroundStations_nextToken = Lens.lens (\ListGroundStations' {nextToken} -> nextToken) (\s@ListGroundStations' {} a -> s {nextToken = a} :: ListGroundStations)

-- | Satellite ID to retrieve on-boarded ground stations.
listGroundStations_satelliteId :: Lens.Lens' ListGroundStations (Prelude.Maybe Prelude.Text)
listGroundStations_satelliteId = Lens.lens (\ListGroundStations' {satelliteId} -> satelliteId) (\s@ListGroundStations' {} a -> s {satelliteId = a} :: ListGroundStations)

instance Core.AWSPager ListGroundStations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroundStationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroundStationsResponse_groundStationList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listGroundStations_nextToken
              Lens..~ rs
              Lens.^? listGroundStationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListGroundStations where
  type
    AWSResponse ListGroundStations =
      ListGroundStationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroundStationsResponse'
            Prelude.<$> ( x
                            Data..?> "groundStationList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroundStations where
  hashWithSalt _salt ListGroundStations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` satelliteId

instance Prelude.NFData ListGroundStations where
  rnf ListGroundStations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf satelliteId

instance Data.ToHeaders ListGroundStations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGroundStations where
  toPath = Prelude.const "/groundstation"

instance Data.ToQuery ListGroundStations where
  toQuery ListGroundStations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "satelliteId" Data.=: satelliteId
      ]

-- |
--
-- /See:/ 'newListGroundStationsResponse' smart constructor.
data ListGroundStationsResponse = ListGroundStationsResponse'
  { -- | List of ground stations.
    groundStationList :: Prelude.Maybe [GroundStationData],
    -- | Next token that can be supplied in the next call to get the next page of
    -- ground stations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroundStationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groundStationList', 'listGroundStationsResponse_groundStationList' - List of ground stations.
--
-- 'nextToken', 'listGroundStationsResponse_nextToken' - Next token that can be supplied in the next call to get the next page of
-- ground stations.
--
-- 'httpStatus', 'listGroundStationsResponse_httpStatus' - The response's http status code.
newListGroundStationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroundStationsResponse
newListGroundStationsResponse pHttpStatus_ =
  ListGroundStationsResponse'
    { groundStationList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of ground stations.
listGroundStationsResponse_groundStationList :: Lens.Lens' ListGroundStationsResponse (Prelude.Maybe [GroundStationData])
listGroundStationsResponse_groundStationList = Lens.lens (\ListGroundStationsResponse' {groundStationList} -> groundStationList) (\s@ListGroundStationsResponse' {} a -> s {groundStationList = a} :: ListGroundStationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token that can be supplied in the next call to get the next page of
-- ground stations.
listGroundStationsResponse_nextToken :: Lens.Lens' ListGroundStationsResponse (Prelude.Maybe Prelude.Text)
listGroundStationsResponse_nextToken = Lens.lens (\ListGroundStationsResponse' {nextToken} -> nextToken) (\s@ListGroundStationsResponse' {} a -> s {nextToken = a} :: ListGroundStationsResponse)

-- | The response's http status code.
listGroundStationsResponse_httpStatus :: Lens.Lens' ListGroundStationsResponse Prelude.Int
listGroundStationsResponse_httpStatus = Lens.lens (\ListGroundStationsResponse' {httpStatus} -> httpStatus) (\s@ListGroundStationsResponse' {} a -> s {httpStatus = a} :: ListGroundStationsResponse)

instance Prelude.NFData ListGroundStationsResponse where
  rnf ListGroundStationsResponse' {..} =
    Prelude.rnf groundStationList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
