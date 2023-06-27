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
-- Module      : Amazonka.XRay.GetInsightImpactGraph
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph structure filtered by the specified insight.
-- The service graph is limited to only structural information. For a
-- complete service graph, use this API with the GetServiceGraph API.
module Amazonka.XRay.GetInsightImpactGraph
  ( -- * Creating a Request
    GetInsightImpactGraph (..),
    newGetInsightImpactGraph,

    -- * Request Lenses
    getInsightImpactGraph_nextToken,
    getInsightImpactGraph_insightId,
    getInsightImpactGraph_startTime,
    getInsightImpactGraph_endTime,

    -- * Destructuring the Response
    GetInsightImpactGraphResponse (..),
    newGetInsightImpactGraphResponse,

    -- * Response Lenses
    getInsightImpactGraphResponse_endTime,
    getInsightImpactGraphResponse_insightId,
    getInsightImpactGraphResponse_nextToken,
    getInsightImpactGraphResponse_serviceGraphEndTime,
    getInsightImpactGraphResponse_serviceGraphStartTime,
    getInsightImpactGraphResponse_services,
    getInsightImpactGraphResponse_startTime,
    getInsightImpactGraphResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetInsightImpactGraph' smart constructor.
data GetInsightImpactGraph = GetInsightImpactGraph'
  { -- | Specify the pagination token returned by a previous request to retrieve
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The insight\'s unique identifier. Use the GetInsightSummaries action to
    -- retrieve an InsightId.
    insightId :: Prelude.Text,
    -- | The estimated start time of the insight, in Unix time seconds. The
    -- StartTime is inclusive of the value provided and can\'t be more than 30
    -- days old.
    startTime :: Data.POSIX,
    -- | The estimated end time of the insight, in Unix time seconds. The EndTime
    -- is exclusive of the value provided. The time range between the start
    -- time and end time can\'t be more than six hours.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightImpactGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInsightImpactGraph_nextToken' - Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
--
-- 'insightId', 'getInsightImpactGraph_insightId' - The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
--
-- 'startTime', 'getInsightImpactGraph_startTime' - The estimated start time of the insight, in Unix time seconds. The
-- StartTime is inclusive of the value provided and can\'t be more than 30
-- days old.
--
-- 'endTime', 'getInsightImpactGraph_endTime' - The estimated end time of the insight, in Unix time seconds. The EndTime
-- is exclusive of the value provided. The time range between the start
-- time and end time can\'t be more than six hours.
newGetInsightImpactGraph ::
  -- | 'insightId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetInsightImpactGraph
newGetInsightImpactGraph
  pInsightId_
  pStartTime_
  pEndTime_ =
    GetInsightImpactGraph'
      { nextToken = Prelude.Nothing,
        insightId = pInsightId_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
getInsightImpactGraph_nextToken :: Lens.Lens' GetInsightImpactGraph (Prelude.Maybe Prelude.Text)
getInsightImpactGraph_nextToken = Lens.lens (\GetInsightImpactGraph' {nextToken} -> nextToken) (\s@GetInsightImpactGraph' {} a -> s {nextToken = a} :: GetInsightImpactGraph)

-- | The insight\'s unique identifier. Use the GetInsightSummaries action to
-- retrieve an InsightId.
getInsightImpactGraph_insightId :: Lens.Lens' GetInsightImpactGraph Prelude.Text
getInsightImpactGraph_insightId = Lens.lens (\GetInsightImpactGraph' {insightId} -> insightId) (\s@GetInsightImpactGraph' {} a -> s {insightId = a} :: GetInsightImpactGraph)

-- | The estimated start time of the insight, in Unix time seconds. The
-- StartTime is inclusive of the value provided and can\'t be more than 30
-- days old.
getInsightImpactGraph_startTime :: Lens.Lens' GetInsightImpactGraph Prelude.UTCTime
getInsightImpactGraph_startTime = Lens.lens (\GetInsightImpactGraph' {startTime} -> startTime) (\s@GetInsightImpactGraph' {} a -> s {startTime = a} :: GetInsightImpactGraph) Prelude.. Data._Time

-- | The estimated end time of the insight, in Unix time seconds. The EndTime
-- is exclusive of the value provided. The time range between the start
-- time and end time can\'t be more than six hours.
getInsightImpactGraph_endTime :: Lens.Lens' GetInsightImpactGraph Prelude.UTCTime
getInsightImpactGraph_endTime = Lens.lens (\GetInsightImpactGraph' {endTime} -> endTime) (\s@GetInsightImpactGraph' {} a -> s {endTime = a} :: GetInsightImpactGraph) Prelude.. Data._Time

instance Core.AWSRequest GetInsightImpactGraph where
  type
    AWSResponse GetInsightImpactGraph =
      GetInsightImpactGraphResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightImpactGraphResponse'
            Prelude.<$> (x Data..?> "EndTime")
            Prelude.<*> (x Data..?> "InsightId")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ServiceGraphEndTime")
            Prelude.<*> (x Data..?> "ServiceGraphStartTime")
            Prelude.<*> (x Data..?> "Services" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "StartTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInsightImpactGraph where
  hashWithSalt _salt GetInsightImpactGraph' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` insightId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetInsightImpactGraph where
  rnf GetInsightImpactGraph' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf insightId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetInsightImpactGraph where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetInsightImpactGraph where
  toJSON GetInsightImpactGraph' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InsightId" Data..= insightId),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetInsightImpactGraph where
  toPath = Prelude.const "/InsightImpactGraph"

instance Data.ToQuery GetInsightImpactGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInsightImpactGraphResponse' smart constructor.
data GetInsightImpactGraphResponse = GetInsightImpactGraphResponse'
  { -- | The provided end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The insight\'s unique identifier.
    insightId :: Prelude.Maybe Prelude.Text,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, at which the service graph ended.
    serviceGraphEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time, in Unix seconds, at which the service graph started.
    serviceGraphStartTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services instrumented services related to the insight.
    services :: Prelude.Maybe [InsightImpactGraphService],
    -- | The provided start time.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInsightImpactGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'getInsightImpactGraphResponse_endTime' - The provided end time.
--
-- 'insightId', 'getInsightImpactGraphResponse_insightId' - The insight\'s unique identifier.
--
-- 'nextToken', 'getInsightImpactGraphResponse_nextToken' - Pagination token.
--
-- 'serviceGraphEndTime', 'getInsightImpactGraphResponse_serviceGraphEndTime' - The time, in Unix seconds, at which the service graph ended.
--
-- 'serviceGraphStartTime', 'getInsightImpactGraphResponse_serviceGraphStartTime' - The time, in Unix seconds, at which the service graph started.
--
-- 'services', 'getInsightImpactGraphResponse_services' - The Amazon Web Services instrumented services related to the insight.
--
-- 'startTime', 'getInsightImpactGraphResponse_startTime' - The provided start time.
--
-- 'httpStatus', 'getInsightImpactGraphResponse_httpStatus' - The response's http status code.
newGetInsightImpactGraphResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInsightImpactGraphResponse
newGetInsightImpactGraphResponse pHttpStatus_ =
  GetInsightImpactGraphResponse'
    { endTime =
        Prelude.Nothing,
      insightId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceGraphEndTime = Prelude.Nothing,
      serviceGraphStartTime = Prelude.Nothing,
      services = Prelude.Nothing,
      startTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The provided end time.
getInsightImpactGraphResponse_endTime :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.UTCTime)
getInsightImpactGraphResponse_endTime = Lens.lens (\GetInsightImpactGraphResponse' {endTime} -> endTime) (\s@GetInsightImpactGraphResponse' {} a -> s {endTime = a} :: GetInsightImpactGraphResponse) Prelude.. Lens.mapping Data._Time

-- | The insight\'s unique identifier.
getInsightImpactGraphResponse_insightId :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.Text)
getInsightImpactGraphResponse_insightId = Lens.lens (\GetInsightImpactGraphResponse' {insightId} -> insightId) (\s@GetInsightImpactGraphResponse' {} a -> s {insightId = a} :: GetInsightImpactGraphResponse)

-- | Pagination token.
getInsightImpactGraphResponse_nextToken :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.Text)
getInsightImpactGraphResponse_nextToken = Lens.lens (\GetInsightImpactGraphResponse' {nextToken} -> nextToken) (\s@GetInsightImpactGraphResponse' {} a -> s {nextToken = a} :: GetInsightImpactGraphResponse)

-- | The time, in Unix seconds, at which the service graph ended.
getInsightImpactGraphResponse_serviceGraphEndTime :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.UTCTime)
getInsightImpactGraphResponse_serviceGraphEndTime = Lens.lens (\GetInsightImpactGraphResponse' {serviceGraphEndTime} -> serviceGraphEndTime) (\s@GetInsightImpactGraphResponse' {} a -> s {serviceGraphEndTime = a} :: GetInsightImpactGraphResponse) Prelude.. Lens.mapping Data._Time

-- | The time, in Unix seconds, at which the service graph started.
getInsightImpactGraphResponse_serviceGraphStartTime :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.UTCTime)
getInsightImpactGraphResponse_serviceGraphStartTime = Lens.lens (\GetInsightImpactGraphResponse' {serviceGraphStartTime} -> serviceGraphStartTime) (\s@GetInsightImpactGraphResponse' {} a -> s {serviceGraphStartTime = a} :: GetInsightImpactGraphResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services instrumented services related to the insight.
getInsightImpactGraphResponse_services :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe [InsightImpactGraphService])
getInsightImpactGraphResponse_services = Lens.lens (\GetInsightImpactGraphResponse' {services} -> services) (\s@GetInsightImpactGraphResponse' {} a -> s {services = a} :: GetInsightImpactGraphResponse) Prelude.. Lens.mapping Lens.coerced

-- | The provided start time.
getInsightImpactGraphResponse_startTime :: Lens.Lens' GetInsightImpactGraphResponse (Prelude.Maybe Prelude.UTCTime)
getInsightImpactGraphResponse_startTime = Lens.lens (\GetInsightImpactGraphResponse' {startTime} -> startTime) (\s@GetInsightImpactGraphResponse' {} a -> s {startTime = a} :: GetInsightImpactGraphResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getInsightImpactGraphResponse_httpStatus :: Lens.Lens' GetInsightImpactGraphResponse Prelude.Int
getInsightImpactGraphResponse_httpStatus = Lens.lens (\GetInsightImpactGraphResponse' {httpStatus} -> httpStatus) (\s@GetInsightImpactGraphResponse' {} a -> s {httpStatus = a} :: GetInsightImpactGraphResponse)

instance Prelude.NFData GetInsightImpactGraphResponse where
  rnf GetInsightImpactGraphResponse' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf insightId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceGraphEndTime
      `Prelude.seq` Prelude.rnf serviceGraphStartTime
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus
