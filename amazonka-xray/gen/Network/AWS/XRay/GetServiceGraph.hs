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
-- Module      : Network.AWS.XRay.GetServiceGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a document that describes services that process incoming
-- requests, and downstream services that they call as a result. Root
-- services process incoming requests and make calls to downstream
-- services. Root services are applications that use the
-- <https://docs.aws.amazon.com/xray/index.html AWS X-Ray SDK>. Downstream
-- services can be other applications, AWS resources, HTTP web APIs, or SQL
-- databases.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetServiceGraph
  ( -- * Creating a Request
    GetServiceGraph (..),
    newGetServiceGraph,

    -- * Request Lenses
    getServiceGraph_nextToken,
    getServiceGraph_groupName,
    getServiceGraph_groupARN,
    getServiceGraph_startTime,
    getServiceGraph_endTime,

    -- * Destructuring the Response
    GetServiceGraphResponse (..),
    newGetServiceGraphResponse,

    -- * Response Lenses
    getServiceGraphResponse_nextToken,
    getServiceGraphResponse_services,
    getServiceGraphResponse_startTime,
    getServiceGraphResponse_containsOldGroupVersions,
    getServiceGraphResponse_endTime,
    getServiceGraphResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetServiceGraph' smart constructor.
data GetServiceGraph = GetServiceGraph'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of a group based on which you want to generate a graph.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a group based on which you want to
    -- generate a graph.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The start of the time frame for which to generate a graph.
    startTime :: Core.POSIX,
    -- | The end of the timeframe for which to generate a graph.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getServiceGraph_nextToken' - Pagination token.
--
-- 'groupName', 'getServiceGraph_groupName' - The name of a group based on which you want to generate a graph.
--
-- 'groupARN', 'getServiceGraph_groupARN' - The Amazon Resource Name (ARN) of a group based on which you want to
-- generate a graph.
--
-- 'startTime', 'getServiceGraph_startTime' - The start of the time frame for which to generate a graph.
--
-- 'endTime', 'getServiceGraph_endTime' - The end of the timeframe for which to generate a graph.
newGetServiceGraph ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetServiceGraph
newGetServiceGraph pStartTime_ pEndTime_ =
  GetServiceGraph'
    { nextToken = Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | Pagination token.
getServiceGraph_nextToken :: Lens.Lens' GetServiceGraph (Prelude.Maybe Prelude.Text)
getServiceGraph_nextToken = Lens.lens (\GetServiceGraph' {nextToken} -> nextToken) (\s@GetServiceGraph' {} a -> s {nextToken = a} :: GetServiceGraph)

-- | The name of a group based on which you want to generate a graph.
getServiceGraph_groupName :: Lens.Lens' GetServiceGraph (Prelude.Maybe Prelude.Text)
getServiceGraph_groupName = Lens.lens (\GetServiceGraph' {groupName} -> groupName) (\s@GetServiceGraph' {} a -> s {groupName = a} :: GetServiceGraph)

-- | The Amazon Resource Name (ARN) of a group based on which you want to
-- generate a graph.
getServiceGraph_groupARN :: Lens.Lens' GetServiceGraph (Prelude.Maybe Prelude.Text)
getServiceGraph_groupARN = Lens.lens (\GetServiceGraph' {groupARN} -> groupARN) (\s@GetServiceGraph' {} a -> s {groupARN = a} :: GetServiceGraph)

-- | The start of the time frame for which to generate a graph.
getServiceGraph_startTime :: Lens.Lens' GetServiceGraph Prelude.UTCTime
getServiceGraph_startTime = Lens.lens (\GetServiceGraph' {startTime} -> startTime) (\s@GetServiceGraph' {} a -> s {startTime = a} :: GetServiceGraph) Prelude.. Core._Time

-- | The end of the timeframe for which to generate a graph.
getServiceGraph_endTime :: Lens.Lens' GetServiceGraph Prelude.UTCTime
getServiceGraph_endTime = Lens.lens (\GetServiceGraph' {endTime} -> endTime) (\s@GetServiceGraph' {} a -> s {endTime = a} :: GetServiceGraph) Prelude.. Core._Time

instance Core.AWSPager GetServiceGraph where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getServiceGraphResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getServiceGraphResponse_services
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getServiceGraph_nextToken
          Lens..~ rs
          Lens.^? getServiceGraphResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetServiceGraph where
  type
    AWSResponse GetServiceGraph =
      GetServiceGraphResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceGraphResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Services" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "StartTime")
            Prelude.<*> (x Core..?> "ContainsOldGroupVersions")
            Prelude.<*> (x Core..?> "EndTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceGraph

instance Prelude.NFData GetServiceGraph

instance Core.ToHeaders GetServiceGraph where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetServiceGraph where
  toJSON GetServiceGraph' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("GroupName" Core..=) Prelude.<$> groupName,
            ("GroupARN" Core..=) Prelude.<$> groupARN,
            Prelude.Just ("StartTime" Core..= startTime),
            Prelude.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetServiceGraph where
  toPath = Prelude.const "/ServiceGraph"

instance Core.ToQuery GetServiceGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceGraphResponse' smart constructor.
data GetServiceGraphResponse = GetServiceGraphResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The services that have processed a traced request during the specified
    -- time frame.
    services :: Prelude.Maybe [ServiceInfo],
    -- | The start of the time frame for which the graph was generated.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A flag indicating whether the group\'s filter expression has been
    -- consistent, or if the returned service graph may show traces from an
    -- older version of the group\'s filter expression.
    containsOldGroupVersions :: Prelude.Maybe Prelude.Bool,
    -- | The end of the time frame for which the graph was generated.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getServiceGraphResponse_nextToken' - Pagination token.
--
-- 'services', 'getServiceGraphResponse_services' - The services that have processed a traced request during the specified
-- time frame.
--
-- 'startTime', 'getServiceGraphResponse_startTime' - The start of the time frame for which the graph was generated.
--
-- 'containsOldGroupVersions', 'getServiceGraphResponse_containsOldGroupVersions' - A flag indicating whether the group\'s filter expression has been
-- consistent, or if the returned service graph may show traces from an
-- older version of the group\'s filter expression.
--
-- 'endTime', 'getServiceGraphResponse_endTime' - The end of the time frame for which the graph was generated.
--
-- 'httpStatus', 'getServiceGraphResponse_httpStatus' - The response's http status code.
newGetServiceGraphResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceGraphResponse
newGetServiceGraphResponse pHttpStatus_ =
  GetServiceGraphResponse'
    { nextToken =
        Prelude.Nothing,
      services = Prelude.Nothing,
      startTime = Prelude.Nothing,
      containsOldGroupVersions = Prelude.Nothing,
      endTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getServiceGraphResponse_nextToken :: Lens.Lens' GetServiceGraphResponse (Prelude.Maybe Prelude.Text)
getServiceGraphResponse_nextToken = Lens.lens (\GetServiceGraphResponse' {nextToken} -> nextToken) (\s@GetServiceGraphResponse' {} a -> s {nextToken = a} :: GetServiceGraphResponse)

-- | The services that have processed a traced request during the specified
-- time frame.
getServiceGraphResponse_services :: Lens.Lens' GetServiceGraphResponse (Prelude.Maybe [ServiceInfo])
getServiceGraphResponse_services = Lens.lens (\GetServiceGraphResponse' {services} -> services) (\s@GetServiceGraphResponse' {} a -> s {services = a} :: GetServiceGraphResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The start of the time frame for which the graph was generated.
getServiceGraphResponse_startTime :: Lens.Lens' GetServiceGraphResponse (Prelude.Maybe Prelude.UTCTime)
getServiceGraphResponse_startTime = Lens.lens (\GetServiceGraphResponse' {startTime} -> startTime) (\s@GetServiceGraphResponse' {} a -> s {startTime = a} :: GetServiceGraphResponse) Prelude.. Lens.mapping Core._Time

-- | A flag indicating whether the group\'s filter expression has been
-- consistent, or if the returned service graph may show traces from an
-- older version of the group\'s filter expression.
getServiceGraphResponse_containsOldGroupVersions :: Lens.Lens' GetServiceGraphResponse (Prelude.Maybe Prelude.Bool)
getServiceGraphResponse_containsOldGroupVersions = Lens.lens (\GetServiceGraphResponse' {containsOldGroupVersions} -> containsOldGroupVersions) (\s@GetServiceGraphResponse' {} a -> s {containsOldGroupVersions = a} :: GetServiceGraphResponse)

-- | The end of the time frame for which the graph was generated.
getServiceGraphResponse_endTime :: Lens.Lens' GetServiceGraphResponse (Prelude.Maybe Prelude.UTCTime)
getServiceGraphResponse_endTime = Lens.lens (\GetServiceGraphResponse' {endTime} -> endTime) (\s@GetServiceGraphResponse' {} a -> s {endTime = a} :: GetServiceGraphResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getServiceGraphResponse_httpStatus :: Lens.Lens' GetServiceGraphResponse Prelude.Int
getServiceGraphResponse_httpStatus = Lens.lens (\GetServiceGraphResponse' {httpStatus} -> httpStatus) (\s@GetServiceGraphResponse' {} a -> s {httpStatus = a} :: GetServiceGraphResponse)

instance Prelude.NFData GetServiceGraphResponse
