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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetServiceGraph' smart constructor.
data GetServiceGraph = GetServiceGraph'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The name of a group based on which you want to generate a graph.
    groupName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of a group based on which you want to
    -- generate a graph.
    groupARN :: Core.Maybe Core.Text,
    -- | The start of the time frame for which to generate a graph.
    startTime :: Core.POSIX,
    -- | The end of the timeframe for which to generate a graph.
    endTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  GetServiceGraph
newGetServiceGraph pStartTime_ pEndTime_ =
  GetServiceGraph'
    { nextToken = Core.Nothing,
      groupName = Core.Nothing,
      groupARN = Core.Nothing,
      startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | Pagination token.
getServiceGraph_nextToken :: Lens.Lens' GetServiceGraph (Core.Maybe Core.Text)
getServiceGraph_nextToken = Lens.lens (\GetServiceGraph' {nextToken} -> nextToken) (\s@GetServiceGraph' {} a -> s {nextToken = a} :: GetServiceGraph)

-- | The name of a group based on which you want to generate a graph.
getServiceGraph_groupName :: Lens.Lens' GetServiceGraph (Core.Maybe Core.Text)
getServiceGraph_groupName = Lens.lens (\GetServiceGraph' {groupName} -> groupName) (\s@GetServiceGraph' {} a -> s {groupName = a} :: GetServiceGraph)

-- | The Amazon Resource Name (ARN) of a group based on which you want to
-- generate a graph.
getServiceGraph_groupARN :: Lens.Lens' GetServiceGraph (Core.Maybe Core.Text)
getServiceGraph_groupARN = Lens.lens (\GetServiceGraph' {groupARN} -> groupARN) (\s@GetServiceGraph' {} a -> s {groupARN = a} :: GetServiceGraph)

-- | The start of the time frame for which to generate a graph.
getServiceGraph_startTime :: Lens.Lens' GetServiceGraph Core.UTCTime
getServiceGraph_startTime = Lens.lens (\GetServiceGraph' {startTime} -> startTime) (\s@GetServiceGraph' {} a -> s {startTime = a} :: GetServiceGraph) Core.. Core._Time

-- | The end of the timeframe for which to generate a graph.
getServiceGraph_endTime :: Lens.Lens' GetServiceGraph Core.UTCTime
getServiceGraph_endTime = Lens.lens (\GetServiceGraph' {endTime} -> endTime) (\s@GetServiceGraph' {} a -> s {endTime = a} :: GetServiceGraph) Core.. Core._Time

instance Core.AWSPager GetServiceGraph where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getServiceGraphResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getServiceGraphResponse_services Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getServiceGraph_nextToken
          Lens..~ rs
          Lens.^? getServiceGraphResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetServiceGraph where
  type
    AWSResponse GetServiceGraph =
      GetServiceGraphResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceGraphResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Services" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "StartTime")
            Core.<*> (x Core..?> "ContainsOldGroupVersions")
            Core.<*> (x Core..?> "EndTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetServiceGraph

instance Core.NFData GetServiceGraph

instance Core.ToHeaders GetServiceGraph where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetServiceGraph where
  toJSON GetServiceGraph' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("GroupName" Core..=) Core.<$> groupName,
            ("GroupARN" Core..=) Core.<$> groupARN,
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetServiceGraph where
  toPath = Core.const "/ServiceGraph"

instance Core.ToQuery GetServiceGraph where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetServiceGraphResponse' smart constructor.
data GetServiceGraphResponse = GetServiceGraphResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The services that have processed a traced request during the specified
    -- time frame.
    services :: Core.Maybe [ServiceInfo],
    -- | The start of the time frame for which the graph was generated.
    startTime :: Core.Maybe Core.POSIX,
    -- | A flag indicating whether the group\'s filter expression has been
    -- consistent, or if the returned service graph may show traces from an
    -- older version of the group\'s filter expression.
    containsOldGroupVersions :: Core.Maybe Core.Bool,
    -- | The end of the time frame for which the graph was generated.
    endTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetServiceGraphResponse
newGetServiceGraphResponse pHttpStatus_ =
  GetServiceGraphResponse'
    { nextToken = Core.Nothing,
      services = Core.Nothing,
      startTime = Core.Nothing,
      containsOldGroupVersions = Core.Nothing,
      endTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getServiceGraphResponse_nextToken :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.Text)
getServiceGraphResponse_nextToken = Lens.lens (\GetServiceGraphResponse' {nextToken} -> nextToken) (\s@GetServiceGraphResponse' {} a -> s {nextToken = a} :: GetServiceGraphResponse)

-- | The services that have processed a traced request during the specified
-- time frame.
getServiceGraphResponse_services :: Lens.Lens' GetServiceGraphResponse (Core.Maybe [ServiceInfo])
getServiceGraphResponse_services = Lens.lens (\GetServiceGraphResponse' {services} -> services) (\s@GetServiceGraphResponse' {} a -> s {services = a} :: GetServiceGraphResponse) Core.. Lens.mapping Lens._Coerce

-- | The start of the time frame for which the graph was generated.
getServiceGraphResponse_startTime :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.UTCTime)
getServiceGraphResponse_startTime = Lens.lens (\GetServiceGraphResponse' {startTime} -> startTime) (\s@GetServiceGraphResponse' {} a -> s {startTime = a} :: GetServiceGraphResponse) Core.. Lens.mapping Core._Time

-- | A flag indicating whether the group\'s filter expression has been
-- consistent, or if the returned service graph may show traces from an
-- older version of the group\'s filter expression.
getServiceGraphResponse_containsOldGroupVersions :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.Bool)
getServiceGraphResponse_containsOldGroupVersions = Lens.lens (\GetServiceGraphResponse' {containsOldGroupVersions} -> containsOldGroupVersions) (\s@GetServiceGraphResponse' {} a -> s {containsOldGroupVersions = a} :: GetServiceGraphResponse)

-- | The end of the time frame for which the graph was generated.
getServiceGraphResponse_endTime :: Lens.Lens' GetServiceGraphResponse (Core.Maybe Core.UTCTime)
getServiceGraphResponse_endTime = Lens.lens (\GetServiceGraphResponse' {endTime} -> endTime) (\s@GetServiceGraphResponse' {} a -> s {endTime = a} :: GetServiceGraphResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getServiceGraphResponse_httpStatus :: Lens.Lens' GetServiceGraphResponse Core.Int
getServiceGraphResponse_httpStatus = Lens.lens (\GetServiceGraphResponse' {httpStatus} -> httpStatus) (\s@GetServiceGraphResponse' {} a -> s {httpStatus = a} :: GetServiceGraphResponse)

instance Core.NFData GetServiceGraphResponse
