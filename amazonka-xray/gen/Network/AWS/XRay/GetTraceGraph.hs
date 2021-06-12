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
-- Module      : Network.AWS.XRay.GetTraceGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph for one or more specific trace IDs.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceGraph
  ( -- * Creating a Request
    GetTraceGraph (..),
    newGetTraceGraph,

    -- * Request Lenses
    getTraceGraph_nextToken,
    getTraceGraph_traceIds,

    -- * Destructuring the Response
    GetTraceGraphResponse (..),
    newGetTraceGraphResponse,

    -- * Response Lenses
    getTraceGraphResponse_nextToken,
    getTraceGraphResponse_services,
    getTraceGraphResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetTraceGraph' smart constructor.
data GetTraceGraph = GetTraceGraph'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | Trace IDs of requests for which to generate a service graph.
    traceIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTraceGraph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTraceGraph_nextToken' - Pagination token.
--
-- 'traceIds', 'getTraceGraph_traceIds' - Trace IDs of requests for which to generate a service graph.
newGetTraceGraph ::
  GetTraceGraph
newGetTraceGraph =
  GetTraceGraph'
    { nextToken = Core.Nothing,
      traceIds = Core.mempty
    }

-- | Pagination token.
getTraceGraph_nextToken :: Lens.Lens' GetTraceGraph (Core.Maybe Core.Text)
getTraceGraph_nextToken = Lens.lens (\GetTraceGraph' {nextToken} -> nextToken) (\s@GetTraceGraph' {} a -> s {nextToken = a} :: GetTraceGraph)

-- | Trace IDs of requests for which to generate a service graph.
getTraceGraph_traceIds :: Lens.Lens' GetTraceGraph [Core.Text]
getTraceGraph_traceIds = Lens.lens (\GetTraceGraph' {traceIds} -> traceIds) (\s@GetTraceGraph' {} a -> s {traceIds = a} :: GetTraceGraph) Core.. Lens._Coerce

instance Core.AWSPager GetTraceGraph where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTraceGraphResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTraceGraphResponse_services Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTraceGraph_nextToken
          Lens..~ rs
          Lens.^? getTraceGraphResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetTraceGraph where
  type
    AWSResponse GetTraceGraph =
      GetTraceGraphResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTraceGraphResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Services" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTraceGraph

instance Core.NFData GetTraceGraph

instance Core.ToHeaders GetTraceGraph where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetTraceGraph where
  toJSON GetTraceGraph' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            Core.Just ("TraceIds" Core..= traceIds)
          ]
      )

instance Core.ToPath GetTraceGraph where
  toPath = Core.const "/TraceGraph"

instance Core.ToQuery GetTraceGraph where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTraceGraphResponse' smart constructor.
data GetTraceGraphResponse = GetTraceGraphResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The services that have processed one of the specified requests.
    services :: Core.Maybe [ServiceInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTraceGraphResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTraceGraphResponse_nextToken' - Pagination token.
--
-- 'services', 'getTraceGraphResponse_services' - The services that have processed one of the specified requests.
--
-- 'httpStatus', 'getTraceGraphResponse_httpStatus' - The response's http status code.
newGetTraceGraphResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTraceGraphResponse
newGetTraceGraphResponse pHttpStatus_ =
  GetTraceGraphResponse'
    { nextToken = Core.Nothing,
      services = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getTraceGraphResponse_nextToken :: Lens.Lens' GetTraceGraphResponse (Core.Maybe Core.Text)
getTraceGraphResponse_nextToken = Lens.lens (\GetTraceGraphResponse' {nextToken} -> nextToken) (\s@GetTraceGraphResponse' {} a -> s {nextToken = a} :: GetTraceGraphResponse)

-- | The services that have processed one of the specified requests.
getTraceGraphResponse_services :: Lens.Lens' GetTraceGraphResponse (Core.Maybe [ServiceInfo])
getTraceGraphResponse_services = Lens.lens (\GetTraceGraphResponse' {services} -> services) (\s@GetTraceGraphResponse' {} a -> s {services = a} :: GetTraceGraphResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTraceGraphResponse_httpStatus :: Lens.Lens' GetTraceGraphResponse Core.Int
getTraceGraphResponse_httpStatus = Lens.lens (\GetTraceGraphResponse' {httpStatus} -> httpStatus) (\s@GetTraceGraphResponse' {} a -> s {httpStatus = a} :: GetTraceGraphResponse)

instance Core.NFData GetTraceGraphResponse
