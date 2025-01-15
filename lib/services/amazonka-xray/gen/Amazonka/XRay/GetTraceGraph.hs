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
-- Module      : Amazonka.XRay.GetTraceGraph
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a service graph for one or more specific trace IDs.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetTraceGraph
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetTraceGraph' smart constructor.
data GetTraceGraph = GetTraceGraph'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Trace IDs of requests for which to generate a service graph.
    traceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      traceIds = Prelude.mempty
    }

-- | Pagination token.
getTraceGraph_nextToken :: Lens.Lens' GetTraceGraph (Prelude.Maybe Prelude.Text)
getTraceGraph_nextToken = Lens.lens (\GetTraceGraph' {nextToken} -> nextToken) (\s@GetTraceGraph' {} a -> s {nextToken = a} :: GetTraceGraph)

-- | Trace IDs of requests for which to generate a service graph.
getTraceGraph_traceIds :: Lens.Lens' GetTraceGraph [Prelude.Text]
getTraceGraph_traceIds = Lens.lens (\GetTraceGraph' {traceIds} -> traceIds) (\s@GetTraceGraph' {} a -> s {traceIds = a} :: GetTraceGraph) Prelude.. Lens.coerced

instance Core.AWSPager GetTraceGraph where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTraceGraphResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTraceGraphResponse_services
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getTraceGraph_nextToken
              Lens..~ rs
              Lens.^? getTraceGraphResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetTraceGraph where
  type
    AWSResponse GetTraceGraph =
      GetTraceGraphResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTraceGraphResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Services" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTraceGraph where
  hashWithSalt _salt GetTraceGraph' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` traceIds

instance Prelude.NFData GetTraceGraph where
  rnf GetTraceGraph' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf traceIds

instance Data.ToHeaders GetTraceGraph where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetTraceGraph where
  toJSON GetTraceGraph' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("TraceIds" Data..= traceIds)
          ]
      )

instance Data.ToPath GetTraceGraph where
  toPath = Prelude.const "/TraceGraph"

instance Data.ToQuery GetTraceGraph where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTraceGraphResponse' smart constructor.
data GetTraceGraphResponse = GetTraceGraphResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The services that have processed one of the specified requests.
    services :: Prelude.Maybe [ServiceInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTraceGraphResponse
newGetTraceGraphResponse pHttpStatus_ =
  GetTraceGraphResponse'
    { nextToken = Prelude.Nothing,
      services = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
getTraceGraphResponse_nextToken :: Lens.Lens' GetTraceGraphResponse (Prelude.Maybe Prelude.Text)
getTraceGraphResponse_nextToken = Lens.lens (\GetTraceGraphResponse' {nextToken} -> nextToken) (\s@GetTraceGraphResponse' {} a -> s {nextToken = a} :: GetTraceGraphResponse)

-- | The services that have processed one of the specified requests.
getTraceGraphResponse_services :: Lens.Lens' GetTraceGraphResponse (Prelude.Maybe [ServiceInfo])
getTraceGraphResponse_services = Lens.lens (\GetTraceGraphResponse' {services} -> services) (\s@GetTraceGraphResponse' {} a -> s {services = a} :: GetTraceGraphResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTraceGraphResponse_httpStatus :: Lens.Lens' GetTraceGraphResponse Prelude.Int
getTraceGraphResponse_httpStatus = Lens.lens (\GetTraceGraphResponse' {httpStatus} -> httpStatus) (\s@GetTraceGraphResponse' {} a -> s {httpStatus = a} :: GetTraceGraphResponse)

instance Prelude.NFData GetTraceGraphResponse where
  rnf GetTraceGraphResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf services `Prelude.seq`
        Prelude.rnf httpStatus
