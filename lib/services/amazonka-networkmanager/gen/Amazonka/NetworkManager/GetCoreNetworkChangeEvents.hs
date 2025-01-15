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
-- Module      : Amazonka.NetworkManager.GetCoreNetworkChangeEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a core network change event.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetCoreNetworkChangeEvents
  ( -- * Creating a Request
    GetCoreNetworkChangeEvents (..),
    newGetCoreNetworkChangeEvents,

    -- * Request Lenses
    getCoreNetworkChangeEvents_maxResults,
    getCoreNetworkChangeEvents_nextToken,
    getCoreNetworkChangeEvents_coreNetworkId,
    getCoreNetworkChangeEvents_policyVersionId,

    -- * Destructuring the Response
    GetCoreNetworkChangeEventsResponse (..),
    newGetCoreNetworkChangeEventsResponse,

    -- * Response Lenses
    getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents,
    getCoreNetworkChangeEventsResponse_nextToken,
    getCoreNetworkChangeEventsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreNetworkChangeEvents' smart constructor.
data GetCoreNetworkChangeEvents = GetCoreNetworkChangeEvents'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text,
    -- | The ID of the policy version.
    policyVersionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkChangeEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCoreNetworkChangeEvents_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getCoreNetworkChangeEvents_nextToken' - The token for the next page of results.
--
-- 'coreNetworkId', 'getCoreNetworkChangeEvents_coreNetworkId' - The ID of a core network.
--
-- 'policyVersionId', 'getCoreNetworkChangeEvents_policyVersionId' - The ID of the policy version.
newGetCoreNetworkChangeEvents ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Int ->
  GetCoreNetworkChangeEvents
newGetCoreNetworkChangeEvents
  pCoreNetworkId_
  pPolicyVersionId_ =
    GetCoreNetworkChangeEvents'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        policyVersionId = pPolicyVersionId_
      }

-- | The maximum number of results to return.
getCoreNetworkChangeEvents_maxResults :: Lens.Lens' GetCoreNetworkChangeEvents (Prelude.Maybe Prelude.Natural)
getCoreNetworkChangeEvents_maxResults = Lens.lens (\GetCoreNetworkChangeEvents' {maxResults} -> maxResults) (\s@GetCoreNetworkChangeEvents' {} a -> s {maxResults = a} :: GetCoreNetworkChangeEvents)

-- | The token for the next page of results.
getCoreNetworkChangeEvents_nextToken :: Lens.Lens' GetCoreNetworkChangeEvents (Prelude.Maybe Prelude.Text)
getCoreNetworkChangeEvents_nextToken = Lens.lens (\GetCoreNetworkChangeEvents' {nextToken} -> nextToken) (\s@GetCoreNetworkChangeEvents' {} a -> s {nextToken = a} :: GetCoreNetworkChangeEvents)

-- | The ID of a core network.
getCoreNetworkChangeEvents_coreNetworkId :: Lens.Lens' GetCoreNetworkChangeEvents Prelude.Text
getCoreNetworkChangeEvents_coreNetworkId = Lens.lens (\GetCoreNetworkChangeEvents' {coreNetworkId} -> coreNetworkId) (\s@GetCoreNetworkChangeEvents' {} a -> s {coreNetworkId = a} :: GetCoreNetworkChangeEvents)

-- | The ID of the policy version.
getCoreNetworkChangeEvents_policyVersionId :: Lens.Lens' GetCoreNetworkChangeEvents Prelude.Int
getCoreNetworkChangeEvents_policyVersionId = Lens.lens (\GetCoreNetworkChangeEvents' {policyVersionId} -> policyVersionId) (\s@GetCoreNetworkChangeEvents' {} a -> s {policyVersionId = a} :: GetCoreNetworkChangeEvents)

instance Core.AWSPager GetCoreNetworkChangeEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCoreNetworkChangeEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getCoreNetworkChangeEvents_nextToken
              Lens..~ rs
              Lens.^? getCoreNetworkChangeEventsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetCoreNetworkChangeEvents where
  type
    AWSResponse GetCoreNetworkChangeEvents =
      GetCoreNetworkChangeEventsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreNetworkChangeEventsResponse'
            Prelude.<$> ( x
                            Data..?> "CoreNetworkChangeEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreNetworkChangeEvents where
  hashWithSalt _salt GetCoreNetworkChangeEvents' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData GetCoreNetworkChangeEvents where
  rnf GetCoreNetworkChangeEvents' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf coreNetworkId `Prelude.seq`
          Prelude.rnf policyVersionId

instance Data.ToHeaders GetCoreNetworkChangeEvents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCoreNetworkChangeEvents where
  toPath GetCoreNetworkChangeEvents' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-change-events/",
        Data.toBS policyVersionId
      ]

instance Data.ToQuery GetCoreNetworkChangeEvents where
  toQuery GetCoreNetworkChangeEvents' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetCoreNetworkChangeEventsResponse' smart constructor.
data GetCoreNetworkChangeEventsResponse = GetCoreNetworkChangeEventsResponse'
  { -- | The response to @GetCoreNetworkChangeEventsRequest@.
    coreNetworkChangeEvents :: Prelude.Maybe [CoreNetworkChangeEvent],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkChangeEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkChangeEvents', 'getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents' - The response to @GetCoreNetworkChangeEventsRequest@.
--
-- 'nextToken', 'getCoreNetworkChangeEventsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getCoreNetworkChangeEventsResponse_httpStatus' - The response's http status code.
newGetCoreNetworkChangeEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreNetworkChangeEventsResponse
newGetCoreNetworkChangeEventsResponse pHttpStatus_ =
  GetCoreNetworkChangeEventsResponse'
    { coreNetworkChangeEvents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to @GetCoreNetworkChangeEventsRequest@.
getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents :: Lens.Lens' GetCoreNetworkChangeEventsResponse (Prelude.Maybe [CoreNetworkChangeEvent])
getCoreNetworkChangeEventsResponse_coreNetworkChangeEvents = Lens.lens (\GetCoreNetworkChangeEventsResponse' {coreNetworkChangeEvents} -> coreNetworkChangeEvents) (\s@GetCoreNetworkChangeEventsResponse' {} a -> s {coreNetworkChangeEvents = a} :: GetCoreNetworkChangeEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getCoreNetworkChangeEventsResponse_nextToken :: Lens.Lens' GetCoreNetworkChangeEventsResponse (Prelude.Maybe Prelude.Text)
getCoreNetworkChangeEventsResponse_nextToken = Lens.lens (\GetCoreNetworkChangeEventsResponse' {nextToken} -> nextToken) (\s@GetCoreNetworkChangeEventsResponse' {} a -> s {nextToken = a} :: GetCoreNetworkChangeEventsResponse)

-- | The response's http status code.
getCoreNetworkChangeEventsResponse_httpStatus :: Lens.Lens' GetCoreNetworkChangeEventsResponse Prelude.Int
getCoreNetworkChangeEventsResponse_httpStatus = Lens.lens (\GetCoreNetworkChangeEventsResponse' {httpStatus} -> httpStatus) (\s@GetCoreNetworkChangeEventsResponse' {} a -> s {httpStatus = a} :: GetCoreNetworkChangeEventsResponse)

instance
  Prelude.NFData
    GetCoreNetworkChangeEventsResponse
  where
  rnf GetCoreNetworkChangeEventsResponse' {..} =
    Prelude.rnf coreNetworkChangeEvents `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
