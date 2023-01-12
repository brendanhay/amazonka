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
-- Module      : Amazonka.NetworkManager.GetCoreNetworkChangeSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a change set between the LIVE core network policy and a
-- submitted policy.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetCoreNetworkChangeSet
  ( -- * Creating a Request
    GetCoreNetworkChangeSet (..),
    newGetCoreNetworkChangeSet,

    -- * Request Lenses
    getCoreNetworkChangeSet_maxResults,
    getCoreNetworkChangeSet_nextToken,
    getCoreNetworkChangeSet_coreNetworkId,
    getCoreNetworkChangeSet_policyVersionId,

    -- * Destructuring the Response
    GetCoreNetworkChangeSetResponse (..),
    newGetCoreNetworkChangeSetResponse,

    -- * Response Lenses
    getCoreNetworkChangeSetResponse_coreNetworkChanges,
    getCoreNetworkChangeSetResponse_nextToken,
    getCoreNetworkChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreNetworkChangeSet' smart constructor.
data GetCoreNetworkChangeSet = GetCoreNetworkChangeSet'
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
-- Create a value of 'GetCoreNetworkChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getCoreNetworkChangeSet_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getCoreNetworkChangeSet_nextToken' - The token for the next page of results.
--
-- 'coreNetworkId', 'getCoreNetworkChangeSet_coreNetworkId' - The ID of a core network.
--
-- 'policyVersionId', 'getCoreNetworkChangeSet_policyVersionId' - The ID of the policy version.
newGetCoreNetworkChangeSet ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Int ->
  GetCoreNetworkChangeSet
newGetCoreNetworkChangeSet
  pCoreNetworkId_
  pPolicyVersionId_ =
    GetCoreNetworkChangeSet'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        policyVersionId = pPolicyVersionId_
      }

-- | The maximum number of results to return.
getCoreNetworkChangeSet_maxResults :: Lens.Lens' GetCoreNetworkChangeSet (Prelude.Maybe Prelude.Natural)
getCoreNetworkChangeSet_maxResults = Lens.lens (\GetCoreNetworkChangeSet' {maxResults} -> maxResults) (\s@GetCoreNetworkChangeSet' {} a -> s {maxResults = a} :: GetCoreNetworkChangeSet)

-- | The token for the next page of results.
getCoreNetworkChangeSet_nextToken :: Lens.Lens' GetCoreNetworkChangeSet (Prelude.Maybe Prelude.Text)
getCoreNetworkChangeSet_nextToken = Lens.lens (\GetCoreNetworkChangeSet' {nextToken} -> nextToken) (\s@GetCoreNetworkChangeSet' {} a -> s {nextToken = a} :: GetCoreNetworkChangeSet)

-- | The ID of a core network.
getCoreNetworkChangeSet_coreNetworkId :: Lens.Lens' GetCoreNetworkChangeSet Prelude.Text
getCoreNetworkChangeSet_coreNetworkId = Lens.lens (\GetCoreNetworkChangeSet' {coreNetworkId} -> coreNetworkId) (\s@GetCoreNetworkChangeSet' {} a -> s {coreNetworkId = a} :: GetCoreNetworkChangeSet)

-- | The ID of the policy version.
getCoreNetworkChangeSet_policyVersionId :: Lens.Lens' GetCoreNetworkChangeSet Prelude.Int
getCoreNetworkChangeSet_policyVersionId = Lens.lens (\GetCoreNetworkChangeSet' {policyVersionId} -> policyVersionId) (\s@GetCoreNetworkChangeSet' {} a -> s {policyVersionId = a} :: GetCoreNetworkChangeSet)

instance Core.AWSPager GetCoreNetworkChangeSet where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCoreNetworkChangeSetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCoreNetworkChangeSetResponse_coreNetworkChanges
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCoreNetworkChangeSet_nextToken
          Lens..~ rs
          Lens.^? getCoreNetworkChangeSetResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetCoreNetworkChangeSet where
  type
    AWSResponse GetCoreNetworkChangeSet =
      GetCoreNetworkChangeSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreNetworkChangeSetResponse'
            Prelude.<$> ( x Data..?> "CoreNetworkChanges"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreNetworkChangeSet where
  hashWithSalt _salt GetCoreNetworkChangeSet' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData GetCoreNetworkChangeSet where
  rnf GetCoreNetworkChangeSet' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf policyVersionId

instance Data.ToHeaders GetCoreNetworkChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCoreNetworkChangeSet where
  toPath GetCoreNetworkChangeSet' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-change-sets/",
        Data.toBS policyVersionId
      ]

instance Data.ToQuery GetCoreNetworkChangeSet where
  toQuery GetCoreNetworkChangeSet' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetCoreNetworkChangeSetResponse' smart constructor.
data GetCoreNetworkChangeSetResponse = GetCoreNetworkChangeSetResponse'
  { -- | Describes a core network changes.
    coreNetworkChanges :: Prelude.Maybe [CoreNetworkChange],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkChanges', 'getCoreNetworkChangeSetResponse_coreNetworkChanges' - Describes a core network changes.
--
-- 'nextToken', 'getCoreNetworkChangeSetResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getCoreNetworkChangeSetResponse_httpStatus' - The response's http status code.
newGetCoreNetworkChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreNetworkChangeSetResponse
newGetCoreNetworkChangeSetResponse pHttpStatus_ =
  GetCoreNetworkChangeSetResponse'
    { coreNetworkChanges =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes a core network changes.
getCoreNetworkChangeSetResponse_coreNetworkChanges :: Lens.Lens' GetCoreNetworkChangeSetResponse (Prelude.Maybe [CoreNetworkChange])
getCoreNetworkChangeSetResponse_coreNetworkChanges = Lens.lens (\GetCoreNetworkChangeSetResponse' {coreNetworkChanges} -> coreNetworkChanges) (\s@GetCoreNetworkChangeSetResponse' {} a -> s {coreNetworkChanges = a} :: GetCoreNetworkChangeSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getCoreNetworkChangeSetResponse_nextToken :: Lens.Lens' GetCoreNetworkChangeSetResponse (Prelude.Maybe Prelude.Text)
getCoreNetworkChangeSetResponse_nextToken = Lens.lens (\GetCoreNetworkChangeSetResponse' {nextToken} -> nextToken) (\s@GetCoreNetworkChangeSetResponse' {} a -> s {nextToken = a} :: GetCoreNetworkChangeSetResponse)

-- | The response's http status code.
getCoreNetworkChangeSetResponse_httpStatus :: Lens.Lens' GetCoreNetworkChangeSetResponse Prelude.Int
getCoreNetworkChangeSetResponse_httpStatus = Lens.lens (\GetCoreNetworkChangeSetResponse' {httpStatus} -> httpStatus) (\s@GetCoreNetworkChangeSetResponse' {} a -> s {httpStatus = a} :: GetCoreNetworkChangeSetResponse)

instance
  Prelude.NFData
    GetCoreNetworkChangeSetResponse
  where
  rnf GetCoreNetworkChangeSetResponse' {..} =
    Prelude.rnf coreNetworkChanges
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
