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
-- Module      : Amazonka.AuditManager.GetDelegations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of delegations from an audit owner to a delegate.
module Amazonka.AuditManager.GetDelegations
  ( -- * Creating a Request
    GetDelegations (..),
    newGetDelegations,

    -- * Request Lenses
    getDelegations_maxResults,
    getDelegations_nextToken,

    -- * Destructuring the Response
    GetDelegationsResponse (..),
    newGetDelegationsResponse,

    -- * Response Lenses
    getDelegationsResponse_delegations,
    getDelegationsResponse_nextToken,
    getDelegationsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDelegations' smart constructor.
data GetDelegations = GetDelegations'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getDelegations_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'getDelegations_nextToken' - The pagination token that\'s used to fetch the next set of results.
newGetDelegations ::
  GetDelegations
newGetDelegations =
  GetDelegations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
getDelegations_maxResults :: Lens.Lens' GetDelegations (Prelude.Maybe Prelude.Natural)
getDelegations_maxResults = Lens.lens (\GetDelegations' {maxResults} -> maxResults) (\s@GetDelegations' {} a -> s {maxResults = a} :: GetDelegations)

-- | The pagination token that\'s used to fetch the next set of results.
getDelegations_nextToken :: Lens.Lens' GetDelegations (Prelude.Maybe Prelude.Text)
getDelegations_nextToken = Lens.lens (\GetDelegations' {nextToken} -> nextToken) (\s@GetDelegations' {} a -> s {nextToken = a} :: GetDelegations)

instance Core.AWSRequest GetDelegations where
  type
    AWSResponse GetDelegations =
      GetDelegationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDelegationsResponse'
            Prelude.<$> (x Data..?> "delegations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDelegations where
  hashWithSalt _salt GetDelegations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDelegations where
  rnf GetDelegations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetDelegations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDelegations where
  toPath = Prelude.const "/delegations"

instance Data.ToQuery GetDelegations where
  toQuery GetDelegations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetDelegationsResponse' smart constructor.
data GetDelegationsResponse = GetDelegationsResponse'
  { -- | The list of delegations that the @GetDelegations@ API returned.
    delegations :: Prelude.Maybe [DelegationMetadata],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegations', 'getDelegationsResponse_delegations' - The list of delegations that the @GetDelegations@ API returned.
--
-- 'nextToken', 'getDelegationsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'getDelegationsResponse_httpStatus' - The response's http status code.
newGetDelegationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDelegationsResponse
newGetDelegationsResponse pHttpStatus_ =
  GetDelegationsResponse'
    { delegations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of delegations that the @GetDelegations@ API returned.
getDelegationsResponse_delegations :: Lens.Lens' GetDelegationsResponse (Prelude.Maybe [DelegationMetadata])
getDelegationsResponse_delegations = Lens.lens (\GetDelegationsResponse' {delegations} -> delegations) (\s@GetDelegationsResponse' {} a -> s {delegations = a} :: GetDelegationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
getDelegationsResponse_nextToken :: Lens.Lens' GetDelegationsResponse (Prelude.Maybe Prelude.Text)
getDelegationsResponse_nextToken = Lens.lens (\GetDelegationsResponse' {nextToken} -> nextToken) (\s@GetDelegationsResponse' {} a -> s {nextToken = a} :: GetDelegationsResponse)

-- | The response's http status code.
getDelegationsResponse_httpStatus :: Lens.Lens' GetDelegationsResponse Prelude.Int
getDelegationsResponse_httpStatus = Lens.lens (\GetDelegationsResponse' {httpStatus} -> httpStatus) (\s@GetDelegationsResponse' {} a -> s {httpStatus = a} :: GetDelegationsResponse)

instance Prelude.NFData GetDelegationsResponse where
  rnf GetDelegationsResponse' {..} =
    Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
