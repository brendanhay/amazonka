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
-- Module      : Amazonka.ResilienceHub.ListSuggestedResiliencyPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the suggested resiliency policies for the Resilience Hub
-- applications.
module Amazonka.ResilienceHub.ListSuggestedResiliencyPolicies
  ( -- * Creating a Request
    ListSuggestedResiliencyPolicies (..),
    newListSuggestedResiliencyPolicies,

    -- * Request Lenses
    listSuggestedResiliencyPolicies_maxResults,
    listSuggestedResiliencyPolicies_nextToken,

    -- * Destructuring the Response
    ListSuggestedResiliencyPoliciesResponse (..),
    newListSuggestedResiliencyPoliciesResponse,

    -- * Response Lenses
    listSuggestedResiliencyPoliciesResponse_nextToken,
    listSuggestedResiliencyPoliciesResponse_httpStatus,
    listSuggestedResiliencyPoliciesResponse_resiliencyPolicies,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSuggestedResiliencyPolicies' smart constructor.
data ListSuggestedResiliencyPolicies = ListSuggestedResiliencyPolicies'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuggestedResiliencyPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSuggestedResiliencyPolicies_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listSuggestedResiliencyPolicies_nextToken' - Null, or the token from a previous call to get the next set of results.
newListSuggestedResiliencyPolicies ::
  ListSuggestedResiliencyPolicies
newListSuggestedResiliencyPolicies =
  ListSuggestedResiliencyPolicies'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listSuggestedResiliencyPolicies_maxResults :: Lens.Lens' ListSuggestedResiliencyPolicies (Prelude.Maybe Prelude.Natural)
listSuggestedResiliencyPolicies_maxResults = Lens.lens (\ListSuggestedResiliencyPolicies' {maxResults} -> maxResults) (\s@ListSuggestedResiliencyPolicies' {} a -> s {maxResults = a} :: ListSuggestedResiliencyPolicies)

-- | Null, or the token from a previous call to get the next set of results.
listSuggestedResiliencyPolicies_nextToken :: Lens.Lens' ListSuggestedResiliencyPolicies (Prelude.Maybe Prelude.Text)
listSuggestedResiliencyPolicies_nextToken = Lens.lens (\ListSuggestedResiliencyPolicies' {nextToken} -> nextToken) (\s@ListSuggestedResiliencyPolicies' {} a -> s {nextToken = a} :: ListSuggestedResiliencyPolicies)

instance
  Core.AWSRequest
    ListSuggestedResiliencyPolicies
  where
  type
    AWSResponse ListSuggestedResiliencyPolicies =
      ListSuggestedResiliencyPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSuggestedResiliencyPoliciesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "resiliencyPolicies"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListSuggestedResiliencyPolicies
  where
  hashWithSalt
    _salt
    ListSuggestedResiliencyPolicies' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListSuggestedResiliencyPolicies
  where
  rnf ListSuggestedResiliencyPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListSuggestedResiliencyPolicies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSuggestedResiliencyPolicies where
  toPath =
    Prelude.const "/list-suggested-resiliency-policies"

instance Data.ToQuery ListSuggestedResiliencyPolicies where
  toQuery ListSuggestedResiliencyPolicies' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSuggestedResiliencyPoliciesResponse' smart constructor.
data ListSuggestedResiliencyPoliciesResponse = ListSuggestedResiliencyPoliciesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The suggested resiliency policies for the Resilience Hub applications.
    resiliencyPolicies :: [ResiliencyPolicy]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSuggestedResiliencyPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSuggestedResiliencyPoliciesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listSuggestedResiliencyPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'resiliencyPolicies', 'listSuggestedResiliencyPoliciesResponse_resiliencyPolicies' - The suggested resiliency policies for the Resilience Hub applications.
newListSuggestedResiliencyPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSuggestedResiliencyPoliciesResponse
newListSuggestedResiliencyPoliciesResponse
  pHttpStatus_ =
    ListSuggestedResiliencyPoliciesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        resiliencyPolicies =
          Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listSuggestedResiliencyPoliciesResponse_nextToken :: Lens.Lens' ListSuggestedResiliencyPoliciesResponse (Prelude.Maybe Prelude.Text)
listSuggestedResiliencyPoliciesResponse_nextToken = Lens.lens (\ListSuggestedResiliencyPoliciesResponse' {nextToken} -> nextToken) (\s@ListSuggestedResiliencyPoliciesResponse' {} a -> s {nextToken = a} :: ListSuggestedResiliencyPoliciesResponse)

-- | The response's http status code.
listSuggestedResiliencyPoliciesResponse_httpStatus :: Lens.Lens' ListSuggestedResiliencyPoliciesResponse Prelude.Int
listSuggestedResiliencyPoliciesResponse_httpStatus = Lens.lens (\ListSuggestedResiliencyPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListSuggestedResiliencyPoliciesResponse' {} a -> s {httpStatus = a} :: ListSuggestedResiliencyPoliciesResponse)

-- | The suggested resiliency policies for the Resilience Hub applications.
listSuggestedResiliencyPoliciesResponse_resiliencyPolicies :: Lens.Lens' ListSuggestedResiliencyPoliciesResponse [ResiliencyPolicy]
listSuggestedResiliencyPoliciesResponse_resiliencyPolicies = Lens.lens (\ListSuggestedResiliencyPoliciesResponse' {resiliencyPolicies} -> resiliencyPolicies) (\s@ListSuggestedResiliencyPoliciesResponse' {} a -> s {resiliencyPolicies = a} :: ListSuggestedResiliencyPoliciesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSuggestedResiliencyPoliciesResponse
  where
  rnf ListSuggestedResiliencyPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resiliencyPolicies
