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
-- Module      : Amazonka.ResilienceHub.ListResiliencyPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resiliency policies for the Resilience Hub applications.
module Amazonka.ResilienceHub.ListResiliencyPolicies
  ( -- * Creating a Request
    ListResiliencyPolicies (..),
    newListResiliencyPolicies,

    -- * Request Lenses
    listResiliencyPolicies_maxResults,
    listResiliencyPolicies_nextToken,
    listResiliencyPolicies_policyName,

    -- * Destructuring the Response
    ListResiliencyPoliciesResponse (..),
    newListResiliencyPoliciesResponse,

    -- * Response Lenses
    listResiliencyPoliciesResponse_nextToken,
    listResiliencyPoliciesResponse_httpStatus,
    listResiliencyPoliciesResponse_resiliencyPolicies,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResiliencyPolicies' smart constructor.
data ListResiliencyPolicies = ListResiliencyPolicies'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy
    policyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResiliencyPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResiliencyPolicies_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listResiliencyPolicies_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'policyName', 'listResiliencyPolicies_policyName' - The name of the policy
newListResiliencyPolicies ::
  ListResiliencyPolicies
newListResiliencyPolicies =
  ListResiliencyPolicies'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      policyName = Prelude.Nothing
    }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listResiliencyPolicies_maxResults :: Lens.Lens' ListResiliencyPolicies (Prelude.Maybe Prelude.Natural)
listResiliencyPolicies_maxResults = Lens.lens (\ListResiliencyPolicies' {maxResults} -> maxResults) (\s@ListResiliencyPolicies' {} a -> s {maxResults = a} :: ListResiliencyPolicies)

-- | Null, or the token from a previous call to get the next set of results.
listResiliencyPolicies_nextToken :: Lens.Lens' ListResiliencyPolicies (Prelude.Maybe Prelude.Text)
listResiliencyPolicies_nextToken = Lens.lens (\ListResiliencyPolicies' {nextToken} -> nextToken) (\s@ListResiliencyPolicies' {} a -> s {nextToken = a} :: ListResiliencyPolicies)

-- | The name of the policy
listResiliencyPolicies_policyName :: Lens.Lens' ListResiliencyPolicies (Prelude.Maybe Prelude.Text)
listResiliencyPolicies_policyName = Lens.lens (\ListResiliencyPolicies' {policyName} -> policyName) (\s@ListResiliencyPolicies' {} a -> s {policyName = a} :: ListResiliencyPolicies)

instance Core.AWSRequest ListResiliencyPolicies where
  type
    AWSResponse ListResiliencyPolicies =
      ListResiliencyPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResiliencyPoliciesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "resiliencyPolicies"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListResiliencyPolicies where
  hashWithSalt _salt ListResiliencyPolicies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData ListResiliencyPolicies where
  rnf ListResiliencyPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders ListResiliencyPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListResiliencyPolicies where
  toPath = Prelude.const "/list-resiliency-policies"

instance Data.ToQuery ListResiliencyPolicies where
  toQuery ListResiliencyPolicies' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "policyName" Data.=: policyName
      ]

-- | /See:/ 'newListResiliencyPoliciesResponse' smart constructor.
data ListResiliencyPoliciesResponse = ListResiliencyPoliciesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resiliency policies for the Resilience Hub applications.
    resiliencyPolicies :: [ResiliencyPolicy]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResiliencyPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResiliencyPoliciesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listResiliencyPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'resiliencyPolicies', 'listResiliencyPoliciesResponse_resiliencyPolicies' - The resiliency policies for the Resilience Hub applications.
newListResiliencyPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResiliencyPoliciesResponse
newListResiliencyPoliciesResponse pHttpStatus_ =
  ListResiliencyPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      resiliencyPolicies = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listResiliencyPoliciesResponse_nextToken :: Lens.Lens' ListResiliencyPoliciesResponse (Prelude.Maybe Prelude.Text)
listResiliencyPoliciesResponse_nextToken = Lens.lens (\ListResiliencyPoliciesResponse' {nextToken} -> nextToken) (\s@ListResiliencyPoliciesResponse' {} a -> s {nextToken = a} :: ListResiliencyPoliciesResponse)

-- | The response's http status code.
listResiliencyPoliciesResponse_httpStatus :: Lens.Lens' ListResiliencyPoliciesResponse Prelude.Int
listResiliencyPoliciesResponse_httpStatus = Lens.lens (\ListResiliencyPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListResiliencyPoliciesResponse' {} a -> s {httpStatus = a} :: ListResiliencyPoliciesResponse)

-- | The resiliency policies for the Resilience Hub applications.
listResiliencyPoliciesResponse_resiliencyPolicies :: Lens.Lens' ListResiliencyPoliciesResponse [ResiliencyPolicy]
listResiliencyPoliciesResponse_resiliencyPolicies = Lens.lens (\ListResiliencyPoliciesResponse' {resiliencyPolicies} -> resiliencyPolicies) (\s@ListResiliencyPoliciesResponse' {} a -> s {resiliencyPolicies = a} :: ListResiliencyPoliciesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListResiliencyPoliciesResponse
  where
  rnf ListResiliencyPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resiliencyPolicies
