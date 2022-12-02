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
-- Module      : Amazonka.AccessAnalyzer.ListPolicyGenerations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the policy generations requested in the last seven days.
--
-- This operation returns paginated results.
module Amazonka.AccessAnalyzer.ListPolicyGenerations
  ( -- * Creating a Request
    ListPolicyGenerations (..),
    newListPolicyGenerations,

    -- * Request Lenses
    listPolicyGenerations_nextToken,
    listPolicyGenerations_principalArn,
    listPolicyGenerations_maxResults,

    -- * Destructuring the Response
    ListPolicyGenerationsResponse (..),
    newListPolicyGenerationsResponse,

    -- * Response Lenses
    listPolicyGenerationsResponse_nextToken,
    listPolicyGenerationsResponse_httpStatus,
    listPolicyGenerationsResponse_policyGenerations,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPolicyGenerations' smart constructor.
data ListPolicyGenerations = ListPolicyGenerations'
  { -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM entity (user or role) for which you are generating a
    -- policy. Use this with @ListGeneratedPolicies@ to filter the results to
    -- only include results for a specific principal.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyGenerations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyGenerations_nextToken' - A token used for pagination of results returned.
--
-- 'principalArn', 'listPolicyGenerations_principalArn' - The ARN of the IAM entity (user or role) for which you are generating a
-- policy. Use this with @ListGeneratedPolicies@ to filter the results to
-- only include results for a specific principal.
--
-- 'maxResults', 'listPolicyGenerations_maxResults' - The maximum number of results to return in the response.
newListPolicyGenerations ::
  ListPolicyGenerations
newListPolicyGenerations =
  ListPolicyGenerations'
    { nextToken = Prelude.Nothing,
      principalArn = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token used for pagination of results returned.
listPolicyGenerations_nextToken :: Lens.Lens' ListPolicyGenerations (Prelude.Maybe Prelude.Text)
listPolicyGenerations_nextToken = Lens.lens (\ListPolicyGenerations' {nextToken} -> nextToken) (\s@ListPolicyGenerations' {} a -> s {nextToken = a} :: ListPolicyGenerations)

-- | The ARN of the IAM entity (user or role) for which you are generating a
-- policy. Use this with @ListGeneratedPolicies@ to filter the results to
-- only include results for a specific principal.
listPolicyGenerations_principalArn :: Lens.Lens' ListPolicyGenerations (Prelude.Maybe Prelude.Text)
listPolicyGenerations_principalArn = Lens.lens (\ListPolicyGenerations' {principalArn} -> principalArn) (\s@ListPolicyGenerations' {} a -> s {principalArn = a} :: ListPolicyGenerations)

-- | The maximum number of results to return in the response.
listPolicyGenerations_maxResults :: Lens.Lens' ListPolicyGenerations (Prelude.Maybe Prelude.Natural)
listPolicyGenerations_maxResults = Lens.lens (\ListPolicyGenerations' {maxResults} -> maxResults) (\s@ListPolicyGenerations' {} a -> s {maxResults = a} :: ListPolicyGenerations)

instance Core.AWSPager ListPolicyGenerations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyGenerationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listPolicyGenerationsResponse_policyGenerations
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicyGenerations_nextToken
          Lens..~ rs
          Lens.^? listPolicyGenerationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyGenerations where
  type
    AWSResponse ListPolicyGenerations =
      ListPolicyGenerationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyGenerationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "policyGenerations"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPolicyGenerations where
  hashWithSalt _salt ListPolicyGenerations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPolicyGenerations where
  rnf ListPolicyGenerations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListPolicyGenerations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPolicyGenerations where
  toPath = Prelude.const "/policy/generation"

instance Data.ToQuery ListPolicyGenerations where
  toQuery ListPolicyGenerations' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "principalArn" Data.=: principalArn,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListPolicyGenerationsResponse' smart constructor.
data ListPolicyGenerationsResponse = ListPolicyGenerationsResponse'
  { -- | A token used for pagination of results returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @PolicyGeneration@ object that contains details about the generated
    -- policy.
    policyGenerations :: [PolicyGeneration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyGenerationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyGenerationsResponse_nextToken' - A token used for pagination of results returned.
--
-- 'httpStatus', 'listPolicyGenerationsResponse_httpStatus' - The response's http status code.
--
-- 'policyGenerations', 'listPolicyGenerationsResponse_policyGenerations' - A @PolicyGeneration@ object that contains details about the generated
-- policy.
newListPolicyGenerationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyGenerationsResponse
newListPolicyGenerationsResponse pHttpStatus_ =
  ListPolicyGenerationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      policyGenerations = Prelude.mempty
    }

-- | A token used for pagination of results returned.
listPolicyGenerationsResponse_nextToken :: Lens.Lens' ListPolicyGenerationsResponse (Prelude.Maybe Prelude.Text)
listPolicyGenerationsResponse_nextToken = Lens.lens (\ListPolicyGenerationsResponse' {nextToken} -> nextToken) (\s@ListPolicyGenerationsResponse' {} a -> s {nextToken = a} :: ListPolicyGenerationsResponse)

-- | The response's http status code.
listPolicyGenerationsResponse_httpStatus :: Lens.Lens' ListPolicyGenerationsResponse Prelude.Int
listPolicyGenerationsResponse_httpStatus = Lens.lens (\ListPolicyGenerationsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyGenerationsResponse' {} a -> s {httpStatus = a} :: ListPolicyGenerationsResponse)

-- | A @PolicyGeneration@ object that contains details about the generated
-- policy.
listPolicyGenerationsResponse_policyGenerations :: Lens.Lens' ListPolicyGenerationsResponse [PolicyGeneration]
listPolicyGenerationsResponse_policyGenerations = Lens.lens (\ListPolicyGenerationsResponse' {policyGenerations} -> policyGenerations) (\s@ListPolicyGenerationsResponse' {} a -> s {policyGenerations = a} :: ListPolicyGenerationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPolicyGenerationsResponse where
  rnf ListPolicyGenerationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyGenerations
