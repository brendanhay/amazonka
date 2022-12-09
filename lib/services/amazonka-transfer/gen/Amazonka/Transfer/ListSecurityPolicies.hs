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
-- Module      : Amazonka.Transfer.ListSecurityPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the security policies that are attached to your file transfer
-- protocol-enabled servers.
--
-- This operation returns paginated results.
module Amazonka.Transfer.ListSecurityPolicies
  ( -- * Creating a Request
    ListSecurityPolicies (..),
    newListSecurityPolicies,

    -- * Request Lenses
    listSecurityPolicies_maxResults,
    listSecurityPolicies_nextToken,

    -- * Destructuring the Response
    ListSecurityPoliciesResponse (..),
    newListSecurityPoliciesResponse,

    -- * Response Lenses
    listSecurityPoliciesResponse_nextToken,
    listSecurityPoliciesResponse_httpStatus,
    listSecurityPoliciesResponse_securityPolicyNames,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newListSecurityPolicies' smart constructor.
data ListSecurityPolicies = ListSecurityPolicies'
  { -- | Specifies the number of security policies to return as a response to the
    -- @ListSecurityPolicies@ query.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When additional results are obtained from the @ListSecurityPolicies@
    -- command, a @NextToken@ parameter is returned in the output. You can then
    -- pass the @NextToken@ parameter in a subsequent command to continue
    -- listing additional security policies.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityPolicies_maxResults' - Specifies the number of security policies to return as a response to the
-- @ListSecurityPolicies@ query.
--
-- 'nextToken', 'listSecurityPolicies_nextToken' - When additional results are obtained from the @ListSecurityPolicies@
-- command, a @NextToken@ parameter is returned in the output. You can then
-- pass the @NextToken@ parameter in a subsequent command to continue
-- listing additional security policies.
newListSecurityPolicies ::
  ListSecurityPolicies
newListSecurityPolicies =
  ListSecurityPolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies the number of security policies to return as a response to the
-- @ListSecurityPolicies@ query.
listSecurityPolicies_maxResults :: Lens.Lens' ListSecurityPolicies (Prelude.Maybe Prelude.Natural)
listSecurityPolicies_maxResults = Lens.lens (\ListSecurityPolicies' {maxResults} -> maxResults) (\s@ListSecurityPolicies' {} a -> s {maxResults = a} :: ListSecurityPolicies)

-- | When additional results are obtained from the @ListSecurityPolicies@
-- command, a @NextToken@ parameter is returned in the output. You can then
-- pass the @NextToken@ parameter in a subsequent command to continue
-- listing additional security policies.
listSecurityPolicies_nextToken :: Lens.Lens' ListSecurityPolicies (Prelude.Maybe Prelude.Text)
listSecurityPolicies_nextToken = Lens.lens (\ListSecurityPolicies' {nextToken} -> nextToken) (\s@ListSecurityPolicies' {} a -> s {nextToken = a} :: ListSecurityPolicies)

instance Core.AWSPager ListSecurityPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityPoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listSecurityPoliciesResponse_securityPolicyNames
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecurityPolicies_nextToken
          Lens..~ rs
          Lens.^? listSecurityPoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSecurityPolicies where
  type
    AWSResponse ListSecurityPolicies =
      ListSecurityPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityPoliciesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "SecurityPolicyNames"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListSecurityPolicies where
  hashWithSalt _salt ListSecurityPolicies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSecurityPolicies where
  rnf ListSecurityPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSecurityPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.ListSecurityPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSecurityPolicies where
  toJSON ListSecurityPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSecurityPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSecurityPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecurityPoliciesResponse' smart constructor.
data ListSecurityPoliciesResponse = ListSecurityPoliciesResponse'
  { -- | When you can get additional results from the @ListSecurityPolicies@
    -- operation, a @NextToken@ parameter is returned in the output. In a
    -- following command, you can pass in the @NextToken@ parameter to continue
    -- listing security policies.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of security policies that were listed.
    securityPolicyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityPoliciesResponse_nextToken' - When you can get additional results from the @ListSecurityPolicies@
-- operation, a @NextToken@ parameter is returned in the output. In a
-- following command, you can pass in the @NextToken@ parameter to continue
-- listing security policies.
--
-- 'httpStatus', 'listSecurityPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'securityPolicyNames', 'listSecurityPoliciesResponse_securityPolicyNames' - An array of security policies that were listed.
newListSecurityPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityPoliciesResponse
newListSecurityPoliciesResponse pHttpStatus_ =
  ListSecurityPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      securityPolicyNames = Prelude.mempty
    }

-- | When you can get additional results from the @ListSecurityPolicies@
-- operation, a @NextToken@ parameter is returned in the output. In a
-- following command, you can pass in the @NextToken@ parameter to continue
-- listing security policies.
listSecurityPoliciesResponse_nextToken :: Lens.Lens' ListSecurityPoliciesResponse (Prelude.Maybe Prelude.Text)
listSecurityPoliciesResponse_nextToken = Lens.lens (\ListSecurityPoliciesResponse' {nextToken} -> nextToken) (\s@ListSecurityPoliciesResponse' {} a -> s {nextToken = a} :: ListSecurityPoliciesResponse)

-- | The response's http status code.
listSecurityPoliciesResponse_httpStatus :: Lens.Lens' ListSecurityPoliciesResponse Prelude.Int
listSecurityPoliciesResponse_httpStatus = Lens.lens (\ListSecurityPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityPoliciesResponse' {} a -> s {httpStatus = a} :: ListSecurityPoliciesResponse)

-- | An array of security policies that were listed.
listSecurityPoliciesResponse_securityPolicyNames :: Lens.Lens' ListSecurityPoliciesResponse [Prelude.Text]
listSecurityPoliciesResponse_securityPolicyNames = Lens.lens (\ListSecurityPoliciesResponse' {securityPolicyNames} -> securityPolicyNames) (\s@ListSecurityPoliciesResponse' {} a -> s {securityPolicyNames = a} :: ListSecurityPoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSecurityPoliciesResponse where
  rnf ListSecurityPoliciesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf securityPolicyNames
