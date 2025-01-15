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
-- Module      : Amazonka.OpenSearchServerless.ListSecurityPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about configured OpenSearch Serverless security
-- policies.
module Amazonka.OpenSearchServerless.ListSecurityPolicies
  ( -- * Creating a Request
    ListSecurityPolicies (..),
    newListSecurityPolicies,

    -- * Request Lenses
    listSecurityPolicies_maxResults,
    listSecurityPolicies_nextToken,
    listSecurityPolicies_resource,
    listSecurityPolicies_type,

    -- * Destructuring the Response
    ListSecurityPoliciesResponse (..),
    newListSecurityPoliciesResponse,

    -- * Response Lenses
    listSecurityPoliciesResponse_nextToken,
    listSecurityPoliciesResponse_securityPolicySummaries,
    listSecurityPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityPolicies' smart constructor.
data ListSecurityPolicies = ListSecurityPolicies'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results. The
    -- default is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListSecurityPolicies@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListSecurityPolicies@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Resource filters (can be collection or indexes) that policies can apply
    -- to.
    resource :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of policy.
    type' :: SecurityPolicyType
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
-- 'maxResults', 'listSecurityPolicies_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
--
-- 'nextToken', 'listSecurityPolicies_nextToken' - If your initial @ListSecurityPolicies@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListSecurityPolicies@ operations, which returns results in the next
-- page.
--
-- 'resource', 'listSecurityPolicies_resource' - Resource filters (can be collection or indexes) that policies can apply
-- to.
--
-- 'type'', 'listSecurityPolicies_type' - The type of policy.
newListSecurityPolicies ::
  -- | 'type''
  SecurityPolicyType ->
  ListSecurityPolicies
newListSecurityPolicies pType_ =
  ListSecurityPolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resource = Prelude.Nothing,
      type' = pType_
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
listSecurityPolicies_maxResults :: Lens.Lens' ListSecurityPolicies (Prelude.Maybe Prelude.Natural)
listSecurityPolicies_maxResults = Lens.lens (\ListSecurityPolicies' {maxResults} -> maxResults) (\s@ListSecurityPolicies' {} a -> s {maxResults = a} :: ListSecurityPolicies)

-- | If your initial @ListSecurityPolicies@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListSecurityPolicies@ operations, which returns results in the next
-- page.
listSecurityPolicies_nextToken :: Lens.Lens' ListSecurityPolicies (Prelude.Maybe Prelude.Text)
listSecurityPolicies_nextToken = Lens.lens (\ListSecurityPolicies' {nextToken} -> nextToken) (\s@ListSecurityPolicies' {} a -> s {nextToken = a} :: ListSecurityPolicies)

-- | Resource filters (can be collection or indexes) that policies can apply
-- to.
listSecurityPolicies_resource :: Lens.Lens' ListSecurityPolicies (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listSecurityPolicies_resource = Lens.lens (\ListSecurityPolicies' {resource} -> resource) (\s@ListSecurityPolicies' {} a -> s {resource = a} :: ListSecurityPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The type of policy.
listSecurityPolicies_type :: Lens.Lens' ListSecurityPolicies SecurityPolicyType
listSecurityPolicies_type = Lens.lens (\ListSecurityPolicies' {type'} -> type') (\s@ListSecurityPolicies' {} a -> s {type' = a} :: ListSecurityPolicies)

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
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "securityPolicySummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecurityPolicies where
  hashWithSalt _salt ListSecurityPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListSecurityPolicies where
  rnf ListSecurityPolicies' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resource `Prelude.seq`
          Prelude.rnf type'

instance Data.ToHeaders ListSecurityPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.ListSecurityPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSecurityPolicies where
  toJSON ListSecurityPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath ListSecurityPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSecurityPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecurityPoliciesResponse' smart constructor.
data ListSecurityPoliciesResponse = ListSecurityPoliciesResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Details about the security policies in your account.
    securityPolicySummaries :: Prelude.Maybe [SecurityPolicySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listSecurityPoliciesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'securityPolicySummaries', 'listSecurityPoliciesResponse_securityPolicySummaries' - Details about the security policies in your account.
--
-- 'httpStatus', 'listSecurityPoliciesResponse_httpStatus' - The response's http status code.
newListSecurityPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityPoliciesResponse
newListSecurityPoliciesResponse pHttpStatus_ =
  ListSecurityPoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      securityPolicySummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listSecurityPoliciesResponse_nextToken :: Lens.Lens' ListSecurityPoliciesResponse (Prelude.Maybe Prelude.Text)
listSecurityPoliciesResponse_nextToken = Lens.lens (\ListSecurityPoliciesResponse' {nextToken} -> nextToken) (\s@ListSecurityPoliciesResponse' {} a -> s {nextToken = a} :: ListSecurityPoliciesResponse)

-- | Details about the security policies in your account.
listSecurityPoliciesResponse_securityPolicySummaries :: Lens.Lens' ListSecurityPoliciesResponse (Prelude.Maybe [SecurityPolicySummary])
listSecurityPoliciesResponse_securityPolicySummaries = Lens.lens (\ListSecurityPoliciesResponse' {securityPolicySummaries} -> securityPolicySummaries) (\s@ListSecurityPoliciesResponse' {} a -> s {securityPolicySummaries = a} :: ListSecurityPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityPoliciesResponse_httpStatus :: Lens.Lens' ListSecurityPoliciesResponse Prelude.Int
listSecurityPoliciesResponse_httpStatus = Lens.lens (\ListSecurityPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListSecurityPoliciesResponse' {} a -> s {httpStatus = a} :: ListSecurityPoliciesResponse)

instance Prelude.NFData ListSecurityPoliciesResponse where
  rnf ListSecurityPoliciesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf securityPolicySummaries `Prelude.seq`
        Prelude.rnf httpStatus
