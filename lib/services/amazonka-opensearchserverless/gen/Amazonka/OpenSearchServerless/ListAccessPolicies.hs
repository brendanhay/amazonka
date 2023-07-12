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
-- Module      : Amazonka.OpenSearchServerless.ListAccessPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a list of OpenSearch Serverless access
-- policies.
module Amazonka.OpenSearchServerless.ListAccessPolicies
  ( -- * Creating a Request
    ListAccessPolicies (..),
    newListAccessPolicies,

    -- * Request Lenses
    listAccessPolicies_maxResults,
    listAccessPolicies_nextToken,
    listAccessPolicies_resource,
    listAccessPolicies_type,

    -- * Destructuring the Response
    ListAccessPoliciesResponse (..),
    newListAccessPoliciesResponse,

    -- * Response Lenses
    listAccessPoliciesResponse_accessPolicySummaries,
    listAccessPoliciesResponse_nextToken,
    listAccessPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccessPolicies' smart constructor.
data ListAccessPolicies = ListAccessPolicies'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results. The
    -- default is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If your initial @ListAccessPolicies@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListAccessPolicies@ operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Resource filters (can be collection or indexes) that policies can apply
    -- to.
    resource :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of access policy.
    type' :: AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAccessPolicies_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
--
-- 'nextToken', 'listAccessPolicies_nextToken' - If your initial @ListAccessPolicies@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListAccessPolicies@ operations, which returns results in the next page.
--
-- 'resource', 'listAccessPolicies_resource' - Resource filters (can be collection or indexes) that policies can apply
-- to.
--
-- 'type'', 'listAccessPolicies_type' - The type of access policy.
newListAccessPolicies ::
  -- | 'type''
  AccessPolicyType ->
  ListAccessPolicies
newListAccessPolicies pType_ =
  ListAccessPolicies'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resource = Prelude.Nothing,
      type' = pType_
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results. The
-- default is 20.
listAccessPolicies_maxResults :: Lens.Lens' ListAccessPolicies (Prelude.Maybe Prelude.Natural)
listAccessPolicies_maxResults = Lens.lens (\ListAccessPolicies' {maxResults} -> maxResults) (\s@ListAccessPolicies' {} a -> s {maxResults = a} :: ListAccessPolicies)

-- | If your initial @ListAccessPolicies@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListAccessPolicies@ operations, which returns results in the next page.
listAccessPolicies_nextToken :: Lens.Lens' ListAccessPolicies (Prelude.Maybe Prelude.Text)
listAccessPolicies_nextToken = Lens.lens (\ListAccessPolicies' {nextToken} -> nextToken) (\s@ListAccessPolicies' {} a -> s {nextToken = a} :: ListAccessPolicies)

-- | Resource filters (can be collection or indexes) that policies can apply
-- to.
listAccessPolicies_resource :: Lens.Lens' ListAccessPolicies (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listAccessPolicies_resource = Lens.lens (\ListAccessPolicies' {resource} -> resource) (\s@ListAccessPolicies' {} a -> s {resource = a} :: ListAccessPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The type of access policy.
listAccessPolicies_type :: Lens.Lens' ListAccessPolicies AccessPolicyType
listAccessPolicies_type = Lens.lens (\ListAccessPolicies' {type'} -> type') (\s@ListAccessPolicies' {} a -> s {type' = a} :: ListAccessPolicies)

instance Core.AWSRequest ListAccessPolicies where
  type
    AWSResponse ListAccessPolicies =
      ListAccessPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessPoliciesResponse'
            Prelude.<$> ( x
                            Data..?> "accessPolicySummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccessPolicies where
  hashWithSalt _salt ListAccessPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListAccessPolicies where
  rnf ListAccessPolicies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListAccessPolicies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.ListAccessPolicies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccessPolicies where
  toJSON ListAccessPolicies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resource" Data..=) Prelude.<$> resource,
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath ListAccessPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccessPolicies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccessPoliciesResponse' smart constructor.
data ListAccessPoliciesResponse = ListAccessPoliciesResponse'
  { -- | Details about the requested access policies.
    accessPolicySummaries :: Prelude.Maybe [AccessPolicySummary],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicySummaries', 'listAccessPoliciesResponse_accessPolicySummaries' - Details about the requested access policies.
--
-- 'nextToken', 'listAccessPoliciesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listAccessPoliciesResponse_httpStatus' - The response's http status code.
newListAccessPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccessPoliciesResponse
newListAccessPoliciesResponse pHttpStatus_ =
  ListAccessPoliciesResponse'
    { accessPolicySummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the requested access policies.
listAccessPoliciesResponse_accessPolicySummaries :: Lens.Lens' ListAccessPoliciesResponse (Prelude.Maybe [AccessPolicySummary])
listAccessPoliciesResponse_accessPolicySummaries = Lens.lens (\ListAccessPoliciesResponse' {accessPolicySummaries} -> accessPolicySummaries) (\s@ListAccessPoliciesResponse' {} a -> s {accessPolicySummaries = a} :: ListAccessPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listAccessPoliciesResponse_nextToken :: Lens.Lens' ListAccessPoliciesResponse (Prelude.Maybe Prelude.Text)
listAccessPoliciesResponse_nextToken = Lens.lens (\ListAccessPoliciesResponse' {nextToken} -> nextToken) (\s@ListAccessPoliciesResponse' {} a -> s {nextToken = a} :: ListAccessPoliciesResponse)

-- | The response's http status code.
listAccessPoliciesResponse_httpStatus :: Lens.Lens' ListAccessPoliciesResponse Prelude.Int
listAccessPoliciesResponse_httpStatus = Lens.lens (\ListAccessPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAccessPoliciesResponse' {} a -> s {httpStatus = a} :: ListAccessPoliciesResponse)

instance Prelude.NFData ListAccessPoliciesResponse where
  rnf ListAccessPoliciesResponse' {..} =
    Prelude.rnf accessPolicySummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
