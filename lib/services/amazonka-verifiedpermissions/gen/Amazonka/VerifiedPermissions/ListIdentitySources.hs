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
-- Module      : Amazonka.VerifiedPermissions.ListIdentitySources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all of the identity sources defined in the
-- specified policy store.
--
-- This operation returns paginated results.
module Amazonka.VerifiedPermissions.ListIdentitySources
  ( -- * Creating a Request
    ListIdentitySources (..),
    newListIdentitySources,

    -- * Request Lenses
    listIdentitySources_filters,
    listIdentitySources_maxResults,
    listIdentitySources_nextToken,
    listIdentitySources_policyStoreId,

    -- * Destructuring the Response
    ListIdentitySourcesResponse (..),
    newListIdentitySourcesResponse,

    -- * Response Lenses
    listIdentitySourcesResponse_nextToken,
    listIdentitySourcesResponse_httpStatus,
    listIdentitySourcesResponse_identitySources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newListIdentitySources' smart constructor.
data ListIdentitySources = ListIdentitySources'
  { -- | Specifies characteristics of an identity source that you can use to
    -- limit the output to matching identity sources.
    filters :: Prelude.Maybe [IdentitySourceFilter],
    -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the policy store that contains the identity sources
    -- that you want to list.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentitySources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listIdentitySources_filters' - Specifies characteristics of an identity source that you can use to
-- limit the output to matching identity sources.
--
-- 'maxResults', 'listIdentitySources_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'listIdentitySources_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'policyStoreId', 'listIdentitySources_policyStoreId' - Specifies the ID of the policy store that contains the identity sources
-- that you want to list.
newListIdentitySources ::
  -- | 'policyStoreId'
  Prelude.Text ->
  ListIdentitySources
newListIdentitySources pPolicyStoreId_ =
  ListIdentitySources'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_
    }

-- | Specifies characteristics of an identity source that you can use to
-- limit the output to matching identity sources.
listIdentitySources_filters :: Lens.Lens' ListIdentitySources (Prelude.Maybe [IdentitySourceFilter])
listIdentitySources_filters = Lens.lens (\ListIdentitySources' {filters} -> filters) (\s@ListIdentitySources' {} a -> s {filters = a} :: ListIdentitySources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listIdentitySources_maxResults :: Lens.Lens' ListIdentitySources (Prelude.Maybe Prelude.Natural)
listIdentitySources_maxResults = Lens.lens (\ListIdentitySources' {maxResults} -> maxResults) (\s@ListIdentitySources' {} a -> s {maxResults = a} :: ListIdentitySources)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listIdentitySources_nextToken :: Lens.Lens' ListIdentitySources (Prelude.Maybe Prelude.Text)
listIdentitySources_nextToken = Lens.lens (\ListIdentitySources' {nextToken} -> nextToken) (\s@ListIdentitySources' {} a -> s {nextToken = a} :: ListIdentitySources)

-- | Specifies the ID of the policy store that contains the identity sources
-- that you want to list.
listIdentitySources_policyStoreId :: Lens.Lens' ListIdentitySources Prelude.Text
listIdentitySources_policyStoreId = Lens.lens (\ListIdentitySources' {policyStoreId} -> policyStoreId) (\s@ListIdentitySources' {} a -> s {policyStoreId = a} :: ListIdentitySources)

instance Core.AWSPager ListIdentitySources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIdentitySourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listIdentitySourcesResponse_identitySources
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listIdentitySources_nextToken
          Lens..~ rs
          Lens.^? listIdentitySourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListIdentitySources where
  type
    AWSResponse ListIdentitySources =
      ListIdentitySourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentitySourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "identitySources"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListIdentitySources where
  hashWithSalt _salt ListIdentitySources' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData ListIdentitySources where
  rnf ListIdentitySources' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyStoreId

instance Data.ToHeaders ListIdentitySources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.ListIdentitySources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIdentitySources where
  toJSON ListIdentitySources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath ListIdentitySources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIdentitySources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIdentitySourcesResponse' smart constructor.
data ListIdentitySourcesResponse = ListIdentitySourcesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of identity sources stored in the specified policy store.
    identitySources :: [IdentitySourceItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentitySourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIdentitySourcesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'httpStatus', 'listIdentitySourcesResponse_httpStatus' - The response's http status code.
--
-- 'identitySources', 'listIdentitySourcesResponse_identitySources' - The list of identity sources stored in the specified policy store.
newListIdentitySourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentitySourcesResponse
newListIdentitySourcesResponse pHttpStatus_ =
  ListIdentitySourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      identitySources = Prelude.mempty
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listIdentitySourcesResponse_nextToken :: Lens.Lens' ListIdentitySourcesResponse (Prelude.Maybe Prelude.Text)
listIdentitySourcesResponse_nextToken = Lens.lens (\ListIdentitySourcesResponse' {nextToken} -> nextToken) (\s@ListIdentitySourcesResponse' {} a -> s {nextToken = a} :: ListIdentitySourcesResponse)

-- | The response's http status code.
listIdentitySourcesResponse_httpStatus :: Lens.Lens' ListIdentitySourcesResponse Prelude.Int
listIdentitySourcesResponse_httpStatus = Lens.lens (\ListIdentitySourcesResponse' {httpStatus} -> httpStatus) (\s@ListIdentitySourcesResponse' {} a -> s {httpStatus = a} :: ListIdentitySourcesResponse)

-- | The list of identity sources stored in the specified policy store.
listIdentitySourcesResponse_identitySources :: Lens.Lens' ListIdentitySourcesResponse [IdentitySourceItem]
listIdentitySourcesResponse_identitySources = Lens.lens (\ListIdentitySourcesResponse' {identitySources} -> identitySources) (\s@ListIdentitySourcesResponse' {} a -> s {identitySources = a} :: ListIdentitySourcesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListIdentitySourcesResponse where
  rnf ListIdentitySourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identitySources
