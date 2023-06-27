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
-- Module      : Amazonka.VerifiedPermissions.ListPolicyStores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all policy stores in the calling Amazon Web
-- Services account.
--
-- This operation returns paginated results.
module Amazonka.VerifiedPermissions.ListPolicyStores
  ( -- * Creating a Request
    ListPolicyStores (..),
    newListPolicyStores,

    -- * Request Lenses
    listPolicyStores_maxResults,
    listPolicyStores_nextToken,

    -- * Destructuring the Response
    ListPolicyStoresResponse (..),
    newListPolicyStoresResponse,

    -- * Response Lenses
    listPolicyStoresResponse_nextToken,
    listPolicyStoresResponse_httpStatus,
    listPolicyStoresResponse_policyStores,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newListPolicyStores' smart constructor.
data ListPolicyStores = ListPolicyStores'
  { -- | Specifies the total number of results that you want included on each
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
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPolicyStores_maxResults' - Specifies the total number of results that you want included on each
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
-- 'nextToken', 'listPolicyStores_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
newListPolicyStores ::
  ListPolicyStores
newListPolicyStores =
  ListPolicyStores'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

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
listPolicyStores_maxResults :: Lens.Lens' ListPolicyStores (Prelude.Maybe Prelude.Natural)
listPolicyStores_maxResults = Lens.lens (\ListPolicyStores' {maxResults} -> maxResults) (\s@ListPolicyStores' {} a -> s {maxResults = a} :: ListPolicyStores)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listPolicyStores_nextToken :: Lens.Lens' ListPolicyStores (Prelude.Maybe Prelude.Text)
listPolicyStores_nextToken = Lens.lens (\ListPolicyStores' {nextToken} -> nextToken) (\s@ListPolicyStores' {} a -> s {nextToken = a} :: ListPolicyStores)

instance Core.AWSPager ListPolicyStores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyStoresResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPolicyStoresResponse_policyStores) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPolicyStores_nextToken
          Lens..~ rs
          Lens.^? listPolicyStoresResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyStores where
  type
    AWSResponse ListPolicyStores =
      ListPolicyStoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyStoresResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "policyStores" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPolicyStores where
  hashWithSalt _salt ListPolicyStores' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPolicyStores where
  rnf ListPolicyStores' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPolicyStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.ListPolicyStores" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPolicyStores where
  toJSON ListPolicyStores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPolicyStores where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPolicyStores where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPolicyStoresResponse' smart constructor.
data ListPolicyStoresResponse = ListPolicyStoresResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of policy stores in the account.
    policyStores :: [PolicyStoreItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyStoresResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'httpStatus', 'listPolicyStoresResponse_httpStatus' - The response's http status code.
--
-- 'policyStores', 'listPolicyStoresResponse_policyStores' - The list of policy stores in the account.
newListPolicyStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyStoresResponse
newListPolicyStoresResponse pHttpStatus_ =
  ListPolicyStoresResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      policyStores = Prelude.mempty
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listPolicyStoresResponse_nextToken :: Lens.Lens' ListPolicyStoresResponse (Prelude.Maybe Prelude.Text)
listPolicyStoresResponse_nextToken = Lens.lens (\ListPolicyStoresResponse' {nextToken} -> nextToken) (\s@ListPolicyStoresResponse' {} a -> s {nextToken = a} :: ListPolicyStoresResponse)

-- | The response's http status code.
listPolicyStoresResponse_httpStatus :: Lens.Lens' ListPolicyStoresResponse Prelude.Int
listPolicyStoresResponse_httpStatus = Lens.lens (\ListPolicyStoresResponse' {httpStatus} -> httpStatus) (\s@ListPolicyStoresResponse' {} a -> s {httpStatus = a} :: ListPolicyStoresResponse)

-- | The list of policy stores in the account.
listPolicyStoresResponse_policyStores :: Lens.Lens' ListPolicyStoresResponse [PolicyStoreItem]
listPolicyStoresResponse_policyStores = Lens.lens (\ListPolicyStoresResponse' {policyStores} -> policyStores) (\s@ListPolicyStoresResponse' {} a -> s {policyStores = a} :: ListPolicyStoresResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPolicyStoresResponse where
  rnf ListPolicyStoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStores
