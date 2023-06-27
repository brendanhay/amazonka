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
-- Module      : Amazonka.VerifiedPermissions.ListPolicyTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all policy templates in the specified policy
-- store.
--
-- This operation returns paginated results.
module Amazonka.VerifiedPermissions.ListPolicyTemplates
  ( -- * Creating a Request
    ListPolicyTemplates (..),
    newListPolicyTemplates,

    -- * Request Lenses
    listPolicyTemplates_maxResults,
    listPolicyTemplates_nextToken,
    listPolicyTemplates_policyStoreId,

    -- * Destructuring the Response
    ListPolicyTemplatesResponse (..),
    newListPolicyTemplatesResponse,

    -- * Response Lenses
    listPolicyTemplatesResponse_nextToken,
    listPolicyTemplatesResponse_httpStatus,
    listPolicyTemplatesResponse_policyTemplates,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newListPolicyTemplates' smart constructor.
data ListPolicyTemplates = ListPolicyTemplates'
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the policy store that contains the policy templates
    -- you want to list.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPolicyTemplates_maxResults' - Specifies the total number of results that you want included on each
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
-- 'nextToken', 'listPolicyTemplates_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'policyStoreId', 'listPolicyTemplates_policyStoreId' - Specifies the ID of the policy store that contains the policy templates
-- you want to list.
newListPolicyTemplates ::
  -- | 'policyStoreId'
  Prelude.Text ->
  ListPolicyTemplates
newListPolicyTemplates pPolicyStoreId_ =
  ListPolicyTemplates'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      policyStoreId = pPolicyStoreId_
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
listPolicyTemplates_maxResults :: Lens.Lens' ListPolicyTemplates (Prelude.Maybe Prelude.Natural)
listPolicyTemplates_maxResults = Lens.lens (\ListPolicyTemplates' {maxResults} -> maxResults) (\s@ListPolicyTemplates' {} a -> s {maxResults = a} :: ListPolicyTemplates)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listPolicyTemplates_nextToken :: Lens.Lens' ListPolicyTemplates (Prelude.Maybe Prelude.Text)
listPolicyTemplates_nextToken = Lens.lens (\ListPolicyTemplates' {nextToken} -> nextToken) (\s@ListPolicyTemplates' {} a -> s {nextToken = a} :: ListPolicyTemplates)

-- | Specifies the ID of the policy store that contains the policy templates
-- you want to list.
listPolicyTemplates_policyStoreId :: Lens.Lens' ListPolicyTemplates Prelude.Text
listPolicyTemplates_policyStoreId = Lens.lens (\ListPolicyTemplates' {policyStoreId} -> policyStoreId) (\s@ListPolicyTemplates' {} a -> s {policyStoreId = a} :: ListPolicyTemplates)

instance Core.AWSPager ListPolicyTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listPolicyTemplatesResponse_policyTemplates
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPolicyTemplates_nextToken
          Lens..~ rs
          Lens.^? listPolicyTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyTemplates where
  type
    AWSResponse ListPolicyTemplates =
      ListPolicyTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "policyTemplates"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListPolicyTemplates where
  hashWithSalt _salt ListPolicyTemplates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData ListPolicyTemplates where
  rnf ListPolicyTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf policyStoreId

instance Data.ToHeaders ListPolicyTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.ListPolicyTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPolicyTemplates where
  toJSON ListPolicyTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath ListPolicyTemplates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPolicyTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPolicyTemplatesResponse' smart constructor.
data ListPolicyTemplatesResponse = ListPolicyTemplatesResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of the policy templates in the specified policy store.
    policyTemplates :: [PolicyTemplateItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPolicyTemplatesResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'httpStatus', 'listPolicyTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'policyTemplates', 'listPolicyTemplatesResponse_policyTemplates' - The list of the policy templates in the specified policy store.
newListPolicyTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyTemplatesResponse
newListPolicyTemplatesResponse pHttpStatus_ =
  ListPolicyTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      policyTemplates = Prelude.mempty
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listPolicyTemplatesResponse_nextToken :: Lens.Lens' ListPolicyTemplatesResponse (Prelude.Maybe Prelude.Text)
listPolicyTemplatesResponse_nextToken = Lens.lens (\ListPolicyTemplatesResponse' {nextToken} -> nextToken) (\s@ListPolicyTemplatesResponse' {} a -> s {nextToken = a} :: ListPolicyTemplatesResponse)

-- | The response's http status code.
listPolicyTemplatesResponse_httpStatus :: Lens.Lens' ListPolicyTemplatesResponse Prelude.Int
listPolicyTemplatesResponse_httpStatus = Lens.lens (\ListPolicyTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListPolicyTemplatesResponse' {} a -> s {httpStatus = a} :: ListPolicyTemplatesResponse)

-- | The list of the policy templates in the specified policy store.
listPolicyTemplatesResponse_policyTemplates :: Lens.Lens' ListPolicyTemplatesResponse [PolicyTemplateItem]
listPolicyTemplatesResponse_policyTemplates = Lens.lens (\ListPolicyTemplatesResponse' {policyTemplates} -> policyTemplates) (\s@ListPolicyTemplatesResponse' {} a -> s {policyTemplates = a} :: ListPolicyTemplatesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPolicyTemplatesResponse where
  rnf ListPolicyTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyTemplates
