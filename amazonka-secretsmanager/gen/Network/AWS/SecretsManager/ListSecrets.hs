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
-- Module      : Network.AWS.SecretsManager.ListSecrets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the secrets that are stored by Secrets Manager in the AWS
-- account. To list the versions currently stored for a specific secret,
-- use ListSecretVersionIds. The encrypted fields @SecretString@ and
-- @SecretBinary@ are not included in the output. To get that information,
-- call the GetSecretValue operation.
--
-- Always check the @NextToken@ response parameter when calling any of the
-- @List*@ operations. These operations can occasionally return an empty or
-- shorter than expected list of results even when there more results
-- become available. When this happens, the @NextToken@ response parameter
-- contains a value to pass to the next call to the same API to request the
-- next part of the list.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:ListSecrets
--
-- __Related operations__
--
-- -   To list the versions attached to a secret, use ListSecretVersionIds.
--
-- This operation returns paginated results.
module Network.AWS.SecretsManager.ListSecrets
  ( -- * Creating a Request
    ListSecrets (..),
    newListSecrets,

    -- * Request Lenses
    listSecrets_sortOrder,
    listSecrets_nextToken,
    listSecrets_maxResults,
    listSecrets_filters,

    -- * Destructuring the Response
    ListSecretsResponse (..),
    newListSecretsResponse,

    -- * Response Lenses
    listSecretsResponse_nextToken,
    listSecretsResponse_secretList,
    listSecretsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newListSecrets' smart constructor.
data ListSecrets = ListSecrets'
  { -- | Lists secrets in the requested order.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | (Optional) Use this parameter in a request if you receive a @NextToken@
    -- response in a previous request indicating there\'s more output
    -- available. In a subsequent call, set it to the value of the previous
    -- call @NextToken@ response to indicate where the output should continue
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Limits the number of results you want to include in the
    -- response. If you don\'t include this parameter, it defaults to a value
    -- that\'s specific to the operation. If additional items exist beyond the
    -- maximum you specify, the @NextToken@ response element is present and has
    -- a value (isn\'t null). Include that value as the @NextToken@ request
    -- parameter in the next call to the operation to get the next part of the
    -- results. Note that Secrets Manager might return fewer results than the
    -- maximum even when there are more results available. You should check
    -- @NextToken@ after every operation to ensure that you receive all of the
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Lists the secret request filters.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecrets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listSecrets_sortOrder' - Lists secrets in the requested order.
--
-- 'nextToken', 'listSecrets_nextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@
-- response in a previous request indicating there\'s more output
-- available. In a subsequent call, set it to the value of the previous
-- call @NextToken@ response to indicate where the output should continue
-- from.
--
-- 'maxResults', 'listSecrets_maxResults' - (Optional) Limits the number of results you want to include in the
-- response. If you don\'t include this parameter, it defaults to a value
-- that\'s specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (isn\'t null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Secrets Manager might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
--
-- 'filters', 'listSecrets_filters' - Lists the secret request filters.
newListSecrets ::
  ListSecrets
newListSecrets =
  ListSecrets'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | Lists secrets in the requested order.
listSecrets_sortOrder :: Lens.Lens' ListSecrets (Prelude.Maybe SortOrderType)
listSecrets_sortOrder = Lens.lens (\ListSecrets' {sortOrder} -> sortOrder) (\s@ListSecrets' {} a -> s {sortOrder = a} :: ListSecrets)

-- | (Optional) Use this parameter in a request if you receive a @NextToken@
-- response in a previous request indicating there\'s more output
-- available. In a subsequent call, set it to the value of the previous
-- call @NextToken@ response to indicate where the output should continue
-- from.
listSecrets_nextToken :: Lens.Lens' ListSecrets (Prelude.Maybe Prelude.Text)
listSecrets_nextToken = Lens.lens (\ListSecrets' {nextToken} -> nextToken) (\s@ListSecrets' {} a -> s {nextToken = a} :: ListSecrets)

-- | (Optional) Limits the number of results you want to include in the
-- response. If you don\'t include this parameter, it defaults to a value
-- that\'s specific to the operation. If additional items exist beyond the
-- maximum you specify, the @NextToken@ response element is present and has
-- a value (isn\'t null). Include that value as the @NextToken@ request
-- parameter in the next call to the operation to get the next part of the
-- results. Note that Secrets Manager might return fewer results than the
-- maximum even when there are more results available. You should check
-- @NextToken@ after every operation to ensure that you receive all of the
-- results.
listSecrets_maxResults :: Lens.Lens' ListSecrets (Prelude.Maybe Prelude.Natural)
listSecrets_maxResults = Lens.lens (\ListSecrets' {maxResults} -> maxResults) (\s@ListSecrets' {} a -> s {maxResults = a} :: ListSecrets)

-- | Lists the secret request filters.
listSecrets_filters :: Lens.Lens' ListSecrets (Prelude.Maybe [Filter])
listSecrets_filters = Lens.lens (\ListSecrets' {filters} -> filters) (\s@ListSecrets' {} a -> s {filters = a} :: ListSecrets) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListSecrets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecretsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecretsResponse_secretList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecrets_nextToken
          Lens..~ rs
          Lens.^? listSecretsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSecrets where
  type AWSResponse ListSecrets = ListSecretsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecretsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "SecretList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecrets

instance Prelude.NFData ListSecrets

instance Core.ToHeaders ListSecrets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("secretsmanager.ListSecrets" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSecrets where
  toJSON ListSecrets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath ListSecrets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSecrets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecretsResponse' smart constructor.
data ListSecretsResponse = ListSecretsResponse'
  { -- | If present in the response, this value indicates that there\'s more
    -- output available than included in the current response. This can occur
    -- even when the response includes no values at all, such as when you ask
    -- for a filtered view of a very long list. Use this value in the
    -- @NextToken@ request parameter in a subsequent call to the operation to
    -- continue processing and get the next part of the output. You should
    -- repeat this until the @NextToken@ response element comes back empty (as
    -- @null@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the secrets in the account.
    secretList :: Prelude.Maybe [SecretListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecretsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecretsResponse_nextToken' - If present in the response, this value indicates that there\'s more
-- output available than included in the current response. This can occur
-- even when the response includes no values at all, such as when you ask
-- for a filtered view of a very long list. Use this value in the
-- @NextToken@ request parameter in a subsequent call to the operation to
-- continue processing and get the next part of the output. You should
-- repeat this until the @NextToken@ response element comes back empty (as
-- @null@).
--
-- 'secretList', 'listSecretsResponse_secretList' - A list of the secrets in the account.
--
-- 'httpStatus', 'listSecretsResponse_httpStatus' - The response's http status code.
newListSecretsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecretsResponse
newListSecretsResponse pHttpStatus_ =
  ListSecretsResponse'
    { nextToken = Prelude.Nothing,
      secretList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present in the response, this value indicates that there\'s more
-- output available than included in the current response. This can occur
-- even when the response includes no values at all, such as when you ask
-- for a filtered view of a very long list. Use this value in the
-- @NextToken@ request parameter in a subsequent call to the operation to
-- continue processing and get the next part of the output. You should
-- repeat this until the @NextToken@ response element comes back empty (as
-- @null@).
listSecretsResponse_nextToken :: Lens.Lens' ListSecretsResponse (Prelude.Maybe Prelude.Text)
listSecretsResponse_nextToken = Lens.lens (\ListSecretsResponse' {nextToken} -> nextToken) (\s@ListSecretsResponse' {} a -> s {nextToken = a} :: ListSecretsResponse)

-- | A list of the secrets in the account.
listSecretsResponse_secretList :: Lens.Lens' ListSecretsResponse (Prelude.Maybe [SecretListEntry])
listSecretsResponse_secretList = Lens.lens (\ListSecretsResponse' {secretList} -> secretList) (\s@ListSecretsResponse' {} a -> s {secretList = a} :: ListSecretsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecretsResponse_httpStatus :: Lens.Lens' ListSecretsResponse Prelude.Int
listSecretsResponse_httpStatus = Lens.lens (\ListSecretsResponse' {httpStatus} -> httpStatus) (\s@ListSecretsResponse' {} a -> s {httpStatus = a} :: ListSecretsResponse)

instance Prelude.NFData ListSecretsResponse
