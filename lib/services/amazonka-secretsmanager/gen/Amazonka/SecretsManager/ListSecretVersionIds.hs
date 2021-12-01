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
-- Module      : Amazonka.SecretsManager.ListSecretVersionIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the versions attached to the specified secret. The output
-- does not include the @SecretString@ or @SecretBinary@ fields. By
-- default, the list includes only versions that have at least one staging
-- label in @VersionStage@ attached.
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
-- -   secretsmanager:ListSecretVersionIds
--
-- __Related operations__
--
-- -   To list the secrets in an account, use ListSecrets.
--
-- This operation returns paginated results.
module Amazonka.SecretsManager.ListSecretVersionIds
  ( -- * Creating a Request
    ListSecretVersionIds (..),
    newListSecretVersionIds,

    -- * Request Lenses
    listSecretVersionIds_nextToken,
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_secretId,

    -- * Destructuring the Response
    ListSecretVersionIdsResponse (..),
    newListSecretVersionIdsResponse,

    -- * Response Lenses
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newListSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { -- | (Optional) Use this parameter in a request if you receive a @NextToken@
    -- response in a previous request indicating there\'s more output
    -- available. In a subsequent call, set it to the value of the previous
    -- call @NextToken@ response to indicate where the output should continue
    -- from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies that you want the results to include versions that
    -- do not have any staging labels attached to them. Such versions are
    -- considered deprecated and are subject to deletion by Secrets Manager as
    -- needed.
    includeDeprecated :: Prelude.Maybe Prelude.Bool,
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
    -- | The identifier for the secret containing the versions you want to list.
    -- You can specify either the Amazon Resource Name (ARN) or the friendly
    -- name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecretVersionIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecretVersionIds_nextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@
-- response in a previous request indicating there\'s more output
-- available. In a subsequent call, set it to the value of the previous
-- call @NextToken@ response to indicate where the output should continue
-- from.
--
-- 'includeDeprecated', 'listSecretVersionIds_includeDeprecated' - (Optional) Specifies that you want the results to include versions that
-- do not have any staging labels attached to them. Such versions are
-- considered deprecated and are subject to deletion by Secrets Manager as
-- needed.
--
-- 'maxResults', 'listSecretVersionIds_maxResults' - (Optional) Limits the number of results you want to include in the
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
-- 'secretId', 'listSecretVersionIds_secretId' - The identifier for the secret containing the versions you want to list.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newListSecretVersionIds ::
  -- | 'secretId'
  Prelude.Text ->
  ListSecretVersionIds
newListSecretVersionIds pSecretId_ =
  ListSecretVersionIds'
    { nextToken = Prelude.Nothing,
      includeDeprecated = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Use this parameter in a request if you receive a @NextToken@
-- response in a previous request indicating there\'s more output
-- available. In a subsequent call, set it to the value of the previous
-- call @NextToken@ response to indicate where the output should continue
-- from.
listSecretVersionIds_nextToken :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Text)
listSecretVersionIds_nextToken = Lens.lens (\ListSecretVersionIds' {nextToken} -> nextToken) (\s@ListSecretVersionIds' {} a -> s {nextToken = a} :: ListSecretVersionIds)

-- | (Optional) Specifies that you want the results to include versions that
-- do not have any staging labels attached to them. Such versions are
-- considered deprecated and are subject to deletion by Secrets Manager as
-- needed.
listSecretVersionIds_includeDeprecated :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Bool)
listSecretVersionIds_includeDeprecated = Lens.lens (\ListSecretVersionIds' {includeDeprecated} -> includeDeprecated) (\s@ListSecretVersionIds' {} a -> s {includeDeprecated = a} :: ListSecretVersionIds)

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
listSecretVersionIds_maxResults :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Natural)
listSecretVersionIds_maxResults = Lens.lens (\ListSecretVersionIds' {maxResults} -> maxResults) (\s@ListSecretVersionIds' {} a -> s {maxResults = a} :: ListSecretVersionIds)

-- | The identifier for the secret containing the versions you want to list.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
listSecretVersionIds_secretId :: Lens.Lens' ListSecretVersionIds Prelude.Text
listSecretVersionIds_secretId = Lens.lens (\ListSecretVersionIds' {secretId} -> secretId) (\s@ListSecretVersionIds' {} a -> s {secretId = a} :: ListSecretVersionIds)

instance Core.AWSPager ListSecretVersionIds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecretVersionIdsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecretVersionIdsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecretVersionIds_nextToken
          Lens..~ rs
          Lens.^? listSecretVersionIdsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSecretVersionIds where
  type
    AWSResponse ListSecretVersionIds =
      ListSecretVersionIdsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecretVersionIdsResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecretVersionIds where
  hashWithSalt salt' ListSecretVersionIds' {..} =
    salt' `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` includeDeprecated
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSecretVersionIds where
  rnf ListSecretVersionIds' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf includeDeprecated

instance Core.ToHeaders ListSecretVersionIds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.ListSecretVersionIds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSecretVersionIds where
  toJSON ListSecretVersionIds' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("IncludeDeprecated" Core..=)
              Prelude.<$> includeDeprecated,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath ListSecretVersionIds where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSecretVersionIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { -- | The Amazon Resource Name (ARN) for the secret.
    --
    -- Secrets Manager automatically adds several random characters to the name
    -- at the end of the ARN when you initially create a secret. This affects
    -- only the ARN and not the actual friendly name. This ensures that if you
    -- create a new secret with the same name as an old secret that you
    -- previously deleted, then users with access to the old secret /don\'t/
    -- automatically get access to the new secret because the ARNs are
    -- different.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The list of the currently available versions of the specified secret.
    versions :: Prelude.Maybe [SecretVersionsListEntry],
    -- | If present in the response, this value indicates that there\'s more
    -- output available than included in the current response. This can occur
    -- even when the response includes no values at all, such as when you ask
    -- for a filtered view of a very long list. Use this value in the
    -- @NextToken@ request parameter in a subsequent call to the operation to
    -- continue processing and get the next part of the output. You should
    -- repeat this until the @NextToken@ response element comes back empty (as
    -- @null@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecretVersionIdsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listSecretVersionIdsResponse_arn' - The Amazon Resource Name (ARN) for the secret.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
--
-- 'versions', 'listSecretVersionIdsResponse_versions' - The list of the currently available versions of the specified secret.
--
-- 'nextToken', 'listSecretVersionIdsResponse_nextToken' - If present in the response, this value indicates that there\'s more
-- output available than included in the current response. This can occur
-- even when the response includes no values at all, such as when you ask
-- for a filtered view of a very long list. Use this value in the
-- @NextToken@ request parameter in a subsequent call to the operation to
-- continue processing and get the next part of the output. You should
-- repeat this until the @NextToken@ response element comes back empty (as
-- @null@).
--
-- 'name', 'listSecretVersionIdsResponse_name' - The friendly name of the secret.
--
-- 'httpStatus', 'listSecretVersionIdsResponse_httpStatus' - The response's http status code.
newListSecretVersionIdsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecretVersionIdsResponse
newListSecretVersionIdsResponse pHttpStatus_ =
  ListSecretVersionIdsResponse'
    { arn =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the secret.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
listSecretVersionIdsResponse_arn :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_arn = Lens.lens (\ListSecretVersionIdsResponse' {arn} -> arn) (\s@ListSecretVersionIdsResponse' {} a -> s {arn = a} :: ListSecretVersionIdsResponse)

-- | The list of the currently available versions of the specified secret.
listSecretVersionIdsResponse_versions :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe [SecretVersionsListEntry])
listSecretVersionIdsResponse_versions = Lens.lens (\ListSecretVersionIdsResponse' {versions} -> versions) (\s@ListSecretVersionIdsResponse' {} a -> s {versions = a} :: ListSecretVersionIdsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present in the response, this value indicates that there\'s more
-- output available than included in the current response. This can occur
-- even when the response includes no values at all, such as when you ask
-- for a filtered view of a very long list. Use this value in the
-- @NextToken@ request parameter in a subsequent call to the operation to
-- continue processing and get the next part of the output. You should
-- repeat this until the @NextToken@ response element comes back empty (as
-- @null@).
listSecretVersionIdsResponse_nextToken :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_nextToken = Lens.lens (\ListSecretVersionIdsResponse' {nextToken} -> nextToken) (\s@ListSecretVersionIdsResponse' {} a -> s {nextToken = a} :: ListSecretVersionIdsResponse)

-- | The friendly name of the secret.
listSecretVersionIdsResponse_name :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_name = Lens.lens (\ListSecretVersionIdsResponse' {name} -> name) (\s@ListSecretVersionIdsResponse' {} a -> s {name = a} :: ListSecretVersionIdsResponse)

-- | The response's http status code.
listSecretVersionIdsResponse_httpStatus :: Lens.Lens' ListSecretVersionIdsResponse Prelude.Int
listSecretVersionIdsResponse_httpStatus = Lens.lens (\ListSecretVersionIdsResponse' {httpStatus} -> httpStatus) (\s@ListSecretVersionIdsResponse' {} a -> s {httpStatus = a} :: ListSecretVersionIdsResponse)

instance Prelude.NFData ListSecretVersionIdsResponse where
  rnf ListSecretVersionIdsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
