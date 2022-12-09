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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a secret. Secrets Manager uses staging labels to
-- indicate the different versions of a secret. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/getting-started.html#term_version Secrets Manager concepts: Versions>.
--
-- To list the secrets in the account, use ListSecrets.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:ListSecretVersionIds@. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
--
-- This operation returns paginated results.
module Amazonka.SecretsManager.ListSecretVersionIds
  ( -- * Creating a Request
    ListSecretVersionIds (..),
    newListSecretVersionIds,

    -- * Request Lenses
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_nextToken,
    listSecretVersionIds_secretId,

    -- * Destructuring the Response
    ListSecretVersionIdsResponse (..),
    newListSecretVersionIdsResponse,

    -- * Response Lenses
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newListSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { -- | Specifies whether to include versions of secrets that don\'t have any
    -- staging labels attached to them. Versions without staging labels are
    -- considered deprecated and are subject to deletion by Secrets Manager.
    includeDeprecated :: Prelude.Maybe Prelude.Bool,
    -- | The number of results to include in the response.
    --
    -- If there are more results available, in the response, Secrets Manager
    -- includes @NextToken@. To get the next results, call
    -- @ListSecretVersionIds@ again with the value from @NextToken@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where the output should continue from, if a
    -- previous call did not show all results. To get the next results, call
    -- @ListSecretVersionIds@ again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN or name of the secret whose versions you want to list.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
-- 'includeDeprecated', 'listSecretVersionIds_includeDeprecated' - Specifies whether to include versions of secrets that don\'t have any
-- staging labels attached to them. Versions without staging labels are
-- considered deprecated and are subject to deletion by Secrets Manager.
--
-- 'maxResults', 'listSecretVersionIds_maxResults' - The number of results to include in the response.
--
-- If there are more results available, in the response, Secrets Manager
-- includes @NextToken@. To get the next results, call
-- @ListSecretVersionIds@ again with the value from @NextToken@.
--
-- 'nextToken', 'listSecretVersionIds_nextToken' - A token that indicates where the output should continue from, if a
-- previous call did not show all results. To get the next results, call
-- @ListSecretVersionIds@ again with this value.
--
-- 'secretId', 'listSecretVersionIds_secretId' - The ARN or name of the secret whose versions you want to list.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newListSecretVersionIds ::
  -- | 'secretId'
  Prelude.Text ->
  ListSecretVersionIds
newListSecretVersionIds pSecretId_ =
  ListSecretVersionIds'
    { includeDeprecated =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | Specifies whether to include versions of secrets that don\'t have any
-- staging labels attached to them. Versions without staging labels are
-- considered deprecated and are subject to deletion by Secrets Manager.
listSecretVersionIds_includeDeprecated :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Bool)
listSecretVersionIds_includeDeprecated = Lens.lens (\ListSecretVersionIds' {includeDeprecated} -> includeDeprecated) (\s@ListSecretVersionIds' {} a -> s {includeDeprecated = a} :: ListSecretVersionIds)

-- | The number of results to include in the response.
--
-- If there are more results available, in the response, Secrets Manager
-- includes @NextToken@. To get the next results, call
-- @ListSecretVersionIds@ again with the value from @NextToken@.
listSecretVersionIds_maxResults :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Natural)
listSecretVersionIds_maxResults = Lens.lens (\ListSecretVersionIds' {maxResults} -> maxResults) (\s@ListSecretVersionIds' {} a -> s {maxResults = a} :: ListSecretVersionIds)

-- | A token that indicates where the output should continue from, if a
-- previous call did not show all results. To get the next results, call
-- @ListSecretVersionIds@ again with this value.
listSecretVersionIds_nextToken :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Text)
listSecretVersionIds_nextToken = Lens.lens (\ListSecretVersionIds' {nextToken} -> nextToken) (\s@ListSecretVersionIds' {} a -> s {nextToken = a} :: ListSecretVersionIds)

-- | The ARN or name of the secret whose versions you want to list.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecretVersionIdsResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecretVersionIds where
  hashWithSalt _salt ListSecretVersionIds' {..} =
    _salt `Prelude.hashWithSalt` includeDeprecated
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData ListSecretVersionIds where
  rnf ListSecretVersionIds' {..} =
    Prelude.rnf includeDeprecated
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf secretId

instance Data.ToHeaders ListSecretVersionIds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.ListSecretVersionIds" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSecretVersionIds where
  toJSON ListSecretVersionIds' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeDeprecated" Data..=)
              Prelude.<$> includeDeprecated,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("SecretId" Data..= secretId)
          ]
      )

instance Data.ToPath ListSecretVersionIds where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSecretVersionIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | Secrets Manager includes this value if there\'s more output available
    -- than what is included in the current response. This can occur even when
    -- the response includes no values at all, such as when you ask for a
    -- filtered view of a long list. To get the next results, call
    -- @ListSecretVersionIds@ again with this value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the versions of the secret.
    versions :: Prelude.Maybe [SecretVersionsListEntry],
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
-- 'arn', 'listSecretVersionIdsResponse_arn' - The ARN of the secret.
--
-- 'name', 'listSecretVersionIdsResponse_name' - The name of the secret.
--
-- 'nextToken', 'listSecretVersionIdsResponse_nextToken' - Secrets Manager includes this value if there\'s more output available
-- than what is included in the current response. This can occur even when
-- the response includes no values at all, such as when you ask for a
-- filtered view of a long list. To get the next results, call
-- @ListSecretVersionIds@ again with this value.
--
-- 'versions', 'listSecretVersionIdsResponse_versions' - A list of the versions of the secret.
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
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret.
listSecretVersionIdsResponse_arn :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_arn = Lens.lens (\ListSecretVersionIdsResponse' {arn} -> arn) (\s@ListSecretVersionIdsResponse' {} a -> s {arn = a} :: ListSecretVersionIdsResponse)

-- | The name of the secret.
listSecretVersionIdsResponse_name :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_name = Lens.lens (\ListSecretVersionIdsResponse' {name} -> name) (\s@ListSecretVersionIdsResponse' {} a -> s {name = a} :: ListSecretVersionIdsResponse)

-- | Secrets Manager includes this value if there\'s more output available
-- than what is included in the current response. This can occur even when
-- the response includes no values at all, such as when you ask for a
-- filtered view of a long list. To get the next results, call
-- @ListSecretVersionIds@ again with this value.
listSecretVersionIdsResponse_nextToken :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_nextToken = Lens.lens (\ListSecretVersionIdsResponse' {nextToken} -> nextToken) (\s@ListSecretVersionIdsResponse' {} a -> s {nextToken = a} :: ListSecretVersionIdsResponse)

-- | A list of the versions of the secret.
listSecretVersionIdsResponse_versions :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe [SecretVersionsListEntry])
listSecretVersionIdsResponse_versions = Lens.lens (\ListSecretVersionIdsResponse' {versions} -> versions) (\s@ListSecretVersionIdsResponse' {} a -> s {versions = a} :: ListSecretVersionIdsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecretVersionIdsResponse_httpStatus :: Lens.Lens' ListSecretVersionIdsResponse Prelude.Int
listSecretVersionIdsResponse_httpStatus = Lens.lens (\ListSecretVersionIdsResponse' {httpStatus} -> httpStatus) (\s@ListSecretVersionIdsResponse' {} a -> s {httpStatus = a} :: ListSecretVersionIdsResponse)

instance Prelude.NFData ListSecretVersionIdsResponse where
  rnf ListSecretVersionIdsResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
