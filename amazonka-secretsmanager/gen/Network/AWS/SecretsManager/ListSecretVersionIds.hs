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
-- Module      : Network.AWS.SecretsManager.ListSecretVersionIds
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
module Network.AWS.SecretsManager.ListSecretVersionIds
  ( -- * Creating a Request
    ListSecretVersionIds (..),
    newListSecretVersionIds,

    -- * Request Lenses
    listSecretVersionIds_nextToken,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_secretId,

    -- * Destructuring the Response
    ListSecretVersionIdsResponse (..),
    newListSecretVersionIdsResponse,

    -- * Response Lenses
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newListSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { -- | (Optional) Use this parameter in a request if you receive a @NextToken@
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
    -- | (Optional) Specifies that you want the results to include versions that
    -- do not have any staging labels attached to them. Such versions are
    -- considered deprecated and are subject to deletion by Secrets Manager as
    -- needed.
    includeDeprecated :: Prelude.Maybe Prelude.Bool,
    -- | The identifier for the secret containing the versions you want to list.
    -- You can specify either the Amazon Resource Name (ARN) or the friendly
    -- name of the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
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
-- 'includeDeprecated', 'listSecretVersionIds_includeDeprecated' - (Optional) Specifies that you want the results to include versions that
-- do not have any staging labels attached to them. Such versions are
-- considered deprecated and are subject to deletion by Secrets Manager as
-- needed.
--
-- 'secretId', 'listSecretVersionIds_secretId' - The identifier for the secret containing the versions you want to list.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
newListSecretVersionIds ::
  -- | 'secretId'
  Prelude.Text ->
  ListSecretVersionIds
newListSecretVersionIds pSecretId_ =
  ListSecretVersionIds'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      includeDeprecated = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Use this parameter in a request if you receive a @NextToken@
-- response in a previous request indicating there\'s more output
-- available. In a subsequent call, set it to the value of the previous
-- call @NextToken@ response to indicate where the output should continue
-- from.
listSecretVersionIds_nextToken :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Text)
listSecretVersionIds_nextToken = Lens.lens (\ListSecretVersionIds' {nextToken} -> nextToken) (\s@ListSecretVersionIds' {} a -> s {nextToken = a} :: ListSecretVersionIds)

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

-- | (Optional) Specifies that you want the results to include versions that
-- do not have any staging labels attached to them. Such versions are
-- considered deprecated and are subject to deletion by Secrets Manager as
-- needed.
listSecretVersionIds_includeDeprecated :: Lens.Lens' ListSecretVersionIds (Prelude.Maybe Prelude.Bool)
listSecretVersionIds_includeDeprecated = Lens.lens (\ListSecretVersionIds' {includeDeprecated} -> includeDeprecated) (\s@ListSecretVersionIds' {} a -> s {includeDeprecated = a} :: ListSecretVersionIds)

-- | The identifier for the secret containing the versions you want to list.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSecretVersionIds

instance Prelude.NFData ListSecretVersionIds

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
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("IncludeDeprecated" Core..=)
              Prelude.<$> includeDeprecated,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath ListSecretVersionIds where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSecretVersionIds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { -- | If present in the response, this value indicates that there\'s more
    -- output available than included in the current response. This can occur
    -- even when the response includes no values at all, such as when you ask
    -- for a filtered view of a very long list. Use this value in the
    -- @NextToken@ request parameter in a subsequent call to the operation to
    -- continue processing and get the next part of the output. You should
    -- repeat this until the @NextToken@ response element comes back empty (as
    -- @null@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of the currently available versions of the specified secret.
    versions :: Prelude.Maybe [SecretVersionsListEntry],
    -- | The Amazon Resource Name (ARN) for the secret.
    --
    -- Secrets Manager automatically adds several random characters to the name
    -- at the end of the ARN when you initially create a secret. This affects
    -- only the ARN and not the actual friendly name. This ensures that if you
    -- create a new secret with the same name as an old secret that you
    -- previously deleted, then users with access to the old secret /don\'t/
    -- automatically get access to the new secret because the ARNs are
    -- different.
    arn :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listSecretVersionIdsResponse_nextToken' - If present in the response, this value indicates that there\'s more
-- output available than included in the current response. This can occur
-- even when the response includes no values at all, such as when you ask
-- for a filtered view of a very long list. Use this value in the
-- @NextToken@ request parameter in a subsequent call to the operation to
-- continue processing and get the next part of the output. You should
-- repeat this until the @NextToken@ response element comes back empty (as
-- @null@).
--
-- 'versions', 'listSecretVersionIdsResponse_versions' - The list of the currently available versions of the specified secret.
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
-- 'name', 'listSecretVersionIdsResponse_name' - The friendly name of the secret.
--
-- 'httpStatus', 'listSecretVersionIdsResponse_httpStatus' - The response's http status code.
newListSecretVersionIdsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecretVersionIdsResponse
newListSecretVersionIdsResponse pHttpStatus_ =
  ListSecretVersionIdsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
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
listSecretVersionIdsResponse_nextToken :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_nextToken = Lens.lens (\ListSecretVersionIdsResponse' {nextToken} -> nextToken) (\s@ListSecretVersionIdsResponse' {} a -> s {nextToken = a} :: ListSecretVersionIdsResponse)

-- | The list of the currently available versions of the specified secret.
listSecretVersionIdsResponse_versions :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe [SecretVersionsListEntry])
listSecretVersionIdsResponse_versions = Lens.lens (\ListSecretVersionIdsResponse' {versions} -> versions) (\s@ListSecretVersionIdsResponse' {} a -> s {versions = a} :: ListSecretVersionIdsResponse) Prelude.. Lens.mapping Lens._Coerce

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

-- | The friendly name of the secret.
listSecretVersionIdsResponse_name :: Lens.Lens' ListSecretVersionIdsResponse (Prelude.Maybe Prelude.Text)
listSecretVersionIdsResponse_name = Lens.lens (\ListSecretVersionIdsResponse' {name} -> name) (\s@ListSecretVersionIdsResponse' {} a -> s {name = a} :: ListSecretVersionIdsResponse)

-- | The response's http status code.
listSecretVersionIdsResponse_httpStatus :: Lens.Lens' ListSecretVersionIdsResponse Prelude.Int
listSecretVersionIdsResponse_httpStatus = Lens.lens (\ListSecretVersionIdsResponse' {httpStatus} -> httpStatus) (\s@ListSecretVersionIdsResponse' {} a -> s {httpStatus = a} :: ListSecretVersionIdsResponse)

instance Prelude.NFData ListSecretVersionIdsResponse
