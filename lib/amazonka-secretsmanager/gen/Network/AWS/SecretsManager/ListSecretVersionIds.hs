{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ListSecretVersionIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the versions attached to the specified secret. The output does not include the @SecretString@ or @SecretBinary@ fields. By default, the list includes only versions that have at least one staging label in @VersionStage@ attached.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:ListSecretVersionIds
--
--
-- __Related operations__
--
--     * To list the secrets in an account, use 'ListSecrets' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.SecretsManager.ListSecretVersionIds
  ( -- * Creating a request
    ListSecretVersionIds (..),
    mkListSecretVersionIds,

    -- ** Request lenses
    lsviNextToken,
    lsviIncludeDeprecated,
    lsviMaxResults,
    lsviSecretId,

    -- * Destructuring the response
    ListSecretVersionIdsResponse (..),
    mkListSecretVersionIdsResponse,

    -- ** Response lenses
    lsvirsARN,
    lsvirsVersions,
    lsvirsNextToken,
    lsvirsName,
    lsvirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkListSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { nextToken ::
      Lude.Maybe Lude.Text,
    includeDeprecated :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    secretId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecretVersionIds' with the minimum fields required to make a request.
--
-- * 'includeDeprecated' - (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
-- * 'maxResults' - (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
-- * 'secretId' - The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkListSecretVersionIds ::
  -- | 'secretId'
  Lude.Text ->
  ListSecretVersionIds
mkListSecretVersionIds pSecretId_ =
  ListSecretVersionIds'
    { nextToken = Lude.Nothing,
      includeDeprecated = Lude.Nothing,
      maxResults = Lude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviNextToken :: Lens.Lens' ListSecretVersionIds (Lude.Maybe Lude.Text)
lsviNextToken = Lens.lens (nextToken :: ListSecretVersionIds -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecretVersionIds)
{-# DEPRECATED lsviNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
--
-- /Note:/ Consider using 'includeDeprecated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviIncludeDeprecated :: Lens.Lens' ListSecretVersionIds (Lude.Maybe Lude.Bool)
lsviIncludeDeprecated = Lens.lens (includeDeprecated :: ListSecretVersionIds -> Lude.Maybe Lude.Bool) (\s a -> s {includeDeprecated = a} :: ListSecretVersionIds)
{-# DEPRECATED lsviIncludeDeprecated "Use generic-lens or generic-optics with 'includeDeprecated' instead." #-}

-- | (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviMaxResults :: Lens.Lens' ListSecretVersionIds (Lude.Maybe Lude.Natural)
lsviMaxResults = Lens.lens (maxResults :: ListSecretVersionIds -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecretVersionIds)
{-# DEPRECATED lsviMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsviSecretId :: Lens.Lens' ListSecretVersionIds Lude.Text
lsviSecretId = Lens.lens (secretId :: ListSecretVersionIds -> Lude.Text) (\s a -> s {secretId = a} :: ListSecretVersionIds)
{-# DEPRECATED lsviSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Page.AWSPager ListSecretVersionIds where
  page rq rs
    | Page.stop (rs Lens.^. lsvirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsvirsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsviNextToken Lens..~ rs Lens.^. lsvirsNextToken

instance Lude.AWSRequest ListSecretVersionIds where
  type Rs ListSecretVersionIds = ListSecretVersionIdsResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecretVersionIdsResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecretVersionIds where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.ListSecretVersionIds" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSecretVersionIds where
  toJSON ListSecretVersionIds' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("IncludeDeprecated" Lude..=) Lude.<$> includeDeprecated,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("SecretId" Lude..= secretId)
          ]
      )

instance Lude.ToPath ListSecretVersionIds where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSecretVersionIds where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    versions ::
      Lude.Maybe
        [SecretVersionsListEntry],
    nextToken :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecretVersionIdsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the secret.
-- * 'name' - The friendly name of the secret.
-- * 'nextToken' - If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
-- * 'responseStatus' - The response status code.
-- * 'versions' - The list of the currently available versions of the specified secret.
mkListSecretVersionIdsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecretVersionIdsResponse
mkListSecretVersionIdsResponse pResponseStatus_ =
  ListSecretVersionIdsResponse'
    { arn = Lude.Nothing,
      versions = Lude.Nothing,
      nextToken = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) for the secret.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirsARN :: Lens.Lens' ListSecretVersionIdsResponse (Lude.Maybe Lude.Text)
lsvirsARN = Lens.lens (arn :: ListSecretVersionIdsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ListSecretVersionIdsResponse)
{-# DEPRECATED lsvirsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The list of the currently available versions of the specified secret.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirsVersions :: Lens.Lens' ListSecretVersionIdsResponse (Lude.Maybe [SecretVersionsListEntry])
lsvirsVersions = Lens.lens (versions :: ListSecretVersionIdsResponse -> Lude.Maybe [SecretVersionsListEntry]) (\s a -> s {versions = a} :: ListSecretVersionIdsResponse)
{-# DEPRECATED lsvirsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirsNextToken :: Lens.Lens' ListSecretVersionIdsResponse (Lude.Maybe Lude.Text)
lsvirsNextToken = Lens.lens (nextToken :: ListSecretVersionIdsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecretVersionIdsResponse)
{-# DEPRECATED lsvirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The friendly name of the secret.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirsName :: Lens.Lens' ListSecretVersionIdsResponse (Lude.Maybe Lude.Text)
lsvirsName = Lens.lens (name :: ListSecretVersionIdsResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ListSecretVersionIdsResponse)
{-# DEPRECATED lsvirsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsvirsResponseStatus :: Lens.Lens' ListSecretVersionIdsResponse Lude.Int
lsvirsResponseStatus = Lens.lens (responseStatus :: ListSecretVersionIdsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecretVersionIdsResponse)
{-# DEPRECATED lsvirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
