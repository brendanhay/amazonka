{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ListSecrets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the secrets that are stored by Secrets Manager in the AWS account. To list the versions currently stored for a specific secret, use 'ListSecretVersionIds' . The encrypted fields @SecretString@ and @SecretBinary@ are not included in the output. To get that information, call the 'GetSecretValue' operation.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:ListSecrets
--
--
-- __Related operations__
--
--     * To list the versions attached to a secret, use 'ListSecretVersionIds' .
--
--
--
-- This operation returns paginated results.
module Network.AWS.SecretsManager.ListSecrets
  ( -- * Creating a request
    ListSecrets (..),
    mkListSecrets,

    -- ** Request lenses
    lsFilters,
    lsNextToken,
    lsSortOrder,
    lsMaxResults,

    -- * Destructuring the response
    ListSecretsResponse (..),
    mkListSecretsResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsSecretList,
    lsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkListSecrets' smart constructor.
data ListSecrets = ListSecrets'
  { -- | Lists the secret request filters.
    filters :: Lude.Maybe [Filter],
    -- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Lists secrets in the requested order.
    sortOrder :: Lude.Maybe SortOrderType,
    -- | (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecrets' with the minimum fields required to make a request.
--
-- * 'filters' - Lists the secret request filters.
-- * 'nextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
-- * 'sortOrder' - Lists secrets in the requested order.
-- * 'maxResults' - (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
mkListSecrets ::
  ListSecrets
mkListSecrets =
  ListSecrets'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Lists the secret request filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsFilters :: Lens.Lens' ListSecrets (Lude.Maybe [Filter])
lsFilters = Lens.lens (filters :: ListSecrets -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: ListSecrets)
{-# DEPRECATED lsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request indicating there's more output available. In a subsequent call, set it to the value of the previous call @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListSecrets (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListSecrets -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecrets)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Lists secrets in the requested order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsSortOrder :: Lens.Lens' ListSecrets (Lude.Maybe SortOrderType)
lsSortOrder = Lens.lens (sortOrder :: ListSecrets -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListSecrets)
{-# DEPRECATED lsSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | (Optional) Limits the number of results you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListSecrets (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListSecrets -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListSecrets)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSecrets where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsSecretList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListSecrets where
  type Rs ListSecrets = ListSecretsResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSecretsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "SecretList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSecrets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.ListSecrets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListSecrets where
  toJSON ListSecrets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SortOrder" Lude..=) Lude.<$> sortOrder,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListSecrets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListSecrets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListSecretsResponse' smart constructor.
data ListSecretsResponse = ListSecretsResponse'
  { -- | If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of the secrets in the account.
    secretList :: Lude.Maybe [SecretListEntry],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSecretsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
-- * 'secretList' - A list of the secrets in the account.
-- * 'responseStatus' - The response status code.
mkListSecretsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSecretsResponse
mkListSecretsResponse pResponseStatus_ =
  ListSecretsResponse'
    { nextToken = Lude.Nothing,
      secretList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present in the response, this value indicates that there's more output available than included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListSecretsResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListSecretsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSecretsResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the secrets in the account.
--
-- /Note:/ Consider using 'secretList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsSecretList :: Lens.Lens' ListSecretsResponse (Lude.Maybe [SecretListEntry])
lsrsSecretList = Lens.lens (secretList :: ListSecretsResponse -> Lude.Maybe [SecretListEntry]) (\s a -> s {secretList = a} :: ListSecretsResponse)
{-# DEPRECATED lsrsSecretList "Use generic-lens or generic-optics with 'secretList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListSecretsResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListSecretsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSecretsResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
