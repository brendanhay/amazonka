{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListDelegatedServicesForAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the AWS services for which the specified account is a delegated administrator.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListDelegatedServicesForAccount
  ( -- * Creating a request
    ListDelegatedServicesForAccount (..),
    mkListDelegatedServicesForAccount,

    -- ** Request lenses
    ldsfaNextToken,
    ldsfaMaxResults,
    ldsfaAccountId,

    -- * Destructuring the response
    ListDelegatedServicesForAccountResponse (..),
    mkListDelegatedServicesForAccountResponse,

    -- ** Response lenses
    ldsfarsDelegatedServices,
    ldsfarsNextToken,
    ldsfarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDelegatedServicesForAccount' smart constructor.
data ListDelegatedServicesForAccount = ListDelegatedServicesForAccount'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    accountId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDelegatedServicesForAccount' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID number of a delegated administrator account in the organization.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
mkListDelegatedServicesForAccount ::
  -- | 'accountId'
  Lude.Text ->
  ListDelegatedServicesForAccount
mkListDelegatedServicesForAccount pAccountId_ =
  ListDelegatedServicesForAccount'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      accountId = pAccountId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaNextToken :: Lens.Lens' ListDelegatedServicesForAccount (Lude.Maybe Lude.Text)
ldsfaNextToken = Lens.lens (nextToken :: ListDelegatedServicesForAccount -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDelegatedServicesForAccount)
{-# DEPRECATED ldsfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaMaxResults :: Lens.Lens' ListDelegatedServicesForAccount (Lude.Maybe Lude.Natural)
ldsfaMaxResults = Lens.lens (maxResults :: ListDelegatedServicesForAccount -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDelegatedServicesForAccount)
{-# DEPRECATED ldsfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The account ID number of a delegated administrator account in the organization.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfaAccountId :: Lens.Lens' ListDelegatedServicesForAccount Lude.Text
ldsfaAccountId = Lens.lens (accountId :: ListDelegatedServicesForAccount -> Lude.Text) (\s a -> s {accountId = a} :: ListDelegatedServicesForAccount)
{-# DEPRECATED ldsfaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Page.AWSPager ListDelegatedServicesForAccount where
  page rq rs
    | Page.stop (rs Lens.^. ldsfarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldsfarsDelegatedServices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldsfaNextToken Lens..~ rs Lens.^. ldsfarsNextToken

instance Lude.AWSRequest ListDelegatedServicesForAccount where
  type
    Rs ListDelegatedServicesForAccount =
      ListDelegatedServicesForAccountResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDelegatedServicesForAccountResponse'
            Lude.<$> (x Lude..?> "DelegatedServices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDelegatedServicesForAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListDelegatedServicesForAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDelegatedServicesForAccount where
  toJSON ListDelegatedServicesForAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("AccountId" Lude..= accountId)
          ]
      )

instance Lude.ToPath ListDelegatedServicesForAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDelegatedServicesForAccount where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDelegatedServicesForAccountResponse' smart constructor.
data ListDelegatedServicesForAccountResponse = ListDelegatedServicesForAccountResponse'
  { delegatedServices ::
      Lude.Maybe
        [DelegatedService],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDelegatedServicesForAccountResponse' with the minimum fields required to make a request.
--
-- * 'delegatedServices' - The services for which the account is a delegated administrator.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListDelegatedServicesForAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDelegatedServicesForAccountResponse
mkListDelegatedServicesForAccountResponse pResponseStatus_ =
  ListDelegatedServicesForAccountResponse'
    { delegatedServices =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The services for which the account is a delegated administrator.
--
-- /Note:/ Consider using 'delegatedServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarsDelegatedServices :: Lens.Lens' ListDelegatedServicesForAccountResponse (Lude.Maybe [DelegatedService])
ldsfarsDelegatedServices = Lens.lens (delegatedServices :: ListDelegatedServicesForAccountResponse -> Lude.Maybe [DelegatedService]) (\s a -> s {delegatedServices = a} :: ListDelegatedServicesForAccountResponse)
{-# DEPRECATED ldsfarsDelegatedServices "Use generic-lens or generic-optics with 'delegatedServices' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarsNextToken :: Lens.Lens' ListDelegatedServicesForAccountResponse (Lude.Maybe Lude.Text)
ldsfarsNextToken = Lens.lens (nextToken :: ListDelegatedServicesForAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDelegatedServicesForAccountResponse)
{-# DEPRECATED ldsfarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsfarsResponseStatus :: Lens.Lens' ListDelegatedServicesForAccountResponse Lude.Int
ldsfarsResponseStatus = Lens.lens (responseStatus :: ListDelegatedServicesForAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDelegatedServicesForAccountResponse)
{-# DEPRECATED ldsfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
