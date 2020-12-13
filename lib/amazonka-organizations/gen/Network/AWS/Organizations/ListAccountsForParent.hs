{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListAccountsForParent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the accounts in an organization that are contained by the specified target root or organizational unit (OU). If you specify the root, you get a list of all the accounts that aren't in any OU. If you specify an OU, you get a list of all the accounts in only that OU and not in any child OUs. To get a list of all accounts in the organization, use the 'ListAccounts' operation.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListAccountsForParent
  ( -- * Creating a request
    ListAccountsForParent (..),
    mkListAccountsForParent,

    -- ** Request lenses
    lafpNextToken,
    lafpMaxResults,
    lafpParentId,

    -- * Destructuring the response
    ListAccountsForParentResponse (..),
    mkListAccountsForParentResponse,

    -- ** Response lenses
    lafprsAccounts,
    lafprsNextToken,
    lafprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAccountsForParent' smart constructor.
data ListAccountsForParent = ListAccountsForParent'
  { -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
    parentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccountsForParent' with the minimum fields required to make a request.
--
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
-- * 'parentId' - The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
mkListAccountsForParent ::
  -- | 'parentId'
  Lude.Text ->
  ListAccountsForParent
mkListAccountsForParent pParentId_ =
  ListAccountsForParent'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      parentId = pParentId_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpNextToken :: Lens.Lens' ListAccountsForParent (Lude.Maybe Lude.Text)
lafpNextToken = Lens.lens (nextToken :: ListAccountsForParent -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAccountsForParent)
{-# DEPRECATED lafpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpMaxResults :: Lens.Lens' ListAccountsForParent (Lude.Maybe Lude.Natural)
lafpMaxResults = Lens.lens (maxResults :: ListAccountsForParent -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListAccountsForParent)
{-# DEPRECATED lafpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier (ID) for the parent root or organization unit (OU) whose accounts you want to list.
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafpParentId :: Lens.Lens' ListAccountsForParent Lude.Text
lafpParentId = Lens.lens (parentId :: ListAccountsForParent -> Lude.Text) (\s a -> s {parentId = a} :: ListAccountsForParent)
{-# DEPRECATED lafpParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

instance Page.AWSPager ListAccountsForParent where
  page rq rs
    | Page.stop (rs Lens.^. lafprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lafprsAccounts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lafpNextToken Lens..~ rs Lens.^. lafprsNextToken

instance Lude.AWSRequest ListAccountsForParent where
  type Rs ListAccountsForParent = ListAccountsForParentResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListAccountsForParentResponse'
            Lude.<$> (x Lude..?> "Accounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAccountsForParent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListAccountsForParent" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListAccountsForParent where
  toJSON ListAccountsForParent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ParentId" Lude..= parentId)
          ]
      )

instance Lude.ToPath ListAccountsForParent where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAccountsForParent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListAccountsForParentResponse' smart constructor.
data ListAccountsForParentResponse = ListAccountsForParentResponse'
  { -- | A list of the accounts in the specified root or OU.
    accounts :: Lude.Maybe [Account],
    -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAccountsForParentResponse' with the minimum fields required to make a request.
--
-- * 'accounts' - A list of the accounts in the specified root or OU.
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'responseStatus' - The response status code.
mkListAccountsForParentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAccountsForParentResponse
mkListAccountsForParentResponse pResponseStatus_ =
  ListAccountsForParentResponse'
    { accounts = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the accounts in the specified root or OU.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprsAccounts :: Lens.Lens' ListAccountsForParentResponse (Lude.Maybe [Account])
lafprsAccounts = Lens.lens (accounts :: ListAccountsForParentResponse -> Lude.Maybe [Account]) (\s a -> s {accounts = a} :: ListAccountsForParentResponse)
{-# DEPRECATED lafprsAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprsNextToken :: Lens.Lens' ListAccountsForParentResponse (Lude.Maybe Lude.Text)
lafprsNextToken = Lens.lens (nextToken :: ListAccountsForParentResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListAccountsForParentResponse)
{-# DEPRECATED lafprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafprsResponseStatus :: Lens.Lens' ListAccountsForParentResponse Lude.Int
lafprsResponseStatus = Lens.lens (responseStatus :: ListAccountsForParentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAccountsForParentResponse)
{-# DEPRECATED lafprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
