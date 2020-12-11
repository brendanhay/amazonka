{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListMemberAccounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @MemberAccounts@ object that lists the member accounts in the administrator's AWS organization.
--
-- The @ListMemberAccounts@ must be submitted by the account that is set as the AWS Firewall Manager administrator.
--
-- This operation returns paginated results.
module Network.AWS.FMS.ListMemberAccounts
  ( -- * Creating a request
    ListMemberAccounts (..),
    mkListMemberAccounts,

    -- ** Request lenses
    lmaNextToken,
    lmaMaxResults,

    -- * Destructuring the response
    ListMemberAccountsResponse (..),
    mkListMemberAccountsResponse,

    -- ** Response lenses
    lmarsNextToken,
    lmarsMemberAccounts,
    lmarsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMemberAccounts' smart constructor.
data ListMemberAccounts = ListMemberAccounts'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMemberAccounts' with the minimum fields required to make a request.
--
-- * 'maxResults' - Specifies the number of member account IDs that you want AWS Firewall Manager to return for this request. If you have more IDs than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of member account IDs.
-- * 'nextToken' - If you specify a value for @MaxResults@ and you have more account IDs than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of IDs. For the second and subsequent @ListMemberAccountsRequest@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of member account IDs.
mkListMemberAccounts ::
  ListMemberAccounts
mkListMemberAccounts =
  ListMemberAccounts'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | If you specify a value for @MaxResults@ and you have more account IDs than the number that you specify for @MaxResults@ , AWS Firewall Manager returns a @NextToken@ value in the response that allows you to list another group of IDs. For the second and subsequent @ListMemberAccountsRequest@ requests, specify the value of @NextToken@ from the previous response to get information about another batch of member account IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaNextToken :: Lens.Lens' ListMemberAccounts (Lude.Maybe Lude.Text)
lmaNextToken = Lens.lens (nextToken :: ListMemberAccounts -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMemberAccounts)
{-# DEPRECATED lmaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the number of member account IDs that you want AWS Firewall Manager to return for this request. If you have more IDs than the number that you specify for @MaxResults@ , the response includes a @NextToken@ value that you can use to get another batch of member account IDs.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmaMaxResults :: Lens.Lens' ListMemberAccounts (Lude.Maybe Lude.Natural)
lmaMaxResults = Lens.lens (maxResults :: ListMemberAccounts -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMemberAccounts)
{-# DEPRECATED lmaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMemberAccounts where
  page rq rs
    | Page.stop (rs Lens.^. lmarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmarsMemberAccounts) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmaNextToken Lens..~ rs Lens.^. lmarsNextToken

instance Lude.AWSRequest ListMemberAccounts where
  type Rs ListMemberAccounts = ListMemberAccountsResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMemberAccountsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "MemberAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMemberAccounts where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.ListMemberAccounts" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListMemberAccounts where
  toJSON ListMemberAccounts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListMemberAccounts where
  toPath = Lude.const "/"

instance Lude.ToQuery ListMemberAccounts where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListMemberAccountsResponse' smart constructor.
data ListMemberAccountsResponse = ListMemberAccountsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    memberAccounts ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListMemberAccountsResponse' with the minimum fields required to make a request.
--
-- * 'memberAccounts' - An array of account IDs.
-- * 'nextToken' - If you have more member account IDs than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more IDs, submit another @ListMemberAccounts@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
-- * 'responseStatus' - The response status code.
mkListMemberAccountsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMemberAccountsResponse
mkListMemberAccountsResponse pResponseStatus_ =
  ListMemberAccountsResponse'
    { nextToken = Lude.Nothing,
      memberAccounts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If you have more member account IDs than the number that you specified for @MaxResults@ in the request, the response includes a @NextToken@ value. To list more IDs, submit another @ListMemberAccounts@ request, and specify the @NextToken@ value from the response in the @NextToken@ value in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsNextToken :: Lens.Lens' ListMemberAccountsResponse (Lude.Maybe Lude.Text)
lmarsNextToken = Lens.lens (nextToken :: ListMemberAccountsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMemberAccountsResponse)
{-# DEPRECATED lmarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of account IDs.
--
-- /Note:/ Consider using 'memberAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsMemberAccounts :: Lens.Lens' ListMemberAccountsResponse (Lude.Maybe [Lude.Text])
lmarsMemberAccounts = Lens.lens (memberAccounts :: ListMemberAccountsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {memberAccounts = a} :: ListMemberAccountsResponse)
{-# DEPRECATED lmarsMemberAccounts "Use generic-lens or generic-optics with 'memberAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmarsResponseStatus :: Lens.Lens' ListMemberAccountsResponse Lude.Int
lmarsResponseStatus = Lens.lens (responseStatus :: ListMemberAccountsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMemberAccountsResponse)
{-# DEPRECATED lmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
