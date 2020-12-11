{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListInvitations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all GuardDuty membership invitations that were sent to the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListInvitations
  ( -- * Creating a request
    ListInvitations (..),
    mkListInvitations,

    -- ** Request lenses
    liNextToken,
    liMaxResults,

    -- * Destructuring the response
    ListInvitationsResponse (..),
    mkListInvitationsResponse,

    -- ** Response lenses
    lirsInvitations,
    lirsNextToken,
    lirsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInvitations' smart constructor.
data ListInvitations = ListInvitations'
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

-- | Creates a value of 'ListInvitations' with the minimum fields required to make a request.
--
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
mkListInvitations ::
  ListInvitations
mkListInvitations =
  ListInvitations'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInvitations (Lude.Maybe Lude.Text)
liNextToken = Lens.lens (nextToken :: ListInvitations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInvitations)
{-# DEPRECATED liNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInvitations (Lude.Maybe Lude.Natural)
liMaxResults = Lens.lens (maxResults :: ListInvitations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInvitations)
{-# DEPRECATED liMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInvitations where
  page rq rs
    | Page.stop (rs Lens.^. lirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsInvitations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& liNextToken Lens..~ rs Lens.^. lirsNextToken

instance Lude.AWSRequest ListInvitations where
  type Rs ListInvitations = ListInvitationsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInvitationsResponse'
            Lude.<$> (x Lude..?> "invitations" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInvitations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInvitations where
  toPath = Lude.const "/invitation"

instance Lude.ToQuery ListInvitations where
  toQuery ListInvitations' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListInvitationsResponse' smart constructor.
data ListInvitationsResponse = ListInvitationsResponse'
  { invitations ::
      Lude.Maybe [Invitation],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListInvitationsResponse' with the minimum fields required to make a request.
--
-- * 'invitations' - A list of invitation descriptions.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListInvitationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInvitationsResponse
mkListInvitationsResponse pResponseStatus_ =
  ListInvitationsResponse'
    { invitations = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of invitation descriptions.
--
-- /Note:/ Consider using 'invitations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInvitations :: Lens.Lens' ListInvitationsResponse (Lude.Maybe [Invitation])
lirsInvitations = Lens.lens (invitations :: ListInvitationsResponse -> Lude.Maybe [Invitation]) (\s a -> s {invitations = a} :: ListInvitationsResponse)
{-# DEPRECATED lirsInvitations "Use generic-lens or generic-optics with 'invitations' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsNextToken :: Lens.Lens' ListInvitationsResponse (Lude.Maybe Lude.Text)
lirsNextToken = Lens.lens (nextToken :: ListInvitationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInvitationsResponse)
{-# DEPRECATED lirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInvitationsResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInvitationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInvitationsResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
