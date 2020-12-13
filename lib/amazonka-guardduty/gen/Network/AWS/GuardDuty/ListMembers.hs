{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListMembers
  ( -- * Creating a request
    ListMembers (..),
    mkListMembers,

    -- ** Request lenses
    lmOnlyAssociated,
    lmNextToken,
    lmDetectorId,
    lmMaxResults,

    -- * Destructuring the response
    ListMembersResponse (..),
    mkListMembersResponse,

    -- ** Response lenses
    lmrsMembers,
    lmrsNextToken,
    lmrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListMembers' smart constructor.
data ListMembers = ListMembers'
  { -- | Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
    onlyAssociated :: Lude.Maybe Lude.Text,
    -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique ID of the detector the member is associated with.
    detectorId :: Lude.Text,
    -- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMembers' with the minimum fields required to make a request.
--
-- * 'onlyAssociated' - Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
-- * 'nextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
-- * 'detectorId' - The unique ID of the detector the member is associated with.
-- * 'maxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
mkListMembers ::
  -- | 'detectorId'
  Lude.Text ->
  ListMembers
mkListMembers pDetectorId_ =
  ListMembers'
    { onlyAssociated = Lude.Nothing,
      nextToken = Lude.Nothing,
      detectorId = pDetectorId_,
      maxResults = Lude.Nothing
    }

-- | Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
--
-- /Note:/ Consider using 'onlyAssociated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmOnlyAssociated :: Lens.Lens' ListMembers (Lude.Maybe Lude.Text)
lmOnlyAssociated = Lens.lens (onlyAssociated :: ListMembers -> Lude.Maybe Lude.Text) (\s a -> s {onlyAssociated = a} :: ListMembers)
{-# DEPRECATED lmOnlyAssociated "Use generic-lens or generic-optics with 'onlyAssociated' instead." #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMembers (Lude.Maybe Lude.Text)
lmNextToken = Lens.lens (nextToken :: ListMembers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMembers)
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique ID of the detector the member is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmDetectorId :: Lens.Lens' ListMembers Lude.Text
lmDetectorId = Lens.lens (detectorId :: ListMembers -> Lude.Text) (\s a -> s {detectorId = a} :: ListMembers)
{-# DEPRECATED lmDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListMembers (Lude.Maybe Lude.Natural)
lmMaxResults = Lens.lens (maxResults :: ListMembers -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListMembers)
{-# DEPRECATED lmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListMembers where
  page rq rs
    | Page.stop (rs Lens.^. lmrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lmrsMembers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lmNextToken Lens..~ rs Lens.^. lmrsNextToken

instance Lude.AWSRequest ListMembers where
  type Rs ListMembers = ListMembersResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListMembersResponse'
            Lude.<$> (x Lude..?> "members" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListMembers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListMembers where
  toPath ListMembers' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId, "/member"]

instance Lude.ToQuery ListMembers where
  toQuery ListMembers' {..} =
    Lude.mconcat
      [ "onlyAssociated" Lude.=: onlyAssociated,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { -- | A list of members.
    members :: Lude.Maybe [Member],
    -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListMembersResponse' with the minimum fields required to make a request.
--
-- * 'members' - A list of members.
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
mkListMembersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListMembersResponse
mkListMembersResponse pResponseStatus_ =
  ListMembersResponse'
    { members = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of members.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsMembers :: Lens.Lens' ListMembersResponse (Lude.Maybe [Member])
lmrsMembers = Lens.lens (members :: ListMembersResponse -> Lude.Maybe [Member]) (\s a -> s {members = a} :: ListMembersResponse)
{-# DEPRECATED lmrsMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsNextToken :: Lens.Lens' ListMembersResponse (Lude.Maybe Lude.Text)
lmrsNextToken = Lens.lens (nextToken :: ListMembersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListMembersResponse)
{-# DEPRECATED lmrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrsResponseStatus :: Lens.Lens' ListMembersResponse Lude.Int
lmrsResponseStatus = Lens.lens (responseStatus :: ListMembersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListMembersResponse)
{-# DEPRECATED lmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
