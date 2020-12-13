{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingsInBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things you have added to the given billing group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInBillingGroup
  ( -- * Creating a request
    ListThingsInBillingGroup (..),
    mkListThingsInBillingGroup,

    -- ** Request lenses
    ltibgNextToken,
    ltibgBillingGroupName,
    ltibgMaxResults,

    -- * Destructuring the response
    ListThingsInBillingGroupResponse (..),
    mkListThingsInBillingGroupResponse,

    -- ** Response lenses
    ltibgrsNextToken,
    ltibgrsThings,
    ltibgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingsInBillingGroup' smart constructor.
data ListThingsInBillingGroup = ListThingsInBillingGroup'
  { -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the billing group.
    billingGroupName :: Lude.Text,
    -- | The maximum number of results to return per request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingsInBillingGroup' with the minimum fields required to make a request.
--
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'billingGroupName' - The name of the billing group.
-- * 'maxResults' - The maximum number of results to return per request.
mkListThingsInBillingGroup ::
  -- | 'billingGroupName'
  Lude.Text ->
  ListThingsInBillingGroup
mkListThingsInBillingGroup pBillingGroupName_ =
  ListThingsInBillingGroup'
    { nextToken = Lude.Nothing,
      billingGroupName = pBillingGroupName_,
      maxResults = Lude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgNextToken :: Lens.Lens' ListThingsInBillingGroup (Lude.Maybe Lude.Text)
ltibgNextToken = Lens.lens (nextToken :: ListThingsInBillingGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingsInBillingGroup)
{-# DEPRECATED ltibgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgBillingGroupName :: Lens.Lens' ListThingsInBillingGroup Lude.Text
ltibgBillingGroupName = Lens.lens (billingGroupName :: ListThingsInBillingGroup -> Lude.Text) (\s a -> s {billingGroupName = a} :: ListThingsInBillingGroup)
{-# DEPRECATED ltibgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgMaxResults :: Lens.Lens' ListThingsInBillingGroup (Lude.Maybe Lude.Natural)
ltibgMaxResults = Lens.lens (maxResults :: ListThingsInBillingGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingsInBillingGroup)
{-# DEPRECATED ltibgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListThingsInBillingGroup where
  page rq rs
    | Page.stop (rs Lens.^. ltibgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltibgrsThings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltibgNextToken Lens..~ rs Lens.^. ltibgrsNextToken

instance Lude.AWSRequest ListThingsInBillingGroup where
  type Rs ListThingsInBillingGroup = ListThingsInBillingGroupResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingsInBillingGroupResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "things" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingsInBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingsInBillingGroup where
  toPath ListThingsInBillingGroup' {..} =
    Lude.mconcat
      ["/billing-groups/", Lude.toBS billingGroupName, "/things"]

instance Lude.ToQuery ListThingsInBillingGroup where
  toQuery ListThingsInBillingGroup' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListThingsInBillingGroupResponse' smart constructor.
data ListThingsInBillingGroupResponse = ListThingsInBillingGroupResponse'
  { -- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of things in the billing group.
    things :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingsInBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results. Will not be returned if operation has returned all results.
-- * 'things' - A list of things in the billing group.
-- * 'responseStatus' - The response status code.
mkListThingsInBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingsInBillingGroupResponse
mkListThingsInBillingGroupResponse pResponseStatus_ =
  ListThingsInBillingGroupResponse'
    { nextToken = Lude.Nothing,
      things = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrsNextToken :: Lens.Lens' ListThingsInBillingGroupResponse (Lude.Maybe Lude.Text)
ltibgrsNextToken = Lens.lens (nextToken :: ListThingsInBillingGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingsInBillingGroupResponse)
{-# DEPRECATED ltibgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of things in the billing group.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrsThings :: Lens.Lens' ListThingsInBillingGroupResponse (Lude.Maybe [Lude.Text])
ltibgrsThings = Lens.lens (things :: ListThingsInBillingGroupResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {things = a} :: ListThingsInBillingGroupResponse)
{-# DEPRECATED ltibgrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltibgrsResponseStatus :: Lens.Lens' ListThingsInBillingGroupResponse Lude.Int
ltibgrsResponseStatus = Lens.lens (responseStatus :: ListThingsInBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingsInBillingGroupResponse)
{-# DEPRECATED ltibgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
