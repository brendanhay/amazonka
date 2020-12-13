{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListBillingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the billing groups you have created.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListBillingGroups
  ( -- * Creating a request
    ListBillingGroups (..),
    mkListBillingGroups,

    -- ** Request lenses
    lbgNamePrefixFilter,
    lbgNextToken,
    lbgMaxResults,

    -- * Destructuring the response
    ListBillingGroupsResponse (..),
    mkListBillingGroupsResponse,

    -- ** Response lenses
    lbgrsNextToken,
    lbgrsBillingGroups,
    lbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListBillingGroups' smart constructor.
data ListBillingGroups = ListBillingGroups'
  { -- | Limit the results to billing groups whose names have the given prefix.
    namePrefixFilter :: Lude.Maybe Lude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return per request.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBillingGroups' with the minimum fields required to make a request.
--
-- * 'namePrefixFilter' - Limit the results to billing groups whose names have the given prefix.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'maxResults' - The maximum number of results to return per request.
mkListBillingGroups ::
  ListBillingGroups
mkListBillingGroups =
  ListBillingGroups'
    { namePrefixFilter = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Limit the results to billing groups whose names have the given prefix.
--
-- /Note:/ Consider using 'namePrefixFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNamePrefixFilter :: Lens.Lens' ListBillingGroups (Lude.Maybe Lude.Text)
lbgNamePrefixFilter = Lens.lens (namePrefixFilter :: ListBillingGroups -> Lude.Maybe Lude.Text) (\s a -> s {namePrefixFilter = a} :: ListBillingGroups)
{-# DEPRECATED lbgNamePrefixFilter "Use generic-lens or generic-optics with 'namePrefixFilter' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgNextToken :: Lens.Lens' ListBillingGroups (Lude.Maybe Lude.Text)
lbgNextToken = Lens.lens (nextToken :: ListBillingGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBillingGroups)
{-# DEPRECATED lbgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgMaxResults :: Lens.Lens' ListBillingGroups (Lude.Maybe Lude.Natural)
lbgMaxResults = Lens.lens (maxResults :: ListBillingGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListBillingGroups)
{-# DEPRECATED lbgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListBillingGroups where
  page rq rs
    | Page.stop (rs Lens.^. lbgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lbgrsBillingGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lbgNextToken Lens..~ rs Lens.^. lbgrsNextToken

instance Lude.AWSRequest ListBillingGroups where
  type Rs ListBillingGroups = ListBillingGroupsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBillingGroupsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "billingGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBillingGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListBillingGroups where
  toPath = Lude.const "/billing-groups"

instance Lude.ToQuery ListBillingGroups where
  toQuery ListBillingGroups' {..} =
    Lude.mconcat
      [ "namePrefixFilter" Lude.=: namePrefixFilter,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListBillingGroupsResponse' smart constructor.
data ListBillingGroupsResponse = ListBillingGroupsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of billing groups.
    billingGroups :: Lude.Maybe [GroupNameAndARN],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBillingGroupsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'billingGroups' - The list of billing groups.
-- * 'responseStatus' - The response status code.
mkListBillingGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBillingGroupsResponse
mkListBillingGroupsResponse pResponseStatus_ =
  ListBillingGroupsResponse'
    { nextToken = Lude.Nothing,
      billingGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrsNextToken :: Lens.Lens' ListBillingGroupsResponse (Lude.Maybe Lude.Text)
lbgrsNextToken = Lens.lens (nextToken :: ListBillingGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListBillingGroupsResponse)
{-# DEPRECATED lbgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of billing groups.
--
-- /Note:/ Consider using 'billingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrsBillingGroups :: Lens.Lens' ListBillingGroupsResponse (Lude.Maybe [GroupNameAndARN])
lbgrsBillingGroups = Lens.lens (billingGroups :: ListBillingGroupsResponse -> Lude.Maybe [GroupNameAndARN]) (\s a -> s {billingGroups = a} :: ListBillingGroupsResponse)
{-# DEPRECATED lbgrsBillingGroups "Use generic-lens or generic-optics with 'billingGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbgrsResponseStatus :: Lens.Lens' ListBillingGroupsResponse Lude.Int
lbgrsResponseStatus = Lens.lens (responseStatus :: ListBillingGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBillingGroupsResponse)
{-# DEPRECATED lbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
