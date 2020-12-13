{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeActivations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes details about the activation, such as the date and time the activation was created, its expiration date, the IAM role assigned to the instances in the activation, and the number of instances registered by using this activation.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeActivations
  ( -- * Creating a request
    DescribeActivations (..),
    mkDescribeActivations,

    -- ** Request lenses
    daFilters,
    daNextToken,
    daMaxResults,

    -- * Destructuring the response
    DescribeActivationsResponse (..),
    mkDescribeActivationsResponse,

    -- ** Response lenses
    dasrsActivationList,
    dasrsNextToken,
    dasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeActivations' smart constructor.
data DescribeActivations = DescribeActivations'
  { -- | A filter to view information about your activations.
    filters :: Lude.Maybe [DescribeActivationsFilter],
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivations' with the minimum fields required to make a request.
--
-- * 'filters' - A filter to view information about your activations.
-- * 'nextToken' - A token to start the list. Use this token to get the next set of results.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
mkDescribeActivations ::
  DescribeActivations
mkDescribeActivations =
  DescribeActivations'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A filter to view information about your activations.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daFilters :: Lens.Lens' DescribeActivations (Lude.Maybe [DescribeActivationsFilter])
daFilters = Lens.lens (filters :: DescribeActivations -> Lude.Maybe [DescribeActivationsFilter]) (\s a -> s {filters = a} :: DescribeActivations)
{-# DEPRECATED daFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daNextToken :: Lens.Lens' DescribeActivations (Lude.Maybe Lude.Text)
daNextToken = Lens.lens (nextToken :: DescribeActivations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeActivations)
{-# DEPRECATED daNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daMaxResults :: Lens.Lens' DescribeActivations (Lude.Maybe Lude.Natural)
daMaxResults = Lens.lens (maxResults :: DescribeActivations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeActivations)
{-# DEPRECATED daMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeActivations where
  page rq rs
    | Page.stop (rs Lens.^. dasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dasrsActivationList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& daNextToken Lens..~ rs Lens.^. dasrsNextToken

instance Lude.AWSRequest DescribeActivations where
  type Rs DescribeActivations = DescribeActivationsResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeActivationsResponse'
            Lude.<$> (x Lude..?> "ActivationList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeActivations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeActivations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeActivations where
  toJSON DescribeActivations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeActivations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeActivations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeActivationsResponse' smart constructor.
data DescribeActivationsResponse = DescribeActivationsResponse'
  { -- | A list of activations for your AWS account.
    activationList :: Lude.Maybe [Activation],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeActivationsResponse' with the minimum fields required to make a request.
--
-- * 'activationList' - A list of activations for your AWS account.
-- * 'nextToken' - The token for the next set of items to return. Use this token to get the next set of results.
-- * 'responseStatus' - The response status code.
mkDescribeActivationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeActivationsResponse
mkDescribeActivationsResponse pResponseStatus_ =
  DescribeActivationsResponse'
    { activationList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of activations for your AWS account.
--
-- /Note:/ Consider using 'activationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsActivationList :: Lens.Lens' DescribeActivationsResponse (Lude.Maybe [Activation])
dasrsActivationList = Lens.lens (activationList :: DescribeActivationsResponse -> Lude.Maybe [Activation]) (\s a -> s {activationList = a} :: DescribeActivationsResponse)
{-# DEPRECATED dasrsActivationList "Use generic-lens or generic-optics with 'activationList' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsNextToken :: Lens.Lens' DescribeActivationsResponse (Lude.Maybe Lude.Text)
dasrsNextToken = Lens.lens (nextToken :: DescribeActivationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeActivationsResponse)
{-# DEPRECATED dasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DescribeActivationsResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DescribeActivationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeActivationsResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
