{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your scaling plans.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlans
  ( -- * Creating a request
    DescribeScalingPlans (..),
    mkDescribeScalingPlans,

    -- ** Request lenses
    dScalingPlanVersion,
    dScalingPlanNames,
    dNextToken,
    dApplicationSources,
    dMaxResults,

    -- * Destructuring the response
    DescribeScalingPlansResponse (..),
    mkDescribeScalingPlansResponse,

    -- ** Response lenses
    drsScalingPlans,
    drsNextToken,
    drsResponseStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalingPlans' smart constructor.
data DescribeScalingPlans = DescribeScalingPlans'
  { scalingPlanVersion ::
      Lude.Maybe Lude.Integer,
    scalingPlanNames :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    applicationSources ::
      Lude.Maybe [ApplicationSource],
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPlans' with the minimum fields required to make a request.
--
-- * 'applicationSources' - The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
-- * 'maxResults' - The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
-- * 'nextToken' - The token for the next set of results.
-- * 'scalingPlanNames' - The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
-- * 'scalingPlanVersion' - The version number of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
mkDescribeScalingPlans ::
  DescribeScalingPlans
mkDescribeScalingPlans =
  DescribeScalingPlans'
    { scalingPlanVersion = Lude.Nothing,
      scalingPlanNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      applicationSources = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The version number of the scaling plan. If you specify a scaling plan version, you must also specify a scaling plan name.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScalingPlanVersion :: Lens.Lens' DescribeScalingPlans (Lude.Maybe Lude.Integer)
dScalingPlanVersion = Lens.lens (scalingPlanVersion :: DescribeScalingPlans -> Lude.Maybe Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: DescribeScalingPlans)
{-# DEPRECATED dScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | The names of the scaling plans (up to 10). If you specify application sources, you cannot specify scaling plan names.
--
-- /Note:/ Consider using 'scalingPlanNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScalingPlanNames :: Lens.Lens' DescribeScalingPlans (Lude.Maybe [Lude.Text])
dScalingPlanNames = Lens.lens (scalingPlanNames :: DescribeScalingPlans -> Lude.Maybe [Lude.Text]) (\s a -> s {scalingPlanNames = a} :: DescribeScalingPlans)
{-# DEPRECATED dScalingPlanNames "Use generic-lens or generic-optics with 'scalingPlanNames' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeScalingPlans (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeScalingPlans -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPlans)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sources for the applications (up to 10). If you specify scaling plan names, you cannot specify application sources.
--
-- /Note:/ Consider using 'applicationSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dApplicationSources :: Lens.Lens' DescribeScalingPlans (Lude.Maybe [ApplicationSource])
dApplicationSources = Lens.lens (applicationSources :: DescribeScalingPlans -> Lude.Maybe [ApplicationSource]) (\s a -> s {applicationSources = a} :: DescribeScalingPlans)
{-# DEPRECATED dApplicationSources "Use generic-lens or generic-optics with 'applicationSources' instead." #-}

-- | The maximum number of scalable resources to return. This value can be between 1 and 50. The default value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeScalingPlans (Lude.Maybe Lude.Int)
dMaxResults = Lens.lens (maxResults :: DescribeScalingPlans -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeScalingPlans)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeScalingPlans where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsScalingPlans) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeScalingPlans where
  type Rs DescribeScalingPlans = DescribeScalingPlansResponse
  request = Req.postJSON autoScalingPlansService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScalingPlansResponse'
            Lude.<$> (x Lude..?> "ScalingPlans" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalingPlans where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleScalingPlannerFrontendService.DescribeScalingPlans" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScalingPlans where
  toJSON DescribeScalingPlans' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScalingPlanVersion" Lude..=) Lude.<$> scalingPlanVersion,
            ("ScalingPlanNames" Lude..=) Lude.<$> scalingPlanNames,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("ApplicationSources" Lude..=) Lude.<$> applicationSources,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeScalingPlans where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingPlans where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScalingPlansResponse' smart constructor.
data DescribeScalingPlansResponse = DescribeScalingPlansResponse'
  { scalingPlans ::
      Lude.Maybe [ScalingPlan],
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

-- | Creates a value of 'DescribeScalingPlansResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'scalingPlans' - Information about the scaling plans.
mkDescribeScalingPlansResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingPlansResponse
mkDescribeScalingPlansResponse pResponseStatus_ =
  DescribeScalingPlansResponse'
    { scalingPlans = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the scaling plans.
--
-- /Note:/ Consider using 'scalingPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsScalingPlans :: Lens.Lens' DescribeScalingPlansResponse (Lude.Maybe [ScalingPlan])
drsScalingPlans = Lens.lens (scalingPlans :: DescribeScalingPlansResponse -> Lude.Maybe [ScalingPlan]) (\s a -> s {scalingPlans = a} :: DescribeScalingPlansResponse)
{-# DEPRECATED drsScalingPlans "Use generic-lens or generic-optics with 'scalingPlans' instead." #-}

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeScalingPlansResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeScalingPlansResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPlansResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeScalingPlansResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeScalingPlansResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingPlansResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
