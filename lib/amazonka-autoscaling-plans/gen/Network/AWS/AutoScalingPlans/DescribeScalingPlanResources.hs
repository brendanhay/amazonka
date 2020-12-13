{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scalable resources in the specified scaling plan.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
  ( -- * Creating a request
    DescribeScalingPlanResources (..),
    mkDescribeScalingPlanResources,

    -- ** Request lenses
    dsprScalingPlanVersion,
    dsprScalingPlanName,
    dsprNextToken,
    dsprMaxResults,

    -- * Destructuring the response
    DescribeScalingPlanResourcesResponse (..),
    mkDescribeScalingPlanResourcesResponse,

    -- ** Response lenses
    dsprrsNextToken,
    dsprrsScalingPlanResources,
    dsprrsResponseStatus,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScalingPlanResources' smart constructor.
data DescribeScalingPlanResources = DescribeScalingPlanResources'
  { -- | The version number of the scaling plan.
    scalingPlanVersion :: Lude.Integer,
    -- | The name of the scaling plan.
    scalingPlanName :: Lude.Text,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of scalable resources to return. The value must be between 1 and 50. The default value is 50.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPlanResources' with the minimum fields required to make a request.
--
-- * 'scalingPlanVersion' - The version number of the scaling plan.
-- * 'scalingPlanName' - The name of the scaling plan.
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of scalable resources to return. The value must be between 1 and 50. The default value is 50.
mkDescribeScalingPlanResources ::
  -- | 'scalingPlanVersion'
  Lude.Integer ->
  -- | 'scalingPlanName'
  Lude.Text ->
  DescribeScalingPlanResources
mkDescribeScalingPlanResources
  pScalingPlanVersion_
  pScalingPlanName_ =
    DescribeScalingPlanResources'
      { scalingPlanVersion =
          pScalingPlanVersion_,
        scalingPlanName = pScalingPlanName_,
        nextToken = Lude.Nothing,
        maxResults = Lude.Nothing
      }

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprScalingPlanVersion :: Lens.Lens' DescribeScalingPlanResources Lude.Integer
dsprScalingPlanVersion = Lens.lens (scalingPlanVersion :: DescribeScalingPlanResources -> Lude.Integer) (\s a -> s {scalingPlanVersion = a} :: DescribeScalingPlanResources)
{-# DEPRECATED dsprScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprScalingPlanName :: Lens.Lens' DescribeScalingPlanResources Lude.Text
dsprScalingPlanName = Lens.lens (scalingPlanName :: DescribeScalingPlanResources -> Lude.Text) (\s a -> s {scalingPlanName = a} :: DescribeScalingPlanResources)
{-# DEPRECATED dsprScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprNextToken :: Lens.Lens' DescribeScalingPlanResources (Lude.Maybe Lude.Text)
dsprNextToken = Lens.lens (nextToken :: DescribeScalingPlanResources -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPlanResources)
{-# DEPRECATED dsprNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of scalable resources to return. The value must be between 1 and 50. The default value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprMaxResults :: Lens.Lens' DescribeScalingPlanResources (Lude.Maybe Lude.Int)
dsprMaxResults = Lens.lens (maxResults :: DescribeScalingPlanResources -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeScalingPlanResources)
{-# DEPRECATED dsprMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeScalingPlanResources where
  page rq rs
    | Page.stop (rs Lens.^. dsprrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsprrsScalingPlanResources) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsprNextToken Lens..~ rs Lens.^. dsprrsNextToken

instance Lude.AWSRequest DescribeScalingPlanResources where
  type
    Rs DescribeScalingPlanResources =
      DescribeScalingPlanResourcesResponse
  request = Req.postJSON autoScalingPlansService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeScalingPlanResourcesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ScalingPlanResources" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScalingPlanResources where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AnyScaleScalingPlannerFrontendService.DescribeScalingPlanResources" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeScalingPlanResources where
  toJSON DescribeScalingPlanResources' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ScalingPlanVersion" Lude..= scalingPlanVersion),
            Lude.Just ("ScalingPlanName" Lude..= scalingPlanName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeScalingPlanResources where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingPlanResources where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeScalingPlanResourcesResponse' smart constructor.
data DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse'
  { -- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the scalable resources.
    scalingPlanResources :: Lude.Maybe [ScalingPlanResource],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScalingPlanResourcesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to get the next set of results. This value is @null@ if there are no more results to return.
-- * 'scalingPlanResources' - Information about the scalable resources.
-- * 'responseStatus' - The response status code.
mkDescribeScalingPlanResourcesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScalingPlanResourcesResponse
mkDescribeScalingPlanResourcesResponse pResponseStatus_ =
  DescribeScalingPlanResourcesResponse'
    { nextToken = Lude.Nothing,
      scalingPlanResources = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsNextToken :: Lens.Lens' DescribeScalingPlanResourcesResponse (Lude.Maybe Lude.Text)
dsprrsNextToken = Lens.lens (nextToken :: DescribeScalingPlanResourcesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScalingPlanResourcesResponse)
{-# DEPRECATED dsprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the scalable resources.
--
-- /Note:/ Consider using 'scalingPlanResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsScalingPlanResources :: Lens.Lens' DescribeScalingPlanResourcesResponse (Lude.Maybe [ScalingPlanResource])
dsprrsScalingPlanResources = Lens.lens (scalingPlanResources :: DescribeScalingPlanResourcesResponse -> Lude.Maybe [ScalingPlanResource]) (\s a -> s {scalingPlanResources = a} :: DescribeScalingPlanResourcesResponse)
{-# DEPRECATED dsprrsScalingPlanResources "Use generic-lens or generic-optics with 'scalingPlanResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrsResponseStatus :: Lens.Lens' DescribeScalingPlanResourcesResponse Lude.Int
dsprrsResponseStatus = Lens.lens (responseStatus :: DescribeScalingPlanResourcesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingPlanResourcesResponse)
{-# DEPRECATED dsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
