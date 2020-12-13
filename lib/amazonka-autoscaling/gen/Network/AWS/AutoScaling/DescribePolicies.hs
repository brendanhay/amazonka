{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the policies for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribePolicies
  ( -- * Creating a request
    DescribePolicies (..),
    mkDescribePolicies,

    -- ** Request lenses
    dpPolicyNames,
    dpNextToken,
    dpAutoScalingGroupName,
    dpMaxRecords,
    dpPolicyTypes,

    -- * Destructuring the response
    DescribePoliciesResponse (..),
    mkDescribePoliciesResponse,

    -- ** Response lenses
    dprsNextToken,
    dprsScalingPolicies,
    dprsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePolicies' smart constructor.
data DescribePolicies = DescribePolicies'
  { -- | The names of one or more policies. If you omit this parameter, all policies are described. If a group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
    policyNames :: Lude.Maybe [Lude.Text],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to be returned with each call. The default value is @50@ and the maximum value is @100@ .
    maxRecords :: Lude.Maybe Lude.Int,
    -- | One or more policy types. The valid values are @SimpleScaling@ , @StepScaling@ , and @TargetTrackingScaling@ .
    policyTypes :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePolicies' with the minimum fields required to make a request.
--
-- * 'policyNames' - The names of one or more policies. If you omit this parameter, all policies are described. If a group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'maxRecords' - The maximum number of items to be returned with each call. The default value is @50@ and the maximum value is @100@ .
-- * 'policyTypes' - One or more policy types. The valid values are @SimpleScaling@ , @StepScaling@ , and @TargetTrackingScaling@ .
mkDescribePolicies ::
  DescribePolicies
mkDescribePolicies =
  DescribePolicies'
    { policyNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      maxRecords = Lude.Nothing,
      policyTypes = Lude.Nothing
    }

-- | The names of one or more policies. If you omit this parameter, all policies are described. If a group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyNames :: Lens.Lens' DescribePolicies (Lude.Maybe [Lude.Text])
dpPolicyNames = Lens.lens (policyNames :: DescribePolicies -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: DescribePolicies)
{-# DEPRECATED dpPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpNextToken :: Lens.Lens' DescribePolicies (Lude.Maybe Lude.Text)
dpNextToken = Lens.lens (nextToken :: DescribePolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePolicies)
{-# DEPRECATED dpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpAutoScalingGroupName :: Lens.Lens' DescribePolicies (Lude.Maybe Lude.Text)
dpAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribePolicies -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribePolicies)
{-# DEPRECATED dpAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to be returned with each call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxRecords :: Lens.Lens' DescribePolicies (Lude.Maybe Lude.Int)
dpMaxRecords = Lens.lens (maxRecords :: DescribePolicies -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribePolicies)
{-# DEPRECATED dpMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | One or more policy types. The valid values are @SimpleScaling@ , @StepScaling@ , and @TargetTrackingScaling@ .
--
-- /Note:/ Consider using 'policyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyTypes :: Lens.Lens' DescribePolicies (Lude.Maybe [Lude.Text])
dpPolicyTypes = Lens.lens (policyTypes :: DescribePolicies -> Lude.Maybe [Lude.Text]) (\s a -> s {policyTypes = a} :: DescribePolicies)
{-# DEPRECATED dpPolicyTypes "Use generic-lens or generic-optics with 'policyTypes' instead." #-}

instance Page.AWSPager DescribePolicies where
  page rq rs
    | Page.stop (rs Lens.^. dprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dprsScalingPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dpNextToken Lens..~ rs Lens.^. dprsNextToken

instance Lude.AWSRequest DescribePolicies where
  type Rs DescribePolicies = DescribePoliciesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribePoliciesResult"
      ( \s h x ->
          DescribePoliciesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "ScalingPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribePolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePolicies where
  toQuery DescribePolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribePolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "PolicyNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyNames),
        "NextToken" Lude.=: nextToken,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "MaxRecords" Lude.=: maxRecords,
        "PolicyTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyTypes)
      ]

-- | /See:/ 'mkDescribePoliciesResponse' smart constructor.
data DescribePoliciesResponse = DescribePoliciesResponse'
  { -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The scaling policies.
    scalingPolicies :: Lude.Maybe [ScalingPolicy],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'scalingPolicies' - The scaling policies.
-- * 'responseStatus' - The response status code.
mkDescribePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePoliciesResponse
mkDescribePoliciesResponse pResponseStatus_ =
  DescribePoliciesResponse'
    { nextToken = Lude.Nothing,
      scalingPolicies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsNextToken :: Lens.Lens' DescribePoliciesResponse (Lude.Maybe Lude.Text)
dprsNextToken = Lens.lens (nextToken :: DescribePoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePoliciesResponse)
{-# DEPRECATED dprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsScalingPolicies :: Lens.Lens' DescribePoliciesResponse (Lude.Maybe [ScalingPolicy])
dprsScalingPolicies = Lens.lens (scalingPolicies :: DescribePoliciesResponse -> Lude.Maybe [ScalingPolicy]) (\s a -> s {scalingPolicies = a} :: DescribePoliciesResponse)
{-# DEPRECATED dprsScalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePoliciesResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePoliciesResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
