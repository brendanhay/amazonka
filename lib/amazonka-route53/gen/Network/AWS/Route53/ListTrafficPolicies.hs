{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the latest version for every traffic policy that is associated with the current AWS account. Policies are listed in the order that they were created in.
--
-- For information about how of deleting a traffic policy affects the response from @ListTrafficPolicies@ , see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy> .
module Network.AWS.Route53.ListTrafficPolicies
  ( -- * Creating a request
    ListTrafficPolicies (..),
    mkListTrafficPolicies,

    -- ** Request lenses
    ltpTrafficPolicyIdMarker,
    ltpMaxItems,

    -- * Destructuring the response
    ListTrafficPoliciesResponse (..),
    mkListTrafficPoliciesResponse,

    -- ** Response lenses
    ltprsTrafficPolicySummaries,
    ltprsTrafficPolicyIdMarker,
    ltprsMaxItems,
    ltprsIsTruncated,
    ltprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains the information about the request to list the traffic policies that are associated with the current AWS account.
--
-- /See:/ 'mkListTrafficPolicies' smart constructor.
data ListTrafficPolicies = ListTrafficPolicies'
  { -- | (Conditional) For your first request to @ListTrafficPolicies@ , don't include the @TrafficPolicyIdMarker@ parameter.
    --
    -- If you have more traffic policies than the value of @MaxItems@ , @ListTrafficPolicies@ returns only the first @MaxItems@ traffic policies. To get the next group of policies, submit another request to @ListTrafficPolicies@ . For the value of @TrafficPolicyIdMarker@ , specify the value of @TrafficPolicyIdMarker@ that was returned in the previous response.
    trafficPolicyIdMarker :: Lude.Maybe Lude.Text,
    -- | (Optional) The maximum number of traffic policies that you want Amazon Route 53 to return in response to this request. If you have more than @MaxItems@ traffic policies, the value of @IsTruncated@ in the response is @true@ , and the value of @TrafficPolicyIdMarker@ is the ID of the first traffic policy that Route 53 will return if you submit another request.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicies' with the minimum fields required to make a request.
--
-- * 'trafficPolicyIdMarker' - (Conditional) For your first request to @ListTrafficPolicies@ , don't include the @TrafficPolicyIdMarker@ parameter.
--
-- If you have more traffic policies than the value of @MaxItems@ , @ListTrafficPolicies@ returns only the first @MaxItems@ traffic policies. To get the next group of policies, submit another request to @ListTrafficPolicies@ . For the value of @TrafficPolicyIdMarker@ , specify the value of @TrafficPolicyIdMarker@ that was returned in the previous response.
-- * 'maxItems' - (Optional) The maximum number of traffic policies that you want Amazon Route 53 to return in response to this request. If you have more than @MaxItems@ traffic policies, the value of @IsTruncated@ in the response is @true@ , and the value of @TrafficPolicyIdMarker@ is the ID of the first traffic policy that Route 53 will return if you submit another request.
mkListTrafficPolicies ::
  ListTrafficPolicies
mkListTrafficPolicies =
  ListTrafficPolicies'
    { trafficPolicyIdMarker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | (Conditional) For your first request to @ListTrafficPolicies@ , don't include the @TrafficPolicyIdMarker@ parameter.
--
-- If you have more traffic policies than the value of @MaxItems@ , @ListTrafficPolicies@ returns only the first @MaxItems@ traffic policies. To get the next group of policies, submit another request to @ListTrafficPolicies@ . For the value of @TrafficPolicyIdMarker@ , specify the value of @TrafficPolicyIdMarker@ that was returned in the previous response.
--
-- /Note:/ Consider using 'trafficPolicyIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpTrafficPolicyIdMarker :: Lens.Lens' ListTrafficPolicies (Lude.Maybe Lude.Text)
ltpTrafficPolicyIdMarker = Lens.lens (trafficPolicyIdMarker :: ListTrafficPolicies -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyIdMarker = a} :: ListTrafficPolicies)
{-# DEPRECATED ltpTrafficPolicyIdMarker "Use generic-lens or generic-optics with 'trafficPolicyIdMarker' instead." #-}

-- | (Optional) The maximum number of traffic policies that you want Amazon Route 53 to return in response to this request. If you have more than @MaxItems@ traffic policies, the value of @IsTruncated@ in the response is @true@ , and the value of @TrafficPolicyIdMarker@ is the ID of the first traffic policy that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpMaxItems :: Lens.Lens' ListTrafficPolicies (Lude.Maybe Lude.Text)
ltpMaxItems = Lens.lens (maxItems :: ListTrafficPolicies -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicies)
{-# DEPRECATED ltpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListTrafficPolicies where
  type Rs ListTrafficPolicies = ListTrafficPoliciesResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTrafficPoliciesResponse'
            Lude.<$> ( x Lude..@? "TrafficPolicySummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "TrafficPolicySummary"
                     )
            Lude.<*> (x Lude..@ "TrafficPolicyIdMarker")
            Lude.<*> (x Lude..@ "MaxItems")
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTrafficPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTrafficPolicies where
  toPath = Lude.const "/2013-04-01/trafficpolicies"

instance Lude.ToQuery ListTrafficPolicies where
  toQuery ListTrafficPolicies' {..} =
    Lude.mconcat
      [ "trafficpolicyid" Lude.=: trafficPolicyIdMarker,
        "maxitems" Lude.=: maxItems
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPoliciesResponse' smart constructor.
data ListTrafficPoliciesResponse = ListTrafficPoliciesResponse'
  { -- | A list that contains one @TrafficPolicySummary@ element for each traffic policy that was created by the current AWS account.
    trafficPolicySummaries :: [TrafficPolicySummary],
    -- | If the value of @IsTruncated@ is @true@ , @TrafficPolicyIdMarker@ is the ID of the first traffic policy in the next group of @MaxItems@ traffic policies.
    trafficPolicyIdMarker :: Lude.Text,
    -- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicies@ request that produced the current response.
    maxItems :: Lude.Text,
    -- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicies@ request and specifying the value of @TrafficPolicyIdMarker@ in the @TrafficPolicyIdMarker@ request parameter.
    isTruncated :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'trafficPolicySummaries' - A list that contains one @TrafficPolicySummary@ element for each traffic policy that was created by the current AWS account.
-- * 'trafficPolicyIdMarker' - If the value of @IsTruncated@ is @true@ , @TrafficPolicyIdMarker@ is the ID of the first traffic policy in the next group of @MaxItems@ traffic policies.
-- * 'maxItems' - The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicies@ request that produced the current response.
-- * 'isTruncated' - A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicies@ request and specifying the value of @TrafficPolicyIdMarker@ in the @TrafficPolicyIdMarker@ request parameter.
-- * 'responseStatus' - The response status code.
mkListTrafficPoliciesResponse ::
  -- | 'trafficPolicyIdMarker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Text ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListTrafficPoliciesResponse
mkListTrafficPoliciesResponse
  pTrafficPolicyIdMarker_
  pMaxItems_
  pIsTruncated_
  pResponseStatus_ =
    ListTrafficPoliciesResponse'
      { trafficPolicySummaries =
          Lude.mempty,
        trafficPolicyIdMarker = pTrafficPolicyIdMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        responseStatus = pResponseStatus_
      }

-- | A list that contains one @TrafficPolicySummary@ element for each traffic policy that was created by the current AWS account.
--
-- /Note:/ Consider using 'trafficPolicySummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsTrafficPolicySummaries :: Lens.Lens' ListTrafficPoliciesResponse [TrafficPolicySummary]
ltprsTrafficPolicySummaries = Lens.lens (trafficPolicySummaries :: ListTrafficPoliciesResponse -> [TrafficPolicySummary]) (\s a -> s {trafficPolicySummaries = a} :: ListTrafficPoliciesResponse)
{-# DEPRECATED ltprsTrafficPolicySummaries "Use generic-lens or generic-optics with 'trafficPolicySummaries' instead." #-}

-- | If the value of @IsTruncated@ is @true@ , @TrafficPolicyIdMarker@ is the ID of the first traffic policy in the next group of @MaxItems@ traffic policies.
--
-- /Note:/ Consider using 'trafficPolicyIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsTrafficPolicyIdMarker :: Lens.Lens' ListTrafficPoliciesResponse Lude.Text
ltprsTrafficPolicyIdMarker = Lens.lens (trafficPolicyIdMarker :: ListTrafficPoliciesResponse -> Lude.Text) (\s a -> s {trafficPolicyIdMarker = a} :: ListTrafficPoliciesResponse)
{-# DEPRECATED ltprsTrafficPolicyIdMarker "Use generic-lens or generic-optics with 'trafficPolicyIdMarker' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicies@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsMaxItems :: Lens.Lens' ListTrafficPoliciesResponse Lude.Text
ltprsMaxItems = Lens.lens (maxItems :: ListTrafficPoliciesResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPoliciesResponse)
{-# DEPRECATED ltprsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicies@ request and specifying the value of @TrafficPolicyIdMarker@ in the @TrafficPolicyIdMarker@ request parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsIsTruncated :: Lens.Lens' ListTrafficPoliciesResponse Lude.Bool
ltprsIsTruncated = Lens.lens (isTruncated :: ListTrafficPoliciesResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListTrafficPoliciesResponse)
{-# DEPRECATED ltprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprsResponseStatus :: Lens.Lens' ListTrafficPoliciesResponse Lude.Int
ltprsResponseStatus = Lens.lens (responseStatus :: ListTrafficPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrafficPoliciesResponse)
{-# DEPRECATED ltprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
