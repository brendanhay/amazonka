{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using a specify traffic policy version.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
  ( -- * Creating a request
    ListTrafficPolicyInstancesByPolicy (..),
    mkListTrafficPolicyInstancesByPolicy,

    -- ** Request lenses
    ltpibpTrafficPolicyInstanceTypeMarker,
    ltpibpMaxItems,
    ltpibpHostedZoneIdMarker,
    ltpibpTrafficPolicyInstanceNameMarker,
    ltpibpTrafficPolicyId,
    ltpibpTrafficPolicyVersion,

    -- * Destructuring the response
    ListTrafficPolicyInstancesByPolicyResponse (..),
    mkListTrafficPolicyInstancesByPolicyResponse,

    -- ** Response lenses
    ltpibprsTrafficPolicyInstanceTypeMarker,
    ltpibprsHostedZoneIdMarker,
    ltpibprsTrafficPolicyInstanceNameMarker,
    ltpibprsResponseStatus,
    ltpibprsTrafficPolicyInstances,
    ltpibprsIsTruncated,
    ltpibprsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains the information about the request to list your traffic policy instances.
--
-- /See:/ 'mkListTrafficPolicyInstancesByPolicy' smart constructor.
data ListTrafficPolicyInstancesByPolicy = ListTrafficPolicyInstancesByPolicy'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe RecordType,
    maxItems ::
      Lude.Maybe Lude.Text,
    hostedZoneIdMarker ::
      Lude.Maybe ResourceId,
    trafficPolicyInstanceNameMarker ::
      Lude.Maybe Lude.Text,
    trafficPolicyId ::
      Lude.Text,
    trafficPolicyVersion ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyInstancesByPolicy' with the minimum fields required to make a request.
--
-- * 'hostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'maxItems' - The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- * 'trafficPolicyId' - The ID of the traffic policy for which you want to list traffic policy instances.
-- * 'trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'trafficPolicyVersion' - The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
mkListTrafficPolicyInstancesByPolicy ::
  -- | 'trafficPolicyId'
  Lude.Text ->
  -- | 'trafficPolicyVersion'
  Lude.Natural ->
  ListTrafficPolicyInstancesByPolicy
mkListTrafficPolicyInstancesByPolicy
  pTrafficPolicyId_
  pTrafficPolicyVersion_ =
    ListTrafficPolicyInstancesByPolicy'
      { trafficPolicyInstanceTypeMarker =
          Lude.Nothing,
        maxItems = Lude.Nothing,
        hostedZoneIdMarker = Lude.Nothing,
        trafficPolicyInstanceNameMarker = Lude.Nothing,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_
      }

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Lude.Maybe RecordType)
ltpibpTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstancesByPolicy -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Lude.Maybe Lude.Text)
ltpibpMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstancesByPolicy -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Lude.Maybe ResourceId)
ltpibpHostedZoneIdMarker = Lens.lens (hostedZoneIdMarker :: ListTrafficPolicyInstancesByPolicy -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Lude.Maybe Lude.Text)
ltpibpTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstancesByPolicy -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | The ID of the traffic policy for which you want to list traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyId :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Lude.Text
ltpibpTrafficPolicyId = Lens.lens (trafficPolicyId :: ListTrafficPolicyInstancesByPolicy -> Lude.Text) (\s a -> s {trafficPolicyId = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyVersion :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Lude.Natural
ltpibpTrafficPolicyVersion = Lens.lens (trafficPolicyVersion :: ListTrafficPolicyInstancesByPolicy -> Lude.Natural) (\s a -> s {trafficPolicyVersion = a} :: ListTrafficPolicyInstancesByPolicy)
{-# DEPRECATED ltpibpTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

instance Lude.AWSRequest ListTrafficPolicyInstancesByPolicy where
  type
    Rs ListTrafficPolicyInstancesByPolicy =
      ListTrafficPolicyInstancesByPolicyResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesByPolicyResponse'
            Lude.<$> (x Lude..@? "TrafficPolicyInstanceTypeMarker")
            Lude.<*> (x Lude..@? "HostedZoneIdMarker")
            Lude.<*> (x Lude..@? "TrafficPolicyInstanceNameMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "TrafficPolicyInstances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "TrafficPolicyInstance"
                     )
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListTrafficPolicyInstancesByPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTrafficPolicyInstancesByPolicy where
  toPath =
    Lude.const "/2013-04-01/trafficpolicyinstances/trafficpolicy"

instance Lude.ToQuery ListTrafficPolicyInstancesByPolicy where
  toQuery ListTrafficPolicyInstancesByPolicy' {..} =
    Lude.mconcat
      [ "trafficpolicyinstancetype"
          Lude.=: trafficPolicyInstanceTypeMarker,
        "maxitems" Lude.=: maxItems,
        "hostedzoneid" Lude.=: hostedZoneIdMarker,
        "trafficpolicyinstancename"
          Lude.=: trafficPolicyInstanceNameMarker,
        "id" Lude.=: trafficPolicyId,
        "version" Lude.=: trafficPolicyVersion
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesByPolicyResponse' smart constructor.
data ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe
        RecordType,
    hostedZoneIdMarker ::
      Lude.Maybe
        ResourceId,
    trafficPolicyInstanceNameMarker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int,
    trafficPolicyInstances ::
      [TrafficPolicyInstance],
    isTruncated ::
      Lude.Bool,
    maxItems ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyInstancesByPolicyResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneIdMarker' - If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
-- * 'isTruncated' - A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
-- * 'maxItems' - The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
-- * 'trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
-- * 'trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
mkListTrafficPolicyInstancesByPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListTrafficPolicyInstancesByPolicyResponse
mkListTrafficPolicyInstancesByPolicyResponse
  pResponseStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesByPolicyResponse'
      { trafficPolicyInstanceTypeMarker =
          Lude.Nothing,
        hostedZoneIdMarker = Lude.Nothing,
        trafficPolicyInstanceNameMarker = Lude.Nothing,
        responseStatus = pResponseStatus_,
        trafficPolicyInstances = Lude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Lude.Maybe RecordType)
ltpibprsTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Lude.Maybe ResourceId)
ltpibprsHostedZoneIdMarker = Lens.lens (hostedZoneIdMarker :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Lude.Maybe Lude.Text)
ltpibprsTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Lude.Int
ltpibprsResponseStatus = Lens.lens (responseStatus :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse [TrafficPolicyInstance]
ltpibprsTrafficPolicyInstances = Lens.lens (trafficPolicyInstances :: ListTrafficPolicyInstancesByPolicyResponse -> [TrafficPolicyInstance]) (\s a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsTrafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead." #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Lude.Bool
ltpibprsIsTruncated = Lens.lens (isTruncated :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Lude.Text
ltpibprsMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstancesByPolicyResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstancesByPolicyResponse)
{-# DEPRECATED ltpibprsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
