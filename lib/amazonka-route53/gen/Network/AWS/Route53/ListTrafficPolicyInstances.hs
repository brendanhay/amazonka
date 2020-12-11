{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using the current AWS account.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstances
  ( -- * Creating a request
    ListTrafficPolicyInstances (..),
    mkListTrafficPolicyInstances,

    -- ** Request lenses
    ltpiTrafficPolicyInstanceTypeMarker,
    ltpiMaxItems,
    ltpiHostedZoneIdMarker,
    ltpiTrafficPolicyInstanceNameMarker,

    -- * Destructuring the response
    ListTrafficPolicyInstancesResponse (..),
    mkListTrafficPolicyInstancesResponse,

    -- ** Response lenses
    ltpirsTrafficPolicyInstanceTypeMarker,
    ltpirsHostedZoneIdMarker,
    ltpirsTrafficPolicyInstanceNameMarker,
    ltpirsResponseStatus,
    ltpirsTrafficPolicyInstances,
    ltpirsIsTruncated,
    ltpirsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get information about the traffic policy instances that you created by using the current AWS account.
--
-- /See:/ 'mkListTrafficPolicyInstances' smart constructor.
data ListTrafficPolicyInstances = ListTrafficPolicyInstances'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe RecordType,
    maxItems :: Lude.Maybe Lude.Text,
    hostedZoneIdMarker ::
      Lude.Maybe ResourceId,
    trafficPolicyInstanceNameMarker ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyInstances' with the minimum fields required to make a request.
--
-- * 'hostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'maxItems' - The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
-- * 'trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
mkListTrafficPolicyInstances ::
  ListTrafficPolicyInstances
mkListTrafficPolicyInstances =
  ListTrafficPolicyInstances'
    { trafficPolicyInstanceTypeMarker =
        Lude.Nothing,
      maxItems = Lude.Nothing,
      hostedZoneIdMarker = Lude.Nothing,
      trafficPolicyInstanceNameMarker = Lude.Nothing
    }

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstances (Lude.Maybe RecordType)
ltpiTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstances -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstances)
{-# DEPRECATED ltpiTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiMaxItems :: Lens.Lens' ListTrafficPolicyInstances (Lude.Maybe Lude.Text)
ltpiMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstances -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstances)
{-# DEPRECATED ltpiMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstances (Lude.Maybe ResourceId)
ltpiHostedZoneIdMarker = Lens.lens (hostedZoneIdMarker :: ListTrafficPolicyInstances -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstances)
{-# DEPRECATED ltpiHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstances (Lude.Maybe Lude.Text)
ltpiTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstances -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstances)
{-# DEPRECATED ltpiTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

instance Lude.AWSRequest ListTrafficPolicyInstances where
  type
    Rs ListTrafficPolicyInstances =
      ListTrafficPolicyInstancesResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesResponse'
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

instance Lude.ToHeaders ListTrafficPolicyInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTrafficPolicyInstances where
  toPath = Lude.const "/2013-04-01/trafficpolicyinstances"

instance Lude.ToQuery ListTrafficPolicyInstances where
  toQuery ListTrafficPolicyInstances' {..} =
    Lude.mconcat
      [ "trafficpolicyinstancetype"
          Lude.=: trafficPolicyInstanceTypeMarker,
        "maxitems" Lude.=: maxItems,
        "hostedzoneid" Lude.=: hostedZoneIdMarker,
        "trafficpolicyinstancename"
          Lude.=: trafficPolicyInstanceNameMarker
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesResponse' smart constructor.
data ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe RecordType,
    hostedZoneIdMarker ::
      Lude.Maybe ResourceId,
    trafficPolicyInstanceNameMarker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int,
    trafficPolicyInstances ::
      [TrafficPolicyInstance],
    isTruncated ::
      Lude.Bool,
    maxItems :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyInstancesResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneIdMarker' - If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
-- * 'isTruncated' - A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
-- * 'maxItems' - The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
-- * 'trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
-- * 'trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
mkListTrafficPolicyInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListTrafficPolicyInstancesResponse
mkListTrafficPolicyInstancesResponse
  pResponseStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesResponse'
      { trafficPolicyInstanceTypeMarker =
          Lude.Nothing,
        hostedZoneIdMarker = Lude.Nothing,
        trafficPolicyInstanceNameMarker = Lude.Nothing,
        responseStatus = pResponseStatus_,
        trafficPolicyInstances = Lude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Lude.Maybe RecordType)
ltpirsTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstancesResponse -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Lude.Maybe ResourceId)
ltpirsHostedZoneIdMarker = Lens.lens (hostedZoneIdMarker :: ListTrafficPolicyInstancesResponse -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Lude.Maybe Lude.Text)
ltpirsTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesResponse Lude.Int
ltpirsResponseStatus = Lens.lens (responseStatus :: ListTrafficPolicyInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesResponse [TrafficPolicyInstance]
ltpirsTrafficPolicyInstances = Lens.lens (trafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> [TrafficPolicyInstance]) (\s a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsTrafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead." #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesResponse Lude.Bool
ltpirsIsTruncated = Lens.lens (isTruncated :: ListTrafficPolicyInstancesResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesResponse Lude.Text
ltpirsMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstancesResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstancesResponse)
{-# DEPRECATED ltpirsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
