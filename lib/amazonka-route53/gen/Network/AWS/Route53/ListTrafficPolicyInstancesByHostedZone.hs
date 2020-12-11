{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created in a specified hosted zone.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
  ( -- * Creating a request
    ListTrafficPolicyInstancesByHostedZone (..),
    mkListTrafficPolicyInstancesByHostedZone,

    -- ** Request lenses
    ltpibhzTrafficPolicyInstanceTypeMarker,
    ltpibhzMaxItems,
    ltpibhzTrafficPolicyInstanceNameMarker,
    ltpibhzHostedZoneId,

    -- * Destructuring the response
    ListTrafficPolicyInstancesByHostedZoneResponse (..),
    mkListTrafficPolicyInstancesByHostedZoneResponse,

    -- ** Response lenses
    ltpibhzrsTrafficPolicyInstanceTypeMarker,
    ltpibhzrsTrafficPolicyInstanceNameMarker,
    ltpibhzrsResponseStatus,
    ltpibhzrsTrafficPolicyInstances,
    ltpibhzrsIsTruncated,
    ltpibhzrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request for the traffic policy instances that you created in a specified hosted zone.
--
-- /See:/ 'mkListTrafficPolicyInstancesByHostedZone' smart constructor.
data ListTrafficPolicyInstancesByHostedZone = ListTrafficPolicyInstancesByHostedZone'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe
        RecordType,
    maxItems ::
      Lude.Maybe
        Lude.Text,
    trafficPolicyInstanceNameMarker ::
      Lude.Maybe
        Lude.Text,
    hostedZoneId ::
      ResourceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyInstancesByHostedZone' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that you want to list traffic policy instances for.
-- * 'maxItems' - The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- * 'trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
-- * 'trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
mkListTrafficPolicyInstancesByHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListTrafficPolicyInstancesByHostedZone
mkListTrafficPolicyInstancesByHostedZone pHostedZoneId_ =
  ListTrafficPolicyInstancesByHostedZone'
    { trafficPolicyInstanceTypeMarker =
        Lude.Nothing,
      maxItems = Lude.Nothing,
      trafficPolicyInstanceNameMarker = Lude.Nothing,
      hostedZoneId = pHostedZoneId_
    }

-- | If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Lude.Maybe RecordType)
ltpibhzTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstancesByHostedZone -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByHostedZone)
{-# DEPRECATED ltpibhzTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Lude.Maybe Lude.Text)
ltpibhzMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstancesByHostedZone -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstancesByHostedZone)
{-# DEPRECATED ltpibhzMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Lude.Maybe Lude.Text)
ltpibhzTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstancesByHostedZone -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByHostedZone)
{-# DEPRECATED ltpibhzTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | The ID of the hosted zone that you want to list traffic policy instances for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzHostedZoneId :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone ResourceId
ltpibhzHostedZoneId = Lens.lens (hostedZoneId :: ListTrafficPolicyInstancesByHostedZone -> ResourceId) (\s a -> s {hostedZoneId = a} :: ListTrafficPolicyInstancesByHostedZone)
{-# DEPRECATED ltpibhzHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

instance Lude.AWSRequest ListTrafficPolicyInstancesByHostedZone where
  type
    Rs ListTrafficPolicyInstancesByHostedZone =
      ListTrafficPolicyInstancesByHostedZoneResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesByHostedZoneResponse'
            Lude.<$> (x Lude..@? "TrafficPolicyInstanceTypeMarker")
            Lude.<*> (x Lude..@? "TrafficPolicyInstanceNameMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "TrafficPolicyInstances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "TrafficPolicyInstance"
                     )
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListTrafficPolicyInstancesByHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTrafficPolicyInstancesByHostedZone where
  toPath = Lude.const "/2013-04-01/trafficpolicyinstances/hostedzone"

instance Lude.ToQuery ListTrafficPolicyInstancesByHostedZone where
  toQuery ListTrafficPolicyInstancesByHostedZone' {..} =
    Lude.mconcat
      [ "trafficpolicyinstancetype"
          Lude.=: trafficPolicyInstanceTypeMarker,
        "maxitems" Lude.=: maxItems,
        "trafficpolicyinstancename"
          Lude.=: trafficPolicyInstanceNameMarker,
        "id" Lude.=: hostedZoneId
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesByHostedZoneResponse' smart constructor.
data ListTrafficPolicyInstancesByHostedZoneResponse = ListTrafficPolicyInstancesByHostedZoneResponse'
  { trafficPolicyInstanceTypeMarker ::
      Lude.Maybe
        RecordType,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'ListTrafficPolicyInstancesByHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by submitting another @ListTrafficPolicyInstancesByHostedZone@ request and specifying the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
-- * 'maxItems' - The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicyInstancesByHostedZone@ request that produced the current response.
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of traffic policy instances.
-- * 'trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of traffic policy instances.
-- * 'trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
mkListTrafficPolicyInstancesByHostedZoneResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListTrafficPolicyInstancesByHostedZoneResponse
mkListTrafficPolicyInstancesByHostedZoneResponse
  pResponseStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesByHostedZoneResponse'
      { trafficPolicyInstanceTypeMarker =
          Lude.Nothing,
        trafficPolicyInstanceNameMarker = Lude.Nothing,
        responseStatus = pResponseStatus_,
        trafficPolicyInstances = Lude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Lude.Maybe RecordType)
ltpibhzrsTrafficPolicyInstanceTypeMarker = Lens.lens (trafficPolicyInstanceTypeMarker :: ListTrafficPolicyInstancesByHostedZoneResponse -> Lude.Maybe RecordType) (\s a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Lude.Maybe Lude.Text)
ltpibhzrsTrafficPolicyInstanceNameMarker = Lens.lens (trafficPolicyInstanceNameMarker :: ListTrafficPolicyInstancesByHostedZoneResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Lude.Int
ltpibhzrsResponseStatus = Lens.lens (responseStatus :: ListTrafficPolicyInstancesByHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse [TrafficPolicyInstance]
ltpibhzrsTrafficPolicyInstances = Lens.lens (trafficPolicyInstances :: ListTrafficPolicyInstancesByHostedZoneResponse -> [TrafficPolicyInstance]) (\s a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsTrafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead." #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by submitting another @ListTrafficPolicyInstancesByHostedZone@ request and specifying the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Lude.Bool
ltpibhzrsIsTruncated = Lens.lens (isTruncated :: ListTrafficPolicyInstancesByHostedZoneResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicyInstancesByHostedZone@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Lude.Text
ltpibhzrsMaxItems = Lens.lens (maxItems :: ListTrafficPolicyInstancesByHostedZoneResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)
{-# DEPRECATED ltpibhzrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
