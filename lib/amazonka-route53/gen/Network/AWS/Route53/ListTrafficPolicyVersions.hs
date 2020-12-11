{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions for a specified traffic policy.
--
-- Traffic policy versions are listed in numerical order by @VersionNumber@ .
module Network.AWS.Route53.ListTrafficPolicyVersions
  ( -- * Creating a request
    ListTrafficPolicyVersions (..),
    mkListTrafficPolicyVersions,

    -- ** Request lenses
    ltpvMaxItems,
    ltpvTrafficPolicyVersionMarker,
    ltpvId,

    -- * Destructuring the response
    ListTrafficPolicyVersionsResponse (..),
    mkListTrafficPolicyVersionsResponse,

    -- ** Response lenses
    ltpvrsResponseStatus,
    ltpvrsTrafficPolicies,
    ltpvrsIsTruncated,
    ltpvrsTrafficPolicyVersionMarker,
    ltpvrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains the information about the request to list your traffic policies.
--
-- /See:/ 'mkListTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
  { maxItems ::
      Lude.Maybe Lude.Text,
    trafficPolicyVersionMarker ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTrafficPolicyVersions' with the minimum fields required to make a request.
--
-- * 'id' - Specify the value of @Id@ of the traffic policy for which you want to list all versions.
-- * 'maxItems' - The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
-- * 'trafficPolicyVersionMarker' - For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
mkListTrafficPolicyVersions ::
  -- | 'id'
  Lude.Text ->
  ListTrafficPolicyVersions
mkListTrafficPolicyVersions pId_ =
  ListTrafficPolicyVersions'
    { maxItems = Lude.Nothing,
      trafficPolicyVersionMarker = Lude.Nothing,
      id = pId_
    }

-- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvMaxItems :: Lens.Lens' ListTrafficPolicyVersions (Lude.Maybe Lude.Text)
ltpvMaxItems = Lens.lens (maxItems :: ListTrafficPolicyVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyVersions)
{-# DEPRECATED ltpvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersions (Lude.Maybe Lude.Text)
ltpvTrafficPolicyVersionMarker = Lens.lens (trafficPolicyVersionMarker :: ListTrafficPolicyVersions -> Lude.Maybe Lude.Text) (\s a -> s {trafficPolicyVersionMarker = a} :: ListTrafficPolicyVersions)
{-# DEPRECATED ltpvTrafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead." #-}

-- | Specify the value of @Id@ of the traffic policy for which you want to list all versions.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvId :: Lens.Lens' ListTrafficPolicyVersions Lude.Text
ltpvId = Lens.lens (id :: ListTrafficPolicyVersions -> Lude.Text) (\s a -> s {id = a} :: ListTrafficPolicyVersions)
{-# DEPRECATED ltpvId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ListTrafficPolicyVersions where
  type
    Rs ListTrafficPolicyVersions =
      ListTrafficPolicyVersionsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListTrafficPolicyVersionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "TrafficPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "TrafficPolicy"
                     )
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "TrafficPolicyVersionMarker")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListTrafficPolicyVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTrafficPolicyVersions where
  toPath ListTrafficPolicyVersions' {..} =
    Lude.mconcat
      ["/2013-04-01/trafficpolicies/", Lude.toBS id, "/versions"]

instance Lude.ToQuery ListTrafficPolicyVersions where
  toQuery ListTrafficPolicyVersions' {..} =
    Lude.mconcat
      [ "maxitems" Lude.=: maxItems,
        "trafficpolicyversion" Lude.=: trafficPolicyVersionMarker
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
  { responseStatus ::
      Lude.Int,
    trafficPolicies ::
      [TrafficPolicy],
    isTruncated ::
      Lude.Bool,
    trafficPolicyVersionMarker ::
      Lude.Text,
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

-- | Creates a value of 'ListTrafficPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
-- * 'maxItems' - The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
-- * 'responseStatus' - The response status code.
-- * 'trafficPolicies' - A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
-- * 'trafficPolicyVersionMarker' - If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
mkListTrafficPolicyVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'trafficPolicyVersionMarker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Text ->
  ListTrafficPolicyVersionsResponse
mkListTrafficPolicyVersionsResponse
  pResponseStatus_
  pIsTruncated_
  pTrafficPolicyVersionMarker_
  pMaxItems_ =
    ListTrafficPolicyVersionsResponse'
      { responseStatus =
          pResponseStatus_,
        trafficPolicies = Lude.mempty,
        isTruncated = pIsTruncated_,
        trafficPolicyVersionMarker = pTrafficPolicyVersionMarker_,
        maxItems = pMaxItems_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrsResponseStatus :: Lens.Lens' ListTrafficPolicyVersionsResponse Lude.Int
ltpvrsResponseStatus = Lens.lens (responseStatus :: ListTrafficPolicyVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTrafficPolicyVersionsResponse)
{-# DEPRECATED ltpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrsTrafficPolicies :: Lens.Lens' ListTrafficPolicyVersionsResponse [TrafficPolicy]
ltpvrsTrafficPolicies = Lens.lens (trafficPolicies :: ListTrafficPolicyVersionsResponse -> [TrafficPolicy]) (\s a -> s {trafficPolicies = a} :: ListTrafficPolicyVersionsResponse)
{-# DEPRECATED ltpvrsTrafficPolicies "Use generic-lens or generic-optics with 'trafficPolicies' instead." #-}

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrsIsTruncated :: Lens.Lens' ListTrafficPolicyVersionsResponse Lude.Bool
ltpvrsIsTruncated = Lens.lens (isTruncated :: ListTrafficPolicyVersionsResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListTrafficPolicyVersionsResponse)
{-# DEPRECATED ltpvrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrsTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersionsResponse Lude.Text
ltpvrsTrafficPolicyVersionMarker = Lens.lens (trafficPolicyVersionMarker :: ListTrafficPolicyVersionsResponse -> Lude.Text) (\s a -> s {trafficPolicyVersionMarker = a} :: ListTrafficPolicyVersionsResponse)
{-# DEPRECATED ltpvrsTrafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrsMaxItems :: Lens.Lens' ListTrafficPolicyVersionsResponse Lude.Text
ltpvrsMaxItems = Lens.lens (maxItems :: ListTrafficPolicyVersionsResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListTrafficPolicyVersionsResponse)
{-# DEPRECATED ltpvrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
