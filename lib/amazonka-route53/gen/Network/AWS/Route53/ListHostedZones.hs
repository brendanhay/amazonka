{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a @HostedZones@ child element for each hosted zone.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the @maxitems@ parameter to list them in groups of up to 100.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHostedZones
  ( -- * Creating a request
    ListHostedZones (..),
    mkListHostedZones,

    -- ** Request lenses
    lhzDelegationSetId,
    lhzMarker,
    lhzMaxItems,

    -- * Destructuring the response
    ListHostedZonesResponse (..),
    mkListHostedZonesResponse,

    -- ** Response lenses
    lhzrsHostedZones,
    lhzrsMarker,
    lhzrsMaxItems,
    lhzrsNextMarker,
    lhzrsIsTruncated,
    lhzrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.
--
-- /See:/ 'mkListHostedZones' smart constructor.
data ListHostedZones = ListHostedZones'
  { -- | If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
    delegationSetId :: Lude.Maybe ResourceId,
    -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.
    --
    -- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
    marker :: Lude.Maybe Lude.Text,
    -- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Route 53 will return if you submit another request.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHostedZones' with the minimum fields required to make a request.
--
-- * 'delegationSetId' - If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
-- * 'marker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
-- * 'maxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Route 53 will return if you submit another request.
mkListHostedZones ::
  ListHostedZones
mkListHostedZones =
  ListHostedZones'
    { delegationSetId = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | If you're using reusable delegation sets and you want to list all of the hosted zones that are associated with a reusable delegation set, specify the ID of that reusable delegation set.
--
-- /Note:/ Consider using 'delegationSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzDelegationSetId :: Lens.Lens' ListHostedZones (Lude.Maybe ResourceId)
lhzDelegationSetId = Lens.lens (delegationSetId :: ListHostedZones -> Lude.Maybe ResourceId) (\s a -> s {delegationSetId = a} :: ListHostedZones)
{-# DEPRECATED lhzDelegationSetId "Use generic-lens or generic-optics with 'delegationSetId' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more hosted zones. To get more hosted zones, submit another @ListHostedZones@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first hosted zone that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more hosted zones to get.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzMarker :: Lens.Lens' ListHostedZones (Lude.Maybe Lude.Text)
lhzMarker = Lens.lens (marker :: ListHostedZones -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListHostedZones)
{-# DEPRECATED lhzMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If you have more than @maxitems@ hosted zones, the value of @IsTruncated@ in the response is @true@ , and the value of @NextMarker@ is the hosted zone ID of the first hosted zone that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzMaxItems :: Lens.Lens' ListHostedZones (Lude.Maybe Lude.Text)
lhzMaxItems = Lens.lens (maxItems :: ListHostedZones -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZones)
{-# DEPRECATED lhzMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListHostedZones where
  page rq rs
    | Page.stop (rs Lens.^. lhzrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lhzrsNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhzMarker Lens..~ rs Lens.^. lhzrsNextMarker

instance Lude.AWSRequest ListHostedZones where
  type Rs ListHostedZones = ListHostedZonesResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListHostedZonesResponse'
            Lude.<$> ( x Lude..@? "HostedZones" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HostedZone"
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@ "MaxItems")
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHostedZones where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListHostedZones where
  toPath = Lude.const "/2013-04-01/hostedzone"

instance Lude.ToQuery ListHostedZones where
  toQuery ListHostedZones' {..} =
    Lude.mconcat
      [ "delegationsetid" Lude.=: delegationSetId,
        "marker" Lude.=: marker,
        "maxitems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListHostedZonesResponse' smart constructor.
data ListHostedZonesResponse = ListHostedZonesResponse'
  { -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [HostedZone],
    -- | For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
    marker :: Lude.Maybe Lude.Text,
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
    maxItems :: Lude.Text,
    -- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
    isTruncated :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHostedZonesResponse' with the minimum fields required to make a request.
--
-- * 'hostedZones' - A complex type that contains general information about the hosted zone.
-- * 'marker' - For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
-- * 'maxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
-- * 'nextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
-- * 'isTruncated' - A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
-- * 'responseStatus' - The response status code.
mkListHostedZonesResponse ::
  -- | 'maxItems'
  Lude.Text ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListHostedZonesResponse
mkListHostedZonesResponse pMaxItems_ pIsTruncated_ pResponseStatus_ =
  ListHostedZonesResponse'
    { hostedZones = Lude.mempty,
      marker = Lude.Nothing,
      maxItems = pMaxItems_,
      nextMarker = Lude.Nothing,
      isTruncated = pIsTruncated_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsHostedZones :: Lens.Lens' ListHostedZonesResponse [HostedZone]
lhzrsHostedZones = Lens.lens (hostedZones :: ListHostedZonesResponse -> [HostedZone]) (\s a -> s {hostedZones = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsHostedZones "Use generic-lens or generic-optics with 'hostedZones' instead." #-}

-- | For the second and subsequent calls to @ListHostedZones@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsMarker :: Lens.Lens' ListHostedZonesResponse (Lude.Maybe Lude.Text)
lhzrsMarker = Lens.lens (marker :: ListHostedZonesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZones@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsMaxItems :: Lens.Lens' ListHostedZonesResponse Lude.Text
lhzrsMaxItems = Lens.lens (maxItems :: ListHostedZonesResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first hosted zone in the next group of hosted zones. Submit another @ListHostedZones@ request, and specify the value of @NextMarker@ from the response in the @marker@ parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsNextMarker :: Lens.Lens' ListHostedZonesResponse (Lude.Maybe Lude.Text)
lhzrsNextMarker = Lens.lens (nextMarker :: ListHostedZonesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag indicating whether there are more hosted zones to be listed. If the response was truncated, you can get more hosted zones by submitting another @ListHostedZones@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsIsTruncated :: Lens.Lens' ListHostedZonesResponse Lude.Bool
lhzrsIsTruncated = Lens.lens (isTruncated :: ListHostedZonesResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzrsResponseStatus :: Lens.Lens' ListHostedZonesResponse Lude.Int
lhzrsResponseStatus = Lens.lens (responseStatus :: ListHostedZonesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHostedZonesResponse)
{-# DEPRECATED lhzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
