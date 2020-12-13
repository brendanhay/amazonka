{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the reusable delegation sets that are associated with the current AWS account.
module Network.AWS.Route53.ListReusableDelegationSets
  ( -- * Creating a request
    ListReusableDelegationSets (..),
    mkListReusableDelegationSets,

    -- ** Request lenses
    lrdsMarker,
    lrdsMaxItems,

    -- * Destructuring the response
    ListReusableDelegationSetsResponse (..),
    mkListReusableDelegationSetsResponse,

    -- ** Response lenses
    lrdsrsDelegationSets,
    lrdsrsMarker,
    lrdsrsMaxItems,
    lrdsrsNextMarker,
    lrdsrsIsTruncated,
    lrdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get a list of the reusable delegation sets that are associated with the current AWS account.
--
-- /See:/ 'mkListReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
  { -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more reusable delegation sets. To get another group, submit another @ListReusableDelegationSets@ request.
    --
    -- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first reusable delegation set that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more reusable delegation sets to get.
    marker :: Lude.Maybe Lude.Text,
    -- | The number of reusable delegation sets that you want Amazon Route 53 to return in the response to this request. If you specify a value greater than 100, Route 53 returns only the first 100 reusable delegation sets.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReusableDelegationSets' with the minimum fields required to make a request.
--
-- * 'marker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more reusable delegation sets. To get another group, submit another @ListReusableDelegationSets@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first reusable delegation set that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more reusable delegation sets to get.
-- * 'maxItems' - The number of reusable delegation sets that you want Amazon Route 53 to return in the response to this request. If you specify a value greater than 100, Route 53 returns only the first 100 reusable delegation sets.
mkListReusableDelegationSets ::
  ListReusableDelegationSets
mkListReusableDelegationSets =
  ListReusableDelegationSets'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more reusable delegation sets. To get another group, submit another @ListReusableDelegationSets@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first reusable delegation set that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more reusable delegation sets to get.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMarker :: Lens.Lens' ListReusableDelegationSets (Lude.Maybe Lude.Text)
lrdsMarker = Lens.lens (marker :: ListReusableDelegationSets -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListReusableDelegationSets)
{-# DEPRECATED lrdsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The number of reusable delegation sets that you want Amazon Route 53 to return in the response to this request. If you specify a value greater than 100, Route 53 returns only the first 100 reusable delegation sets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsMaxItems :: Lens.Lens' ListReusableDelegationSets (Lude.Maybe Lude.Text)
lrdsMaxItems = Lens.lens (maxItems :: ListReusableDelegationSets -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListReusableDelegationSets)
{-# DEPRECATED lrdsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListReusableDelegationSets where
  type
    Rs ListReusableDelegationSets =
      ListReusableDelegationSetsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListReusableDelegationSetsResponse'
            Lude.<$> ( x Lude..@? "DelegationSets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "DelegationSet"
                     )
            Lude.<*> (x Lude..@ "Marker")
            Lude.<*> (x Lude..@ "MaxItems")
            Lude.<*> (x Lude..@? "NextMarker")
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReusableDelegationSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListReusableDelegationSets where
  toPath = Lude.const "/2013-04-01/delegationset"

instance Lude.ToQuery ListReusableDelegationSets where
  toQuery ListReusableDelegationSets' {..} =
    Lude.mconcat
      ["marker" Lude.=: marker, "maxitems" Lude.=: maxItems]

-- | A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.
--
-- /See:/ 'mkListReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
  { -- | A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
    delegationSets :: [DelegationSet],
    -- | For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
    marker :: Lude.Text,
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
    maxItems :: Lude.Text,
    -- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the next reusable delegation set that Amazon Route 53 will return if you submit another @ListReusableDelegationSets@ request and specify the value of @NextMarker@ in the @marker@ parameter.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more reusable delegation sets to be listed.
    isTruncated :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReusableDelegationSetsResponse' with the minimum fields required to make a request.
--
-- * 'delegationSets' - A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
-- * 'marker' - For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
-- * 'maxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
-- * 'nextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the next reusable delegation set that Amazon Route 53 will return if you submit another @ListReusableDelegationSets@ request and specify the value of @NextMarker@ in the @marker@ parameter.
-- * 'isTruncated' - A flag that indicates whether there are more reusable delegation sets to be listed.
-- * 'responseStatus' - The response status code.
mkListReusableDelegationSetsResponse ::
  -- | 'marker'
  Lude.Text ->
  -- | 'maxItems'
  Lude.Text ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListReusableDelegationSetsResponse
mkListReusableDelegationSetsResponse
  pMarker_
  pMaxItems_
  pIsTruncated_
  pResponseStatus_ =
    ListReusableDelegationSetsResponse'
      { delegationSets = Lude.mempty,
        marker = pMarker_,
        maxItems = pMaxItems_,
        nextMarker = Lude.Nothing,
        isTruncated = pIsTruncated_,
        responseStatus = pResponseStatus_
      }

-- | A complex type that contains one @DelegationSet@ element for each reusable delegation set that was created by the current AWS account.
--
-- /Note:/ Consider using 'delegationSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsDelegationSets :: Lens.Lens' ListReusableDelegationSetsResponse [DelegationSet]
lrdsrsDelegationSets = Lens.lens (delegationSets :: ListReusableDelegationSetsResponse -> [DelegationSet]) (\s a -> s {delegationSets = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsDelegationSets "Use generic-lens or generic-optics with 'delegationSets' instead." #-}

-- | For the second and subsequent calls to @ListReusableDelegationSets@ , @Marker@ is the value that you specified for the @marker@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsMarker :: Lens.Lens' ListReusableDelegationSetsResponse Lude.Text
lrdsrsMarker = Lens.lens (marker :: ListReusableDelegationSetsResponse -> Lude.Text) (\s a -> s {marker = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListReusableDelegationSets@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsMaxItems :: Lens.Lens' ListReusableDelegationSetsResponse Lude.Text
lrdsrsMaxItems = Lens.lens (maxItems :: ListReusableDelegationSetsResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the next reusable delegation set that Amazon Route 53 will return if you submit another @ListReusableDelegationSets@ request and specify the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsNextMarker :: Lens.Lens' ListReusableDelegationSetsResponse (Lude.Maybe Lude.Text)
lrdsrsNextMarker = Lens.lens (nextMarker :: ListReusableDelegationSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A flag that indicates whether there are more reusable delegation sets to be listed.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsIsTruncated :: Lens.Lens' ListReusableDelegationSetsResponse Lude.Bool
lrdsrsIsTruncated = Lens.lens (isTruncated :: ListReusableDelegationSetsResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdsrsResponseStatus :: Lens.Lens' ListReusableDelegationSetsResponse Lude.Int
lrdsrsResponseStatus = Lens.lens (responseStatus :: ListReusableDelegationSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReusableDelegationSetsResponse)
{-# DEPRECATED lrdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
