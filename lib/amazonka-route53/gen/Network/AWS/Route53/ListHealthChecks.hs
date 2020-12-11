{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the health checks that are associated with the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListHealthChecks
  ( -- * Creating a request
    ListHealthChecks (..),
    mkListHealthChecks,

    -- ** Request lenses
    lhcMarker,
    lhcMaxItems,

    -- * Destructuring the response
    ListHealthChecksResponse (..),
    mkListHealthChecksResponse,

    -- ** Response lenses
    lhcrsNextMarker,
    lhcrsResponseStatus,
    lhcrsHealthChecks,
    lhcrsMarker,
    lhcrsIsTruncated,
    lhcrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to retrieve a list of the health checks that are associated with the current AWS account.
--
-- /See:/ 'mkListHealthChecks' smart constructor.
data ListHealthChecks = ListHealthChecks'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHealthChecks' with the minimum fields required to make a request.
--
-- * 'marker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more health checks. To get another group, submit another @ListHealthChecks@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first health check that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more health checks to get.
-- * 'maxItems' - The maximum number of health checks that you want @ListHealthChecks@ to return in response to the current request. Amazon Route 53 returns a maximum of 100 items. If you set @MaxItems@ to a value greater than 100, Route 53 returns only the first 100 health checks.
mkListHealthChecks ::
  ListHealthChecks
mkListHealthChecks =
  ListHealthChecks' {marker = Lude.Nothing, maxItems = Lude.Nothing}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more health checks. To get another group, submit another @ListHealthChecks@ request.
--
-- For the value of @marker@ , specify the value of @NextMarker@ from the previous response, which is the ID of the first health check that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more health checks to get.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcMarker :: Lens.Lens' ListHealthChecks (Lude.Maybe Lude.Text)
lhcMarker = Lens.lens (marker :: ListHealthChecks -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListHealthChecks)
{-# DEPRECATED lhcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of health checks that you want @ListHealthChecks@ to return in response to the current request. Amazon Route 53 returns a maximum of 100 items. If you set @MaxItems@ to a value greater than 100, Route 53 returns only the first 100 health checks.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcMaxItems :: Lens.Lens' ListHealthChecks (Lude.Maybe Lude.Text)
lhcMaxItems = Lens.lens (maxItems :: ListHealthChecks -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListHealthChecks)
{-# DEPRECATED lhcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListHealthChecks where
  page rq rs
    | Page.stop (rs Lens.^. lhcrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lhcrsNextMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lhcMarker Lens..~ rs Lens.^. lhcrsNextMarker

instance Lude.AWSRequest ListHealthChecks where
  type Rs ListHealthChecks = ListHealthChecksResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListHealthChecksResponse'
            Lude.<$> (x Lude..@? "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "HealthChecks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HealthCheck"
                     )
            Lude.<*> (x Lude..@ "Marker")
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListHealthChecks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListHealthChecks where
  toPath = Lude.const "/2013-04-01/healthcheck"

instance Lude.ToQuery ListHealthChecks where
  toQuery ListHealthChecks' {..} =
    Lude.mconcat
      ["marker" Lude.=: marker, "maxitems" Lude.=: maxItems]

-- | A complex type that contains the response to a @ListHealthChecks@ request.
--
-- /See:/ 'mkListHealthChecksResponse' smart constructor.
data ListHealthChecksResponse = ListHealthChecksResponse'
  { nextMarker ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    healthChecks :: [HealthCheck],
    marker :: Lude.Text,
    isTruncated :: Lude.Bool,
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

-- | Creates a value of 'ListHealthChecksResponse' with the minimum fields required to make a request.
--
-- * 'healthChecks' - A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
-- * 'isTruncated' - A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of health checks by submitting another @ListHealthChecks@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
-- * 'marker' - For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the @marker@ parameter in the previous request.
-- * 'maxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHealthChecks@ that produced the current response.
-- * 'nextMarker' - If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check that Amazon Route 53 returns if you submit another @ListHealthChecks@ request and specify the value of @NextMarker@ in the @marker@ parameter.
-- * 'responseStatus' - The response status code.
mkListHealthChecksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'marker'
  Lude.Text ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListHealthChecksResponse
mkListHealthChecksResponse
  pResponseStatus_
  pMarker_
  pIsTruncated_
  pMaxItems_ =
    ListHealthChecksResponse'
      { nextMarker = Lude.Nothing,
        responseStatus = pResponseStatus_,
        healthChecks = Lude.mempty,
        marker = pMarker_,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@ , the value of @NextMarker@ identifies the first health check that Amazon Route 53 returns if you submit another @ListHealthChecks@ request and specify the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsNextMarker :: Lens.Lens' ListHealthChecksResponse (Lude.Maybe Lude.Text)
lhcrsNextMarker = Lens.lens (nextMarker :: ListHealthChecksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsResponseStatus :: Lens.Lens' ListHealthChecksResponse Lude.Int
lhcrsResponseStatus = Lens.lens (responseStatus :: ListHealthChecksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains one @HealthCheck@ element for each health check that is associated with the current AWS account.
--
-- /Note:/ Consider using 'healthChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsHealthChecks :: Lens.Lens' ListHealthChecksResponse [HealthCheck]
lhcrsHealthChecks = Lens.lens (healthChecks :: ListHealthChecksResponse -> [HealthCheck]) (\s a -> s {healthChecks = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsHealthChecks "Use generic-lens or generic-optics with 'healthChecks' instead." #-}

-- | For the second and subsequent calls to @ListHealthChecks@ , @Marker@ is the value that you specified for the @marker@ parameter in the previous request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsMarker :: Lens.Lens' ListHealthChecksResponse Lude.Text
lhcrsMarker = Lens.lens (marker :: ListHealthChecksResponse -> Lude.Text) (\s a -> s {marker = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more health checks to be listed. If the response was truncated, you can get the next group of health checks by submitting another @ListHealthChecks@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsIsTruncated :: Lens.Lens' ListHealthChecksResponse Lude.Bool
lhcrsIsTruncated = Lens.lens (isTruncated :: ListHealthChecksResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHealthChecks@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhcrsMaxItems :: Lens.Lens' ListHealthChecksResponse Lude.Text
lhcrsMaxItems = Lens.lens (maxItems :: ListHealthChecksResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListHealthChecksResponse)
{-# DEPRECATED lhcrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
