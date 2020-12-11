{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ViewBilling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the domain-related billing records for the current AWS account for a specified period
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ViewBilling
  ( -- * Creating a request
    ViewBilling (..),
    mkViewBilling,

    -- ** Request lenses
    vbStart,
    vbEnd,
    vbMarker,
    vbMaxItems,

    -- * Destructuring the response
    ViewBillingResponse (..),
    mkViewBillingResponse,

    -- ** Response lenses
    vbrsNextPageMarker,
    vbrsBillingRecords,
    vbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The ViewBilling request includes the following elements.
--
-- /See:/ 'mkViewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { start :: Lude.Maybe Lude.Timestamp,
    end :: Lude.Maybe Lude.Timestamp,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ViewBilling' with the minimum fields required to make a request.
--
-- * 'end' - The end date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
-- * 'marker' - For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
-- * 'maxItems' - The number of billing records to be returned.
--
-- Default: 20
-- * 'start' - The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
mkViewBilling ::
  ViewBilling
mkViewBilling =
  ViewBilling'
    { start = Lude.Nothing,
      end = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbStart :: Lens.Lens' ViewBilling (Lude.Maybe Lude.Timestamp)
vbStart = Lens.lens (start :: ViewBilling -> Lude.Maybe Lude.Timestamp) (\s a -> s {start = a} :: ViewBilling)
{-# DEPRECATED vbStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The end date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbEnd :: Lens.Lens' ViewBilling (Lude.Maybe Lude.Timestamp)
vbEnd = Lens.lens (end :: ViewBilling -> Lude.Maybe Lude.Timestamp) (\s a -> s {end = a} :: ViewBilling)
{-# DEPRECATED vbEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbMarker :: Lens.Lens' ViewBilling (Lude.Maybe Lude.Text)
vbMarker = Lens.lens (marker :: ViewBilling -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ViewBilling)
{-# DEPRECATED vbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The number of billing records to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbMaxItems :: Lens.Lens' ViewBilling (Lude.Maybe Lude.Int)
vbMaxItems = Lens.lens (maxItems :: ViewBilling -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ViewBilling)
{-# DEPRECATED vbMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ViewBilling where
  page rq rs
    | Page.stop (rs Lens.^. vbrsNextPageMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. vbrsBillingRecords) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& vbMarker Lens..~ rs Lens.^. vbrsNextPageMarker

instance Lude.AWSRequest ViewBilling where
  type Rs ViewBilling = ViewBillingResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ViewBillingResponse'
            Lude.<$> (x Lude..?> "NextPageMarker")
            Lude.<*> (x Lude..?> "BillingRecords" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ViewBilling where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.ViewBilling" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ViewBilling where
  toJSON ViewBilling' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Start" Lude..=) Lude.<$> start,
            ("End" Lude..=) Lude.<$> end,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxItems" Lude..=) Lude.<$> maxItems
          ]
      )

instance Lude.ToPath ViewBilling where
  toPath = Lude.const "/"

instance Lude.ToQuery ViewBilling where
  toQuery = Lude.const Lude.mempty

-- | The ViewBilling response includes the following elements.
--
-- /See:/ 'mkViewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { nextPageMarker ::
      Lude.Maybe Lude.Text,
    billingRecords :: Lude.Maybe [BillingRecord],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ViewBillingResponse' with the minimum fields required to make a request.
--
-- * 'billingRecords' - A summary of billing records.
-- * 'nextPageMarker' - If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
-- * 'responseStatus' - The response status code.
mkViewBillingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ViewBillingResponse
mkViewBillingResponse pResponseStatus_ =
  ViewBillingResponse'
    { nextPageMarker = Lude.Nothing,
      billingRecords = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrsNextPageMarker :: Lens.Lens' ViewBillingResponse (Lude.Maybe Lude.Text)
vbrsNextPageMarker = Lens.lens (nextPageMarker :: ViewBillingResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageMarker = a} :: ViewBillingResponse)
{-# DEPRECATED vbrsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | A summary of billing records.
--
-- /Note:/ Consider using 'billingRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrsBillingRecords :: Lens.Lens' ViewBillingResponse (Lude.Maybe [BillingRecord])
vbrsBillingRecords = Lens.lens (billingRecords :: ViewBillingResponse -> Lude.Maybe [BillingRecord]) (\s a -> s {billingRecords = a} :: ViewBillingResponse)
{-# DEPRECATED vbrsBillingRecords "Use generic-lens or generic-optics with 'billingRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrsResponseStatus :: Lens.Lens' ViewBillingResponse Lude.Int
vbrsResponseStatus = Lens.lens (responseStatus :: ViewBillingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ViewBillingResponse)
{-# DEPRECATED vbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
