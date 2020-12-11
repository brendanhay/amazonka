{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route 53 for the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldMarker,
    ldMaxItems,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrsNextPageMarker,
    ldrsResponseStatus,
    ldrsDomains,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The ListDomains request includes the following elements.
--
-- /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- * 'marker' - For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value specified in the previous request.
-- * 'maxItems' - Number of domains to be returned.
--
-- Default: 20
mkListDomains ::
  ListDomains
mkListDomains =
  ListDomains' {marker = Lude.Nothing, maxItems = Lude.Nothing}

-- | For an initial request for a list of domains, omit this element. If the number of domains that are associated with the current AWS account is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional domains. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value specified in the previous request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMarker :: Lens.Lens' ListDomains (Lude.Maybe Lude.Text)
ldMarker = Lens.lens (marker :: ListDomains -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListDomains)
{-# DEPRECATED ldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Number of domains to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxItems :: Lens.Lens' ListDomains (Lude.Maybe Lude.Int)
ldMaxItems = Lens.lens (maxItems :: ListDomains -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ListDomains)
{-# DEPRECATED ldMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListDomains where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextPageMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDomains) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldMarker Lens..~ rs Lens.^. ldrsNextPageMarker

instance Lude.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Lude.<$> (x Lude..?> "NextPageMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "Domains" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListDomains where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.ListDomains" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("MaxItems" Lude..=) Lude.<$> maxItems
          ]
      )

instance Lude.ToPath ListDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDomains where
  toQuery = Lude.const Lude.mempty

-- | The ListDomains response includes the following elements.
--
-- /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { nextPageMarker ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    domains :: [DomainSummary]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- * 'domains' - A summary of domains.
-- * 'nextPageMarker' - If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
-- * 'responseStatus' - The response status code.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainsResponse
mkListDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { nextPageMarker = Lude.Nothing,
      responseStatus = pResponseStatus_,
      domains = Lude.mempty
    }

-- | If there are more domains than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextPageMarker :: Lens.Lens' ListDomainsResponse (Lude.Maybe Lude.Text)
ldrsNextPageMarker = Lens.lens (nextPageMarker :: ListDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageMarker = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDomainsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A summary of domains.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDomains :: Lens.Lens' ListDomainsResponse [DomainSummary]
ldrsDomains = Lens.lens (domains :: ListDomainsResponse -> [DomainSummary]) (\s a -> s {domains = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsDomains "Use generic-lens or generic-optics with 'domains' instead." #-}
