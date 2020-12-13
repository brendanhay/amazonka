{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListServices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information for all the services that are associated with one or more specified namespaces.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListServices
  ( -- * Creating a request
    ListServices (..),
    mkListServices,

    -- ** Request lenses
    lsFilters,
    lsNextToken,
    lsMaxResults,

    -- * Destructuring the response
    ListServicesResponse (..),
    mkListServicesResponse,

    -- ** Response lenses
    lsrsNextToken,
    lsrsServices,
    lsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkListServices' smart constructor.
data ListServices = ListServices'
  { -- | A complex type that contains specifications for the namespaces that you want to list services for.
    --
    -- If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
    filters :: Lude.Maybe [ServiceFilter],
    -- | For the first @ListServices@ request, omit this value.
    --
    -- If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of services that you want AWS Cloud Map to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 services.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServices' with the minimum fields required to make a request.
--
-- * 'filters' - A complex type that contains specifications for the namespaces that you want to list services for.
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
-- * 'nextToken' - For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'maxResults' - The maximum number of services that you want AWS Cloud Map to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 services.
mkListServices ::
  ListServices
mkListServices =
  ListServices'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A complex type that contains specifications for the namespaces that you want to list services for.
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListServices@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsFilters :: Lens.Lens' ListServices (Lude.Maybe [ServiceFilter])
lsFilters = Lens.lens (filters :: ListServices -> Lude.Maybe [ServiceFilter]) (\s a -> s {filters = a} :: ListServices)
{-# DEPRECATED lsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | For the first @ListServices@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListServices (Lude.Maybe Lude.Text)
lsNextToken = Lens.lens (nextToken :: ListServices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServices)
{-# DEPRECATED lsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of services that you want AWS Cloud Map to return in the response to a @ListServices@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 services.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListServices (Lude.Maybe Lude.Natural)
lsMaxResults = Lens.lens (maxResults :: ListServices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListServices)
{-# DEPRECATED lsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListServices where
  page rq rs
    | Page.stop (rs Lens.^. lsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsServices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsNextToken Lens..~ rs Lens.^. lsrsNextToken

instance Lude.AWSRequest ListServices where
  type Rs ListServices = ListServicesResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListServicesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Services" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListServices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.ListServices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListServices where
  toJSON ListServices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListServices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListServicesResponse' smart constructor.
data ListServicesResponse = ListServicesResponse'
  { -- | If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
    services :: Lude.Maybe [ServiceSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'services' - An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
-- * 'responseStatus' - The response status code.
mkListServicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServicesResponse
mkListServicesResponse pResponseStatus_ =
  ListServicesResponse'
    { nextToken = Lude.Nothing,
      services = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the response contains @NextToken@ , submit another @ListServices@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsNextToken :: Lens.Lens' ListServicesResponse (Lude.Maybe Lude.Text)
lsrsNextToken = Lens.lens (nextToken :: ListServicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServicesResponse)
{-# DEPRECATED lsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array that contains one @ServiceSummary@ object for each service that matches the specified filter criteria.
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsServices :: Lens.Lens' ListServicesResponse (Lude.Maybe [ServiceSummary])
lsrsServices = Lens.lens (services :: ListServicesResponse -> Lude.Maybe [ServiceSummary]) (\s a -> s {services = a} :: ListServicesResponse)
{-# DEPRECATED lsrsServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListServicesResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListServicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServicesResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
