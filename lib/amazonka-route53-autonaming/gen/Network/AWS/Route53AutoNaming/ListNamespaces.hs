{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListNamespaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the namespaces that were created by the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListNamespaces
  ( -- * Creating a request
    ListNamespaces (..),
    mkListNamespaces,

    -- ** Request lenses
    lnFilters,
    lnNextToken,
    lnMaxResults,

    -- * Destructuring the response
    ListNamespacesResponse (..),
    mkListNamespacesResponse,

    -- ** Response lenses
    lnrsNamespaces,
    lnrsNextToken,
    lnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { filters ::
      Lude.Maybe [NamespaceFilter],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListNamespaces' with the minimum fields required to make a request.
--
-- * 'filters' - A complex type that contains specifications for the namespaces that you want to list.
--
-- If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
-- * 'maxResults' - The maximum number of namespaces that you want AWS Cloud Map to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 namespaces.
-- * 'nextToken' - For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
mkListNamespaces ::
  ListNamespaces
mkListNamespaces =
  ListNamespaces'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A complex type that contains specifications for the namespaces that you want to list.
--
-- If you specify more than one filter, a namespace must match all filters to be returned by @ListNamespaces@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnFilters :: Lens.Lens' ListNamespaces (Lude.Maybe [NamespaceFilter])
lnFilters = Lens.lens (filters :: ListNamespaces -> Lude.Maybe [NamespaceFilter]) (\s a -> s {filters = a} :: ListNamespaces)
{-# DEPRECATED lnFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | For the first @ListNamespaces@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnNextToken :: Lens.Lens' ListNamespaces (Lude.Maybe Lude.Text)
lnNextToken = Lens.lens (nextToken :: ListNamespaces -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNamespaces)
{-# DEPRECATED lnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of namespaces that you want AWS Cloud Map to return in the response to a @ListNamespaces@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 namespaces.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnMaxResults :: Lens.Lens' ListNamespaces (Lude.Maybe Lude.Natural)
lnMaxResults = Lens.lens (maxResults :: ListNamespaces -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListNamespaces)
{-# DEPRECATED lnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListNamespaces where
  page rq rs
    | Page.stop (rs Lens.^. lnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lnrsNamespaces) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lnNextToken Lens..~ rs Lens.^. lnrsNextToken

instance Lude.AWSRequest ListNamespaces where
  type Rs ListNamespaces = ListNamespacesResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Lude.<$> (x Lude..?> "Namespaces" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListNamespaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.ListNamespaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListNamespaces where
  toJSON ListNamespaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListNamespaces where
  toPath = Lude.const "/"

instance Lude.ToQuery ListNamespaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { namespaces ::
      Lude.Maybe [NamespaceSummary],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListNamespacesResponse' with the minimum fields required to make a request.
--
-- * 'namespaces' - An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
-- * 'nextToken' - If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
-- * 'responseStatus' - The response status code.
mkListNamespacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListNamespacesResponse
mkListNamespacesResponse pResponseStatus_ =
  ListNamespacesResponse'
    { namespaces = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array that contains one @NamespaceSummary@ object for each namespace that matches the specified filter criteria.
--
-- /Note:/ Consider using 'namespaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrsNamespaces :: Lens.Lens' ListNamespacesResponse (Lude.Maybe [NamespaceSummary])
lnrsNamespaces = Lens.lens (namespaces :: ListNamespacesResponse -> Lude.Maybe [NamespaceSummary]) (\s a -> s {namespaces = a} :: ListNamespacesResponse)
{-# DEPRECATED lnrsNamespaces "Use generic-lens or generic-optics with 'namespaces' instead." #-}

-- | If the response contains @NextToken@ , submit another @ListNamespaces@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrsNextToken :: Lens.Lens' ListNamespacesResponse (Lude.Maybe Lude.Text)
lnrsNextToken = Lens.lens (nextToken :: ListNamespacesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListNamespacesResponse)
{-# DEPRECATED lnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lnrsResponseStatus :: Lens.Lens' ListNamespacesResponse Lude.Int
lnrsResponseStatus = Lens.lens (responseStatus :: ListNamespacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListNamespacesResponse)
{-# DEPRECATED lnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
