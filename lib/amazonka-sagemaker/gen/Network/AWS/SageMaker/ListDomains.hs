{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the domains.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldNextToken,
    ldMaxResults,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDomains,
    ldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
-- * 'maxResults' - Returns a list up to a specified limit.
mkListDomains ::
  ListDomains
mkListDomains =
  ListDomains' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDomains (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDomains -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomains)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a list up to a specified limit.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDomains (Lude.Maybe Lude.Natural)
ldMaxResults = Lens.lens (maxResults :: ListDomains -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDomains)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDomains where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDomains) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Domains" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDomains where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.ListDomains" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDomains where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of domains.
    domains :: Lude.Maybe [DomainDetails],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
-- * 'domains' - The list of domains.
-- * 'responseStatus' - The response status code.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainsResponse
mkListDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { nextToken = Lude.Nothing,
      domains = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the previous response was truncated, you will receive this token. Use it in your next request to receive the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDomainsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of domains.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDomains :: Lens.Lens' ListDomainsResponse (Lude.Maybe [DomainDetails])
ldrsDomains = Lens.lens (domains :: ListDomainsResponse -> Lude.Maybe [DomainDetails]) (\s a -> s {domains = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDomainsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
