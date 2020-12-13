{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDomains
  ( -- * Creating a request
    GetDomains (..),
    mkGetDomains,

    -- ** Request lenses
    gdPageToken,

    -- * Destructuring the response
    GetDomainsResponse (..),
    mkGetDomainsResponse,

    -- ** Response lenses
    gdrsNextPageToken,
    gdrsDomains,
    gdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDomains' smart constructor.
newtype GetDomains = GetDomains'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomains' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetDomains ::
  GetDomains
mkGetDomains = GetDomains' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPageToken :: Lens.Lens' GetDomains (Lude.Maybe Lude.Text)
gdPageToken = Lens.lens (pageToken :: GetDomains -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetDomains)
{-# DEPRECATED gdPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetDomains where
  page rq rs
    | Page.stop (rs Lens.^. gdrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdrsDomains) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdPageToken Lens..~ rs Lens.^. gdrsNextPageToken

instance Lude.AWSRequest GetDomains where
  type Rs GetDomains = GetDomainsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDomainsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "domains" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDomains where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDomains" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDomains where
  toJSON GetDomains' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDomains where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of key-value pairs containing information about each of the domain entries in the user's account.
    domains :: Lude.Maybe [Domain],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
-- * 'domains' - An array of key-value pairs containing information about each of the domain entries in the user's account.
-- * 'responseStatus' - The response status code.
mkGetDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDomainsResponse
mkGetDomainsResponse pResponseStatus_ =
  GetDomainsResponse'
    { nextPageToken = Lude.Nothing,
      domains = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsNextPageToken :: Lens.Lens' GetDomainsResponse (Lude.Maybe Lude.Text)
gdrsNextPageToken = Lens.lens (nextPageToken :: GetDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDomainsResponse)
{-# DEPRECATED gdrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs containing information about each of the domain entries in the user's account.
--
-- /Note:/ Consider using 'domains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDomains :: Lens.Lens' GetDomainsResponse (Lude.Maybe [Domain])
gdrsDomains = Lens.lens (domains :: GetDomainsResponse -> Lude.Maybe [Domain]) (\s a -> s {domains = a} :: GetDomainsResponse)
{-# DEPRECATED gdrsDomains "Use generic-lens or generic-optics with 'domains' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDomainsResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDomainsResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
