{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetActiveNames
  ( -- * Creating a request
    GetActiveNames (..),
    mkGetActiveNames,

    -- ** Request lenses
    ganPageToken,

    -- * Destructuring the response
    GetActiveNamesResponse (..),
    mkGetActiveNamesResponse,

    -- ** Response lenses
    ganrsNextPageToken,
    ganrsActiveNames,
    ganrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetActiveNames' smart constructor.
newtype GetActiveNames = GetActiveNames'
  { pageToken ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetActiveNames' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetActiveNames ::
  GetActiveNames
mkGetActiveNames = GetActiveNames' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganPageToken :: Lens.Lens' GetActiveNames (Lude.Maybe Lude.Text)
ganPageToken = Lens.lens (pageToken :: GetActiveNames -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetActiveNames)
{-# DEPRECATED ganPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetActiveNames where
  page rq rs
    | Page.stop (rs Lens.^. ganrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ganrsActiveNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ganPageToken Lens..~ rs Lens.^. ganrsNextPageToken

instance Lude.AWSRequest GetActiveNames where
  type Rs GetActiveNames = GetActiveNamesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetActiveNamesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "activeNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetActiveNames where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetActiveNames" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetActiveNames where
  toJSON GetActiveNames' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetActiveNames where
  toPath = Lude.const "/"

instance Lude.ToQuery GetActiveNames where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    activeNames :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'GetActiveNamesResponse' with the minimum fields required to make a request.
--
-- * 'activeNames' - The list of active names returned by the get active names request.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetActiveNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetActiveNamesResponse
mkGetActiveNamesResponse pResponseStatus_ =
  GetActiveNamesResponse'
    { nextPageToken = Lude.Nothing,
      activeNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrsNextPageToken :: Lens.Lens' GetActiveNamesResponse (Lude.Maybe Lude.Text)
ganrsNextPageToken = Lens.lens (nextPageToken :: GetActiveNamesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetActiveNamesResponse)
{-# DEPRECATED ganrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The list of active names returned by the get active names request.
--
-- /Note:/ Consider using 'activeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrsActiveNames :: Lens.Lens' GetActiveNamesResponse (Lude.Maybe [Lude.Text])
ganrsActiveNames = Lens.lens (activeNames :: GetActiveNamesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {activeNames = a} :: GetActiveNamesResponse)
{-# DEPRECATED ganrsActiveNames "Use generic-lens or generic-optics with 'activeNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ganrsResponseStatus :: Lens.Lens' GetActiveNamesResponse Lude.Int
ganrsResponseStatus = Lens.lens (responseStatus :: GetActiveNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetActiveNamesResponse)
{-# DEPRECATED ganrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
