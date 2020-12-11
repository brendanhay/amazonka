{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDisks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disks in your AWS account and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDisks
  ( -- * Creating a request
    GetDisks (..),
    mkGetDisks,

    -- ** Request lenses
    getPageToken,

    -- * Destructuring the response
    GetDisksResponse (..),
    mkGetDisksResponse,

    -- ** Response lenses
    getersNextPageToken,
    getersDisks,
    getersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDisks' smart constructor.
newtype GetDisks = GetDisks' {pageToken :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDisks' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetDisks ::
  GetDisks
mkGetDisks = GetDisks' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getPageToken :: Lens.Lens' GetDisks (Lude.Maybe Lude.Text)
getPageToken = Lens.lens (pageToken :: GetDisks -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetDisks)
{-# DEPRECATED getPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetDisks where
  page rq rs
    | Page.stop (rs Lens.^. getersNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. getersDisks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& getPageToken Lens..~ rs Lens.^. getersNextPageToken

instance Lude.AWSRequest GetDisks where
  type Rs GetDisks = GetDisksResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDisksResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "disks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDisks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDisks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDisks where
  toJSON GetDisks' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetDisks where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDisks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDisksResponse' smart constructor.
data GetDisksResponse = GetDisksResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    disks :: Lude.Maybe [Disk],
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

-- | Creates a value of 'GetDisksResponse' with the minimum fields required to make a request.
--
-- * 'disks' - An array of objects containing information about all block storage disks.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetDisksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDisksResponse
mkGetDisksResponse pResponseStatus_ =
  GetDisksResponse'
    { nextPageToken = Lude.Nothing,
      disks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getersNextPageToken :: Lens.Lens' GetDisksResponse (Lude.Maybe Lude.Text)
getersNextPageToken = Lens.lens (nextPageToken :: GetDisksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDisksResponse)
{-# DEPRECATED getersNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getersDisks :: Lens.Lens' GetDisksResponse (Lude.Maybe [Disk])
getersDisks = Lens.lens (disks :: GetDisksResponse -> Lude.Maybe [Disk]) (\s a -> s {disks = a} :: GetDisksResponse)
{-# DEPRECATED getersDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getersResponseStatus :: Lens.Lens' GetDisksResponse Lude.Int
getersResponseStatus = Lens.lens (responseStatus :: GetDisksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDisksResponse)
{-# DEPRECATED getersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
