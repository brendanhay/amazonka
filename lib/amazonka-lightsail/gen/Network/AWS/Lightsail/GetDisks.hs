{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gdfPageToken,

    -- * Destructuring the response
    GetDisksResponse (..),
    mkGetDisksResponse,

    -- ** Response lenses
    gdlrsNextPageToken,
    gdlrsDisks,
    gdlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDisks' smart constructor.
newtype GetDisks = GetDisks'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
gdfPageToken :: Lens.Lens' GetDisks (Lude.Maybe Lude.Text)
gdfPageToken = Lens.lens (pageToken :: GetDisks -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetDisks)
{-# DEPRECATED gdfPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetDisks where
  page rq rs
    | Page.stop (rs Lens.^. gdlrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdlrsDisks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdfPageToken Lens..~ rs Lens.^. gdlrsNextPageToken

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
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects containing information about all block storage disks.
    disks :: Lude.Maybe [Disk],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDisksResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
-- * 'disks' - An array of objects containing information about all block storage disks.
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
gdlrsNextPageToken :: Lens.Lens' GetDisksResponse (Lude.Maybe Lude.Text)
gdlrsNextPageToken = Lens.lens (nextPageToken :: GetDisksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDisksResponse)
{-# DEPRECATED gdlrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrsDisks :: Lens.Lens' GetDisksResponse (Lude.Maybe [Disk])
gdlrsDisks = Lens.lens (disks :: GetDisksResponse -> Lude.Maybe [Disk]) (\s a -> s {disks = a} :: GetDisksResponse)
{-# DEPRECATED gdlrsDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrsResponseStatus :: Lens.Lens' GetDisksResponse Lude.Int
gdlrsResponseStatus = Lens.lens (responseStatus :: GetDisksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDisksResponse)
{-# DEPRECATED gdlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
