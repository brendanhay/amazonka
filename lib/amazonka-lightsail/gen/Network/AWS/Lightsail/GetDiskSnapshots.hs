{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDiskSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disk snapshots in your AWS account and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDiskSnapshots
  ( -- * Creating a request
    GetDiskSnapshots (..),
    mkGetDiskSnapshots,

    -- ** Request lenses
    gdsPageToken,

    -- * Destructuring the response
    GetDiskSnapshotsResponse (..),
    mkGetDiskSnapshotsResponse,

    -- ** Response lenses
    gdssrsNextPageToken,
    gdssrsDiskSnapshots,
    gdssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDiskSnapshots' smart constructor.
newtype GetDiskSnapshots = GetDiskSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDiskSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiskSnapshots' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDiskSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetDiskSnapshots ::
  GetDiskSnapshots
mkGetDiskSnapshots = GetDiskSnapshots' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDiskSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsPageToken :: Lens.Lens' GetDiskSnapshots (Lude.Maybe Lude.Text)
gdsPageToken = Lens.lens (pageToken :: GetDiskSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetDiskSnapshots)
{-# DEPRECATED gdsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetDiskSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. gdssrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gdssrsDiskSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gdsPageToken Lens..~ rs Lens.^. gdssrsNextPageToken

instance Lude.AWSRequest GetDiskSnapshots where
  type Rs GetDiskSnapshots = GetDiskSnapshotsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDiskSnapshotsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "diskSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDiskSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDiskSnapshots" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDiskSnapshots where
  toJSON GetDiskSnapshots' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetDiskSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDiskSnapshots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDiskSnapshotsResponse' smart constructor.
data GetDiskSnapshotsResponse = GetDiskSnapshotsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetDiskSnapshots@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of objects containing information about all block storage disk snapshots.
    diskSnapshots :: Lude.Maybe [DiskSnapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDiskSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDiskSnapshots@ request and specify the next page token using the @pageToken@ parameter.
-- * 'diskSnapshots' - An array of objects containing information about all block storage disk snapshots.
-- * 'responseStatus' - The response status code.
mkGetDiskSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDiskSnapshotsResponse
mkGetDiskSnapshotsResponse pResponseStatus_ =
  GetDiskSnapshotsResponse'
    { nextPageToken = Lude.Nothing,
      diskSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetDiskSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdssrsNextPageToken :: Lens.Lens' GetDiskSnapshotsResponse (Lude.Maybe Lude.Text)
gdssrsNextPageToken = Lens.lens (nextPageToken :: GetDiskSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetDiskSnapshotsResponse)
{-# DEPRECATED gdssrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects containing information about all block storage disk snapshots.
--
-- /Note:/ Consider using 'diskSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdssrsDiskSnapshots :: Lens.Lens' GetDiskSnapshotsResponse (Lude.Maybe [DiskSnapshot])
gdssrsDiskSnapshots = Lens.lens (diskSnapshots :: GetDiskSnapshotsResponse -> Lude.Maybe [DiskSnapshot]) (\s a -> s {diskSnapshots = a} :: GetDiskSnapshotsResponse)
{-# DEPRECATED gdssrsDiskSnapshots "Use generic-lens or generic-optics with 'diskSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdssrsResponseStatus :: Lens.Lens' GetDiskSnapshotsResponse Lude.Int
gdssrsResponseStatus = Lens.lens (responseStatus :: GetDiskSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDiskSnapshotsResponse)
{-# DEPRECATED gdssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
