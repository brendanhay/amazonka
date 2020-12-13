{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all instance snapshots for the user's account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstanceSnapshots
  ( -- * Creating a request
    GetInstanceSnapshots (..),
    mkGetInstanceSnapshots,

    -- ** Request lenses
    gisPageToken,

    -- * Destructuring the response
    GetInstanceSnapshotsResponse (..),
    mkGetInstanceSnapshotsResponse,

    -- ** Response lenses
    gissrsNextPageToken,
    gissrsInstanceSnapshots,
    gissrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceSnapshots' smart constructor.
newtype GetInstanceSnapshots = GetInstanceSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstanceSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceSnapshots' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetInstanceSnapshots ::
  GetInstanceSnapshots
mkGetInstanceSnapshots =
  GetInstanceSnapshots' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisPageToken :: Lens.Lens' GetInstanceSnapshots (Lude.Maybe Lude.Text)
gisPageToken = Lens.lens (pageToken :: GetInstanceSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetInstanceSnapshots)
{-# DEPRECATED gisPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetInstanceSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. gissrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gissrsInstanceSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gisPageToken Lens..~ rs Lens.^. gissrsNextPageToken

instance Lude.AWSRequest GetInstanceSnapshots where
  type Rs GetInstanceSnapshots = GetInstanceSnapshotsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "instanceSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstanceSnapshots" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceSnapshots where
  toJSON GetInstanceSnapshots' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetInstanceSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceSnapshots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceSnapshotsResponse' smart constructor.
data GetInstanceSnapshotsResponse = GetInstanceSnapshotsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetInstanceSnapshots@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An array of key-value pairs containing information about the results of your get instance snapshots request.
    instanceSnapshots :: Lude.Maybe [InstanceSnapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstanceSnapshots@ request and specify the next page token using the @pageToken@ parameter.
-- * 'instanceSnapshots' - An array of key-value pairs containing information about the results of your get instance snapshots request.
-- * 'responseStatus' - The response status code.
mkGetInstanceSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceSnapshotsResponse
mkGetInstanceSnapshotsResponse pResponseStatus_ =
  GetInstanceSnapshotsResponse'
    { nextPageToken = Lude.Nothing,
      instanceSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstanceSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gissrsNextPageToken :: Lens.Lens' GetInstanceSnapshotsResponse (Lude.Maybe Lude.Text)
gissrsNextPageToken = Lens.lens (nextPageToken :: GetInstanceSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetInstanceSnapshotsResponse)
{-# DEPRECATED gissrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of key-value pairs containing information about the results of your get instance snapshots request.
--
-- /Note:/ Consider using 'instanceSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gissrsInstanceSnapshots :: Lens.Lens' GetInstanceSnapshotsResponse (Lude.Maybe [InstanceSnapshot])
gissrsInstanceSnapshots = Lens.lens (instanceSnapshots :: GetInstanceSnapshotsResponse -> Lude.Maybe [InstanceSnapshot]) (\s a -> s {instanceSnapshots = a} :: GetInstanceSnapshotsResponse)
{-# DEPRECATED gissrsInstanceSnapshots "Use generic-lens or generic-optics with 'instanceSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gissrsResponseStatus :: Lens.Lens' GetInstanceSnapshotsResponse Lude.Int
gissrsResponseStatus = Lens.lens (responseStatus :: GetInstanceSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceSnapshotsResponse)
{-# DEPRECATED gissrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
