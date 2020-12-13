{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your database snapshots in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
  ( -- * Creating a request
    GetRelationalDatabaseSnapshots (..),
    mkGetRelationalDatabaseSnapshots,

    -- ** Request lenses
    grdsPageToken,

    -- * Destructuring the response
    GetRelationalDatabaseSnapshotsResponse (..),
    mkGetRelationalDatabaseSnapshotsResponse,

    -- ** Response lenses
    grdssrsNextPageToken,
    grdssrsRelationalDatabaseSnapshots,
    grdssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseSnapshots' smart constructor.
newtype GetRelationalDatabaseSnapshots = GetRelationalDatabaseSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseSnapshots' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetRelationalDatabaseSnapshots ::
  GetRelationalDatabaseSnapshots
mkGetRelationalDatabaseSnapshots =
  GetRelationalDatabaseSnapshots' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseSnapshots@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsPageToken :: Lens.Lens' GetRelationalDatabaseSnapshots (Lude.Maybe Lude.Text)
grdsPageToken = Lens.lens (pageToken :: GetRelationalDatabaseSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseSnapshots)
{-# DEPRECATED grdsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetRelationalDatabaseSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. grdssrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdssrsRelationalDatabaseSnapshots) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdsPageToken Lens..~ rs Lens.^. grdssrsNextPageToken

instance Lude.AWSRequest GetRelationalDatabaseSnapshots where
  type
    Rs GetRelationalDatabaseSnapshots =
      GetRelationalDatabaseSnapshotsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseSnapshotsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "relationalDatabaseSnapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseSnapshots" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseSnapshots where
  toJSON GetRelationalDatabaseSnapshots' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetRelationalDatabaseSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseSnapshots where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseSnapshotsResponse' smart constructor.
data GetRelationalDatabaseSnapshotsResponse = GetRelationalDatabaseSnapshotsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetRelationalDatabaseSnapshots@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | An object describing the result of your get relational database snapshots request.
    relationalDatabaseSnapshots :: Lude.Maybe [RelationalDatabaseSnapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseSnapshots@ request and specify the next page token using the @pageToken@ parameter.
-- * 'relationalDatabaseSnapshots' - An object describing the result of your get relational database snapshots request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseSnapshotsResponse
mkGetRelationalDatabaseSnapshotsResponse pResponseStatus_ =
  GetRelationalDatabaseSnapshotsResponse'
    { nextPageToken =
        Lude.Nothing,
      relationalDatabaseSnapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseSnapshots@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdssrsNextPageToken :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Lude.Maybe Lude.Text)
grdssrsNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabaseSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabaseSnapshotsResponse)
{-# DEPRECATED grdssrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational database snapshots request.
--
-- /Note:/ Consider using 'relationalDatabaseSnapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdssrsRelationalDatabaseSnapshots :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse (Lude.Maybe [RelationalDatabaseSnapshot])
grdssrsRelationalDatabaseSnapshots = Lens.lens (relationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> Lude.Maybe [RelationalDatabaseSnapshot]) (\s a -> s {relationalDatabaseSnapshots = a} :: GetRelationalDatabaseSnapshotsResponse)
{-# DEPRECATED grdssrsRelationalDatabaseSnapshots "Use generic-lens or generic-optics with 'relationalDatabaseSnapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdssrsResponseStatus :: Lens.Lens' GetRelationalDatabaseSnapshotsResponse Lude.Int
grdssrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseSnapshotsResponse)
{-# DEPRECATED grdssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
