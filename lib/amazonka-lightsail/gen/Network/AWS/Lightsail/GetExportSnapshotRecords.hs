{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetExportSnapshotRecords
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the export snapshot record created as a result of the @export snapshot@ operation.
--
-- An export snapshot record can be used to create a new Amazon EC2 instance and its related resources with the @create cloud formation stack@ operation.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetExportSnapshotRecords
  ( -- * Creating a request
    GetExportSnapshotRecords (..),
    mkGetExportSnapshotRecords,

    -- ** Request lenses
    gesrPageToken,

    -- * Destructuring the response
    GetExportSnapshotRecordsResponse (..),
    mkGetExportSnapshotRecordsResponse,

    -- ** Response lenses
    gesrrsNextPageToken,
    gesrrsExportSnapshotRecords,
    gesrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetExportSnapshotRecords' smart constructor.
newtype GetExportSnapshotRecords = GetExportSnapshotRecords'
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

-- | Creates a value of 'GetExportSnapshotRecords' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetExportSnapshotRecords ::
  GetExportSnapshotRecords
mkGetExportSnapshotRecords =
  GetExportSnapshotRecords' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrPageToken :: Lens.Lens' GetExportSnapshotRecords (Lude.Maybe Lude.Text)
gesrPageToken = Lens.lens (pageToken :: GetExportSnapshotRecords -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetExportSnapshotRecords)
{-# DEPRECATED gesrPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetExportSnapshotRecords where
  page rq rs
    | Page.stop (rs Lens.^. gesrrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gesrrsExportSnapshotRecords) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gesrPageToken Lens..~ rs Lens.^. gesrrsNextPageToken

instance Lude.AWSRequest GetExportSnapshotRecords where
  type Rs GetExportSnapshotRecords = GetExportSnapshotRecordsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetExportSnapshotRecordsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "exportSnapshotRecords" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetExportSnapshotRecords where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetExportSnapshotRecords" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetExportSnapshotRecords where
  toJSON GetExportSnapshotRecords' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetExportSnapshotRecords where
  toPath = Lude.const "/"

instance Lude.ToQuery GetExportSnapshotRecords where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetExportSnapshotRecordsResponse' smart constructor.
data GetExportSnapshotRecordsResponse = GetExportSnapshotRecordsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    exportSnapshotRecords ::
      Lude.Maybe
        [ExportSnapshotRecord],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetExportSnapshotRecordsResponse' with the minimum fields required to make a request.
--
-- * 'exportSnapshotRecords' - A list of objects describing the export snapshot records.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetExportSnapshotRecords@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetExportSnapshotRecordsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetExportSnapshotRecordsResponse
mkGetExportSnapshotRecordsResponse pResponseStatus_ =
  GetExportSnapshotRecordsResponse'
    { nextPageToken = Lude.Nothing,
      exportSnapshotRecords = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetExportSnapshotRecords@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsNextPageToken :: Lens.Lens' GetExportSnapshotRecordsResponse (Lude.Maybe Lude.Text)
gesrrsNextPageToken = Lens.lens (nextPageToken :: GetExportSnapshotRecordsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetExportSnapshotRecordsResponse)
{-# DEPRECATED gesrrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | A list of objects describing the export snapshot records.
--
-- /Note:/ Consider using 'exportSnapshotRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsExportSnapshotRecords :: Lens.Lens' GetExportSnapshotRecordsResponse (Lude.Maybe [ExportSnapshotRecord])
gesrrsExportSnapshotRecords = Lens.lens (exportSnapshotRecords :: GetExportSnapshotRecordsResponse -> Lude.Maybe [ExportSnapshotRecord]) (\s a -> s {exportSnapshotRecords = a} :: GetExportSnapshotRecordsResponse)
{-# DEPRECATED gesrrsExportSnapshotRecords "Use generic-lens or generic-optics with 'exportSnapshotRecords' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsResponseStatus :: Lens.Lens' GetExportSnapshotRecordsResponse Lude.Int
gesrrsResponseStatus = Lens.lens (responseStatus :: GetExportSnapshotRecordsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetExportSnapshotRecordsResponse)
{-# DEPRECATED gesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
