{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of events for a specific database in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseEvents
  ( -- * Creating a request
    GetRelationalDatabaseEvents (..),
    mkGetRelationalDatabaseEvents,

    -- ** Request lenses
    grdeDurationInMinutes,
    grdePageToken,
    grdeRelationalDatabaseName,

    -- * Destructuring the response
    GetRelationalDatabaseEventsResponse (..),
    mkGetRelationalDatabaseEventsResponse,

    -- ** Response lenses
    grdersNextPageToken,
    grdersRelationalDatabaseEvents,
    grdersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseEvents' smart constructor.
data GetRelationalDatabaseEvents = GetRelationalDatabaseEvents'
  { durationInMinutes ::
      Lude.Maybe Lude.Int,
    pageToken :: Lude.Maybe Lude.Text,
    relationalDatabaseName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRelationalDatabaseEvents' with the minimum fields required to make a request.
--
-- * 'durationInMinutes' - The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
-- * 'relationalDatabaseName' - The name of the database from which to get events.
mkGetRelationalDatabaseEvents ::
  -- | 'relationalDatabaseName'
  Lude.Text ->
  GetRelationalDatabaseEvents
mkGetRelationalDatabaseEvents pRelationalDatabaseName_ =
  GetRelationalDatabaseEvents'
    { durationInMinutes = Lude.Nothing,
      pageToken = Lude.Nothing,
      relationalDatabaseName = pRelationalDatabaseName_
    }

-- | The number of minutes in the past from which to retrieve events. For example, to get all events from the past 2 hours, enter 120.
--
-- Default: @60@
-- The minimum is 1 and the maximum is 14 days (20160 minutes).
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdeDurationInMinutes :: Lens.Lens' GetRelationalDatabaseEvents (Lude.Maybe Lude.Int)
grdeDurationInMinutes = Lens.lens (durationInMinutes :: GetRelationalDatabaseEvents -> Lude.Maybe Lude.Int) (\s a -> s {durationInMinutes = a} :: GetRelationalDatabaseEvents)
{-# DEPRECATED grdeDurationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseEvents@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdePageToken :: Lens.Lens' GetRelationalDatabaseEvents (Lude.Maybe Lude.Text)
grdePageToken = Lens.lens (pageToken :: GetRelationalDatabaseEvents -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseEvents)
{-# DEPRECATED grdePageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

-- | The name of the database from which to get events.
--
-- /Note:/ Consider using 'relationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdeRelationalDatabaseName :: Lens.Lens' GetRelationalDatabaseEvents Lude.Text
grdeRelationalDatabaseName = Lens.lens (relationalDatabaseName :: GetRelationalDatabaseEvents -> Lude.Text) (\s a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseEvents)
{-# DEPRECATED grdeRelationalDatabaseName "Use generic-lens or generic-optics with 'relationalDatabaseName' instead." #-}

instance Page.AWSPager GetRelationalDatabaseEvents where
  page rq rs
    | Page.stop (rs Lens.^. grdersNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdersRelationalDatabaseEvents) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdePageToken Lens..~ rs Lens.^. grdersNextPageToken

instance Lude.AWSRequest GetRelationalDatabaseEvents where
  type
    Rs GetRelationalDatabaseEvents =
      GetRelationalDatabaseEventsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseEventsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "relationalDatabaseEvents" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseEvents" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseEvents where
  toJSON GetRelationalDatabaseEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("durationInMinutes" Lude..=) Lude.<$> durationInMinutes,
            ("pageToken" Lude..=) Lude.<$> pageToken,
            Lude.Just
              ("relationalDatabaseName" Lude..= relationalDatabaseName)
          ]
      )

instance Lude.ToPath GetRelationalDatabaseEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseEventsResponse' smart constructor.
data GetRelationalDatabaseEventsResponse = GetRelationalDatabaseEventsResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    relationalDatabaseEvents ::
      Lude.Maybe
        [RelationalDatabaseEvent],
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

-- | Creates a value of 'GetRelationalDatabaseEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseEvents@ request and specify the next page token using the @pageToken@ parameter.
-- * 'relationalDatabaseEvents' - An object describing the result of your get relational database events request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseEventsResponse
mkGetRelationalDatabaseEventsResponse pResponseStatus_ =
  GetRelationalDatabaseEventsResponse'
    { nextPageToken =
        Lude.Nothing,
      relationalDatabaseEvents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseEvents@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdersNextPageToken :: Lens.Lens' GetRelationalDatabaseEventsResponse (Lude.Maybe Lude.Text)
grdersNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabaseEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabaseEventsResponse)
{-# DEPRECATED grdersNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational database events request.
--
-- /Note:/ Consider using 'relationalDatabaseEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdersRelationalDatabaseEvents :: Lens.Lens' GetRelationalDatabaseEventsResponse (Lude.Maybe [RelationalDatabaseEvent])
grdersRelationalDatabaseEvents = Lens.lens (relationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> Lude.Maybe [RelationalDatabaseEvent]) (\s a -> s {relationalDatabaseEvents = a} :: GetRelationalDatabaseEventsResponse)
{-# DEPRECATED grdersRelationalDatabaseEvents "Use generic-lens or generic-optics with 'relationalDatabaseEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdersResponseStatus :: Lens.Lens' GetRelationalDatabaseEventsResponse Lude.Int
grdersResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseEventsResponse)
{-# DEPRECATED grdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
