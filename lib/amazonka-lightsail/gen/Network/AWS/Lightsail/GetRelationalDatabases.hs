{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all of your databases in Amazon Lightsail.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabases
  ( -- * Creating a request
    GetRelationalDatabases (..),
    mkGetRelationalDatabases,

    -- ** Request lenses
    grdPageToken,

    -- * Destructuring the response
    GetRelationalDatabasesResponse (..),
    mkGetRelationalDatabasesResponse,

    -- ** Response lenses
    grdrsNextPageToken,
    grdrsRelationalDatabases,
    grdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabases' smart constructor.
newtype GetRelationalDatabases = GetRelationalDatabases'
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

-- | Creates a value of 'GetRelationalDatabases' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetRelationalDatabases ::
  GetRelationalDatabases
mkGetRelationalDatabases =
  GetRelationalDatabases' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabases@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdPageToken :: Lens.Lens' GetRelationalDatabases (Lude.Maybe Lude.Text)
grdPageToken = Lens.lens (pageToken :: GetRelationalDatabases -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabases)
{-# DEPRECATED grdPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetRelationalDatabases where
  page rq rs
    | Page.stop (rs Lens.^. grdrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdrsRelationalDatabases) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdPageToken Lens..~ rs Lens.^. grdrsNextPageToken

instance Lude.AWSRequest GetRelationalDatabases where
  type Rs GetRelationalDatabases = GetRelationalDatabasesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabasesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "relationalDatabases" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabases where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetRelationalDatabases" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabases where
  toJSON GetRelationalDatabases' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetRelationalDatabases where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabases where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabasesResponse' smart constructor.
data GetRelationalDatabasesResponse = GetRelationalDatabasesResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    relationalDatabases ::
      Lude.Maybe
        [RelationalDatabase],
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

-- | Creates a value of 'GetRelationalDatabasesResponse' with the minimum fields required to make a request.
--
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabases@ request and specify the next page token using the @pageToken@ parameter.
-- * 'relationalDatabases' - An object describing the result of your get relational databases request.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabasesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabasesResponse
mkGetRelationalDatabasesResponse pResponseStatus_ =
  GetRelationalDatabasesResponse'
    { nextPageToken = Lude.Nothing,
      relationalDatabases = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabases@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsNextPageToken :: Lens.Lens' GetRelationalDatabasesResponse (Lude.Maybe Lude.Text)
grdrsNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabasesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabasesResponse)
{-# DEPRECATED grdrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational databases request.
--
-- /Note:/ Consider using 'relationalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsRelationalDatabases :: Lens.Lens' GetRelationalDatabasesResponse (Lude.Maybe [RelationalDatabase])
grdrsRelationalDatabases = Lens.lens (relationalDatabases :: GetRelationalDatabasesResponse -> Lude.Maybe [RelationalDatabase]) (\s a -> s {relationalDatabases = a} :: GetRelationalDatabasesResponse)
{-# DEPRECATED grdrsRelationalDatabases "Use generic-lens or generic-optics with 'relationalDatabases' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdrsResponseStatus :: Lens.Lens' GetRelationalDatabasesResponse Lude.Int
grdrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabasesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabasesResponse)
{-# DEPRECATED grdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
