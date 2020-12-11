{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available database blueprints in Amazon Lightsail. A blueprint describes the major engine version of a database.
--
-- You can use a blueprint ID to create a new database that runs a specific database engine.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
  ( -- * Creating a request
    GetRelationalDatabaseBlueprints (..),
    mkGetRelationalDatabaseBlueprints,

    -- ** Request lenses
    grdbPageToken,

    -- * Destructuring the response
    GetRelationalDatabaseBlueprintsResponse (..),
    mkGetRelationalDatabaseBlueprintsResponse,

    -- ** Response lenses
    grdbsrsBlueprints,
    grdbsrsNextPageToken,
    grdbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseBlueprints' smart constructor.
newtype GetRelationalDatabaseBlueprints = GetRelationalDatabaseBlueprints'
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

-- | Creates a value of 'GetRelationalDatabaseBlueprints' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetRelationalDatabaseBlueprints ::
  GetRelationalDatabaseBlueprints
mkGetRelationalDatabaseBlueprints =
  GetRelationalDatabaseBlueprints' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbPageToken :: Lens.Lens' GetRelationalDatabaseBlueprints (Lude.Maybe Lude.Text)
grdbPageToken = Lens.lens (pageToken :: GetRelationalDatabaseBlueprints -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseBlueprints)
{-# DEPRECATED grdbPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetRelationalDatabaseBlueprints where
  page rq rs
    | Page.stop (rs Lens.^. grdbsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdbsrsBlueprints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdbPageToken Lens..~ rs Lens.^. grdbsrsNextPageToken

instance Lude.AWSRequest GetRelationalDatabaseBlueprints where
  type
    Rs GetRelationalDatabaseBlueprints =
      GetRelationalDatabaseBlueprintsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBlueprintsResponse'
            Lude.<$> (x Lude..?> "blueprints" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseBlueprints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseBlueprints" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseBlueprints where
  toJSON GetRelationalDatabaseBlueprints' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetRelationalDatabaseBlueprints where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseBlueprints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseBlueprintsResponse' smart constructor.
data GetRelationalDatabaseBlueprintsResponse = GetRelationalDatabaseBlueprintsResponse'
  { blueprints ::
      Lude.Maybe
        [RelationalDatabaseBlueprint],
    nextPageToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'GetRelationalDatabaseBlueprintsResponse' with the minimum fields required to make a request.
--
-- * 'blueprints' - An object describing the result of your get relational database blueprints request.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseBlueprintsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseBlueprintsResponse
mkGetRelationalDatabaseBlueprintsResponse pResponseStatus_ =
  GetRelationalDatabaseBlueprintsResponse'
    { blueprints =
        Lude.Nothing,
      nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object describing the result of your get relational database blueprints request.
--
-- /Note:/ Consider using 'blueprints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbsrsBlueprints :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Lude.Maybe [RelationalDatabaseBlueprint])
grdbsrsBlueprints = Lens.lens (blueprints :: GetRelationalDatabaseBlueprintsResponse -> Lude.Maybe [RelationalDatabaseBlueprint]) (\s a -> s {blueprints = a} :: GetRelationalDatabaseBlueprintsResponse)
{-# DEPRECATED grdbsrsBlueprints "Use generic-lens or generic-optics with 'blueprints' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBlueprints@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbsrsNextPageToken :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse (Lude.Maybe Lude.Text)
grdbsrsNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabaseBlueprintsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabaseBlueprintsResponse)
{-# DEPRECATED grdbsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbsrsResponseStatus :: Lens.Lens' GetRelationalDatabaseBlueprintsResponse Lude.Int
grdbsrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseBlueprintsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseBlueprintsResponse)
{-# DEPRECATED grdbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
