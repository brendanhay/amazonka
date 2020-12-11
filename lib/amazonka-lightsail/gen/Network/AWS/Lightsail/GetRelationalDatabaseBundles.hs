{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A bundle describes the performance specifications for a database.
--
-- You can use a bundle ID to create a new database with explicit performance specifications.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBundles
  ( -- * Creating a request
    GetRelationalDatabaseBundles (..),
    mkGetRelationalDatabaseBundles,

    -- ** Request lenses
    grdbsPageToken,

    -- * Destructuring the response
    GetRelationalDatabaseBundlesResponse (..),
    mkGetRelationalDatabaseBundlesResponse,

    -- ** Response lenses
    grdbrsNextPageToken,
    grdbrsBundles,
    grdbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRelationalDatabaseBundles' smart constructor.
newtype GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
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

-- | Creates a value of 'GetRelationalDatabaseBundles' with the minimum fields required to make a request.
--
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetRelationalDatabaseBundles ::
  GetRelationalDatabaseBundles
mkGetRelationalDatabaseBundles =
  GetRelationalDatabaseBundles' {pageToken = Lude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbsPageToken :: Lens.Lens' GetRelationalDatabaseBundles (Lude.Maybe Lude.Text)
grdbsPageToken = Lens.lens (pageToken :: GetRelationalDatabaseBundles -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetRelationalDatabaseBundles)
{-# DEPRECATED grdbsPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetRelationalDatabaseBundles where
  page rq rs
    | Page.stop (rs Lens.^. grdbrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grdbrsBundles) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grdbsPageToken Lens..~ rs Lens.^. grdbrsNextPageToken

instance Lude.AWSRequest GetRelationalDatabaseBundles where
  type
    Rs GetRelationalDatabaseBundles =
      GetRelationalDatabaseBundlesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBundlesResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (x Lude..?> "bundles" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRelationalDatabaseBundles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetRelationalDatabaseBundles" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRelationalDatabaseBundles where
  toJSON GetRelationalDatabaseBundles' {..} =
    Lude.object
      (Lude.catMaybes [("pageToken" Lude..=) Lude.<$> pageToken])

instance Lude.ToPath GetRelationalDatabaseBundles where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRelationalDatabaseBundles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { nextPageToken ::
      Lude.Maybe
        Lude.Text,
    bundles ::
      Lude.Maybe
        [RelationalDatabaseBundle],
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

-- | Creates a value of 'GetRelationalDatabaseBundlesResponse' with the minimum fields required to make a request.
--
-- * 'bundles' - An object describing the result of your get relational database bundles request.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetRelationalDatabaseBundlesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRelationalDatabaseBundlesResponse
mkGetRelationalDatabaseBundlesResponse pResponseStatus_ =
  GetRelationalDatabaseBundlesResponse'
    { nextPageToken =
        Lude.Nothing,
      bundles = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrsNextPageToken :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Lude.Maybe Lude.Text)
grdbrsNextPageToken = Lens.lens (nextPageToken :: GetRelationalDatabaseBundlesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetRelationalDatabaseBundlesResponse)
{-# DEPRECATED grdbrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object describing the result of your get relational database bundles request.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrsBundles :: Lens.Lens' GetRelationalDatabaseBundlesResponse (Lude.Maybe [RelationalDatabaseBundle])
grdbrsBundles = Lens.lens (bundles :: GetRelationalDatabaseBundlesResponse -> Lude.Maybe [RelationalDatabaseBundle]) (\s a -> s {bundles = a} :: GetRelationalDatabaseBundlesResponse)
{-# DEPRECATED grdbrsBundles "Use generic-lens or generic-optics with 'bundles' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdbrsResponseStatus :: Lens.Lens' GetRelationalDatabaseBundlesResponse Lude.Int
grdbrsResponseStatus = Lens.lens (responseStatus :: GetRelationalDatabaseBundlesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRelationalDatabaseBundlesResponse)
{-# DEPRECATED grdbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
