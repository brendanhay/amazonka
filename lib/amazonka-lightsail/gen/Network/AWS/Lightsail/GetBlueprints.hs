{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetBlueprints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available instance images, or /blueprints/ . You can use a blueprint to create a new instance already running a specific operating system, as well as a preinstalled app or development stack. The software each instance is running depends on the blueprint image you choose.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetBlueprints
  ( -- * Creating a request
    GetBlueprints (..),
    mkGetBlueprints,

    -- ** Request lenses
    gbIncludeInactive,
    gbPageToken,

    -- * Destructuring the response
    GetBlueprintsResponse (..),
    mkGetBlueprintsResponse,

    -- ** Response lenses
    gbsrsBlueprints,
    gbsrsNextPageToken,
    gbsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBlueprints' smart constructor.
data GetBlueprints = GetBlueprints'
  { -- | A Boolean value indicating whether to include inactive results in your request.
    includeInactive :: Lude.Maybe Lude.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlueprints' with the minimum fields required to make a request.
--
-- * 'includeInactive' - A Boolean value indicating whether to include inactive results in your request.
-- * 'pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
mkGetBlueprints ::
  GetBlueprints
mkGetBlueprints =
  GetBlueprints'
    { includeInactive = Lude.Nothing,
      pageToken = Lude.Nothing
    }

-- | A Boolean value indicating whether to include inactive results in your request.
--
-- /Note:/ Consider using 'includeInactive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbIncludeInactive :: Lens.Lens' GetBlueprints (Lude.Maybe Lude.Bool)
gbIncludeInactive = Lens.lens (includeInactive :: GetBlueprints -> Lude.Maybe Lude.Bool) (\s a -> s {includeInactive = a} :: GetBlueprints)
{-# DEPRECATED gbIncludeInactive "Use generic-lens or generic-optics with 'includeInactive' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBlueprints@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbPageToken :: Lens.Lens' GetBlueprints (Lude.Maybe Lude.Text)
gbPageToken = Lens.lens (pageToken :: GetBlueprints -> Lude.Maybe Lude.Text) (\s a -> s {pageToken = a} :: GetBlueprints)
{-# DEPRECATED gbPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Page.AWSPager GetBlueprints where
  page rq rs
    | Page.stop (rs Lens.^. gbsrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gbsrsBlueprints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gbPageToken Lens..~ rs Lens.^. gbsrsNextPageToken

instance Lude.AWSRequest GetBlueprints where
  type Rs GetBlueprints = GetBlueprintsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBlueprintsResponse'
            Lude.<$> (x Lude..?> "blueprints" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBlueprints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetBlueprints" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBlueprints where
  toJSON GetBlueprints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeInactive" Lude..=) Lude.<$> includeInactive,
            ("pageToken" Lude..=) Lude.<$> pageToken
          ]
      )

instance Lude.ToPath GetBlueprints where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBlueprints where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetBlueprintsResponse' smart constructor.
data GetBlueprintsResponse = GetBlueprintsResponse'
  { -- | An array of key-value pairs that contains information about the available blueprints.
    blueprints :: Lude.Maybe [Blueprint],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetBlueprints@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBlueprintsResponse' with the minimum fields required to make a request.
--
-- * 'blueprints' - An array of key-value pairs that contains information about the available blueprints.
-- * 'nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBlueprints@ request and specify the next page token using the @pageToken@ parameter.
-- * 'responseStatus' - The response status code.
mkGetBlueprintsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBlueprintsResponse
mkGetBlueprintsResponse pResponseStatus_ =
  GetBlueprintsResponse'
    { blueprints = Lude.Nothing,
      nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs that contains information about the available blueprints.
--
-- /Note:/ Consider using 'blueprints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsBlueprints :: Lens.Lens' GetBlueprintsResponse (Lude.Maybe [Blueprint])
gbsrsBlueprints = Lens.lens (blueprints :: GetBlueprintsResponse -> Lude.Maybe [Blueprint]) (\s a -> s {blueprints = a} :: GetBlueprintsResponse)
{-# DEPRECATED gbsrsBlueprints "Use generic-lens or generic-optics with 'blueprints' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetBlueprints@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsNextPageToken :: Lens.Lens' GetBlueprintsResponse (Lude.Maybe Lude.Text)
gbsrsNextPageToken = Lens.lens (nextPageToken :: GetBlueprintsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: GetBlueprintsResponse)
{-# DEPRECATED gbsrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbsrsResponseStatus :: Lens.Lens' GetBlueprintsResponse Lude.Int
gbsrsResponseStatus = Lens.lens (responseStatus :: GetBlueprintsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBlueprintsResponse)
{-# DEPRECATED gbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
