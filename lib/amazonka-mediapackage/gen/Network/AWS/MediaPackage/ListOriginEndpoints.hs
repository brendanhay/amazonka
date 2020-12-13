{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListOriginEndpoints
  ( -- * Creating a request
    ListOriginEndpoints (..),
    mkListOriginEndpoints,

    -- ** Request lenses
    loeChannelId,
    loeNextToken,
    loeMaxResults,

    -- * Destructuring the response
    ListOriginEndpointsResponse (..),
    mkListOriginEndpointsResponse,

    -- ** Response lenses
    loersOriginEndpoints,
    loersNextToken,
    loersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { -- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
    channelId :: Lude.Maybe Lude.Text,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOriginEndpoints' with the minimum fields required to make a request.
--
-- * 'channelId' - When specified, the request will return only OriginEndpoints associated with the given Channel ID.
-- * 'nextToken' - A token used to resume pagination from the end of a previous request.
-- * 'maxResults' - The upper bound on the number of records to return.
mkListOriginEndpoints ::
  ListOriginEndpoints
mkListOriginEndpoints =
  ListOriginEndpoints'
    { channelId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeChannelId :: Lens.Lens' ListOriginEndpoints (Lude.Maybe Lude.Text)
loeChannelId = Lens.lens (channelId :: ListOriginEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: ListOriginEndpoints)
{-# DEPRECATED loeChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeNextToken :: Lens.Lens' ListOriginEndpoints (Lude.Maybe Lude.Text)
loeNextToken = Lens.lens (nextToken :: ListOriginEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOriginEndpoints)
{-# DEPRECATED loeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The upper bound on the number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeMaxResults :: Lens.Lens' ListOriginEndpoints (Lude.Maybe Lude.Natural)
loeMaxResults = Lens.lens (maxResults :: ListOriginEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOriginEndpoints)
{-# DEPRECATED loeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOriginEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. loersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loersOriginEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loeNextToken Lens..~ rs Lens.^. loersNextToken

instance Lude.AWSRequest ListOriginEndpoints where
  type Rs ListOriginEndpoints = ListOriginEndpointsResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOriginEndpointsResponse'
            Lude.<$> (x Lude..?> "originEndpoints" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOriginEndpoints where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListOriginEndpoints where
  toPath = Lude.const "/origin_endpoints"

instance Lude.ToQuery ListOriginEndpoints where
  toQuery ListOriginEndpoints' {..} =
    Lude.mconcat
      [ "channelId" Lude.=: channelId,
        "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { -- | A list of OriginEndpoint records.
    originEndpoints :: Lude.Maybe [OriginEndpoint],
    -- | A token that can be used to resume pagination from the end of the collection.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOriginEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'originEndpoints' - A list of OriginEndpoint records.
-- * 'nextToken' - A token that can be used to resume pagination from the end of the collection.
-- * 'responseStatus' - The response status code.
mkListOriginEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOriginEndpointsResponse
mkListOriginEndpointsResponse pResponseStatus_ =
  ListOriginEndpointsResponse'
    { originEndpoints = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of OriginEndpoint records.
--
-- /Note:/ Consider using 'originEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loersOriginEndpoints :: Lens.Lens' ListOriginEndpointsResponse (Lude.Maybe [OriginEndpoint])
loersOriginEndpoints = Lens.lens (originEndpoints :: ListOriginEndpointsResponse -> Lude.Maybe [OriginEndpoint]) (\s a -> s {originEndpoints = a} :: ListOriginEndpointsResponse)
{-# DEPRECATED loersOriginEndpoints "Use generic-lens or generic-optics with 'originEndpoints' instead." #-}

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loersNextToken :: Lens.Lens' ListOriginEndpointsResponse (Lude.Maybe Lude.Text)
loersNextToken = Lens.lens (nextToken :: ListOriginEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOriginEndpointsResponse)
{-# DEPRECATED loersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loersResponseStatus :: Lens.Lens' ListOriginEndpointsResponse Lude.Int
loersResponseStatus = Lens.lens (responseStatus :: ListOriginEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOriginEndpointsResponse)
{-# DEPRECATED loersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
