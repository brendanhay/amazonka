{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of Channels.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListChannels
  ( -- * Creating a request
    ListChannels (..),
    mkListChannels,

    -- ** Request lenses
    lcNextToken,
    lcMaxResults,

    -- * Destructuring the response
    ListChannelsResponse (..),
    mkListChannelsResponse,

    -- ** Response lenses
    lcrsChannels,
    lcrsNextToken,
    lcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListChannels' smart constructor.
data ListChannels = ListChannels'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListChannels' with the minimum fields required to make a request.
--
-- * 'maxResults' - Upper bound on number of records to return.
-- * 'nextToken' - A token used to resume pagination from the end of a previous request.
mkListChannels ::
  ListChannels
mkListChannels =
  ListChannels'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListChannels (Lude.Maybe Lude.Text)
lcNextToken = Lens.lens (nextToken :: ListChannels -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChannels)
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Upper bound on number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListChannels (Lude.Maybe Lude.Natural)
lcMaxResults = Lens.lens (maxResults :: ListChannels -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListChannels)
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListChannels where
  page rq rs
    | Page.stop (rs Lens.^. lcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcrsChannels) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcNextToken Lens..~ rs Lens.^. lcrsNextToken

instance Lude.AWSRequest ListChannels where
  type Rs ListChannels = ListChannelsResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListChannelsResponse'
            Lude.<$> (x Lude..?> "channels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListChannels where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListChannels where
  toPath = Lude.const "/channels"

instance Lude.ToQuery ListChannels where
  toQuery ListChannels' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListChannelsResponse' smart constructor.
data ListChannelsResponse = ListChannelsResponse'
  { channels ::
      Lude.Maybe [Channel],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListChannelsResponse' with the minimum fields required to make a request.
--
-- * 'channels' - A list of Channel records.
-- * 'nextToken' - A token that can be used to resume pagination from the end of the collection.
-- * 'responseStatus' - The response status code.
mkListChannelsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListChannelsResponse
mkListChannelsResponse pResponseStatus_ =
  ListChannelsResponse'
    { channels = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of Channel records.
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsChannels :: Lens.Lens' ListChannelsResponse (Lude.Maybe [Channel])
lcrsChannels = Lens.lens (channels :: ListChannelsResponse -> Lude.Maybe [Channel]) (\s a -> s {channels = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsNextToken :: Lens.Lens' ListChannelsResponse (Lude.Maybe Lude.Text)
lcrsNextToken = Lens.lens (nextToken :: ListChannelsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrsResponseStatus :: Lens.Lens' ListChannelsResponse Lude.Int
lcrsResponseStatus = Lens.lens (responseStatus :: ListChannelsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListChannelsResponse)
{-# DEPRECATED lcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
