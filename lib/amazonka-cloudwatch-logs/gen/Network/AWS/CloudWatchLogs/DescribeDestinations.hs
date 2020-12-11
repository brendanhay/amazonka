{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your destinations. The results are ASCII-sorted by destination name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeDestinations
  ( -- * Creating a request
    DescribeDestinations (..),
    mkDescribeDestinations,

    -- ** Request lenses
    ddNextToken,
    ddLimit,
    ddDestinationNamePrefix,

    -- * Destructuring the response
    DescribeDestinationsResponse (..),
    mkDescribeDestinationsResponse,

    -- ** Response lenses
    ddrsNextToken,
    ddrsDestinations,
    ddrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
  { nextToken ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    destinationNamePrefix :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDestinations' with the minimum fields required to make a request.
--
-- * 'destinationNamePrefix' - The prefix to match. If you don't specify a value, no prefix filter is applied.
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeDestinations ::
  DescribeDestinations
mkDescribeDestinations =
  DescribeDestinations'
    { nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      destinationNamePrefix = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddNextToken :: Lens.Lens' DescribeDestinations (Lude.Maybe Lude.Text)
ddNextToken = Lens.lens (nextToken :: DescribeDestinations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDestinations)
{-# DEPRECATED ddNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLimit :: Lens.Lens' DescribeDestinations (Lude.Maybe Lude.Natural)
ddLimit = Lens.lens (limit :: DescribeDestinations -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDestinations)
{-# DEPRECATED ddLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The prefix to match. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'destinationNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDestinationNamePrefix :: Lens.Lens' DescribeDestinations (Lude.Maybe Lude.Text)
ddDestinationNamePrefix = Lens.lens (destinationNamePrefix :: DescribeDestinations -> Lude.Maybe Lude.Text) (\s a -> s {destinationNamePrefix = a} :: DescribeDestinations)
{-# DEPRECATED ddDestinationNamePrefix "Use generic-lens or generic-optics with 'destinationNamePrefix' instead." #-}

instance Page.AWSPager DescribeDestinations where
  page rq rs
    | Page.stop (rs Lens.^. ddrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddrsDestinations) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddNextToken Lens..~ rs Lens.^. ddrsNextToken

instance Lude.AWSRequest DescribeDestinations where
  type Rs DescribeDestinations = DescribeDestinationsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDestinationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "destinations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDestinations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeDestinations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDestinations where
  toJSON DescribeDestinations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit,
            ("DestinationNamePrefix" Lude..=) Lude.<$> destinationNamePrefix
          ]
      )

instance Lude.ToPath DescribeDestinations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDestinations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    destinations ::
      Lude.Maybe [Destination],
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

-- | Creates a value of 'DescribeDestinationsResponse' with the minimum fields required to make a request.
--
-- * 'destinations' - The destinations.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeDestinationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDestinationsResponse
mkDescribeDestinationsResponse pResponseStatus_ =
  DescribeDestinationsResponse'
    { nextToken = Lude.Nothing,
      destinations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsNextToken :: Lens.Lens' DescribeDestinationsResponse (Lude.Maybe Lude.Text)
ddrsNextToken = Lens.lens (nextToken :: DescribeDestinationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDestinationsResponse)
{-# DEPRECATED ddrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDestinations :: Lens.Lens' DescribeDestinationsResponse (Lude.Maybe [Destination])
ddrsDestinations = Lens.lens (destinations :: DescribeDestinationsResponse -> Lude.Maybe [Destination]) (\s a -> s {destinations = a} :: DescribeDestinationsResponse)
{-# DEPRECATED ddrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DescribeDestinationsResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DescribeDestinationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDestinationsResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
