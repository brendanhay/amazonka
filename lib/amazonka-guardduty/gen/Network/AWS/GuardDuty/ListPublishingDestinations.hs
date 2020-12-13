{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListPublishingDestinations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of publishing destinations associated with the specified @dectectorId@ .
module Network.AWS.GuardDuty.ListPublishingDestinations
  ( -- * Creating a request
    ListPublishingDestinations (..),
    mkListPublishingDestinations,

    -- ** Request lenses
    lpdNextToken,
    lpdDetectorId,
    lpdMaxResults,

    -- * Destructuring the response
    ListPublishingDestinationsResponse (..),
    mkListPublishingDestinationsResponse,

    -- ** Response lenses
    lpdrsNextToken,
    lpdrsDestinations,
    lpdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPublishingDestinations' smart constructor.
data ListPublishingDestinations = ListPublishingDestinations'
  { -- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ID of the detector to retrieve publishing destinations for.
    detectorId :: Lude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublishingDestinations' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
-- * 'detectorId' - The ID of the detector to retrieve publishing destinations for.
-- * 'maxResults' - The maximum number of results to return in the response.
mkListPublishingDestinations ::
  -- | 'detectorId'
  Lude.Text ->
  ListPublishingDestinations
mkListPublishingDestinations pDetectorId_ =
  ListPublishingDestinations'
    { nextToken = Lude.Nothing,
      detectorId = pDetectorId_,
      maxResults = Lude.Nothing
    }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdNextToken :: Lens.Lens' ListPublishingDestinations (Lude.Maybe Lude.Text)
lpdNextToken = Lens.lens (nextToken :: ListPublishingDestinations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublishingDestinations)
{-# DEPRECATED lpdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the detector to retrieve publishing destinations for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdDetectorId :: Lens.Lens' ListPublishingDestinations Lude.Text
lpdDetectorId = Lens.lens (detectorId :: ListPublishingDestinations -> Lude.Text) (\s a -> s {detectorId = a} :: ListPublishingDestinations)
{-# DEPRECATED lpdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdMaxResults :: Lens.Lens' ListPublishingDestinations (Lude.Maybe Lude.Natural)
lpdMaxResults = Lens.lens (maxResults :: ListPublishingDestinations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPublishingDestinations)
{-# DEPRECATED lpdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListPublishingDestinations where
  type
    Rs ListPublishingDestinations =
      ListPublishingDestinationsResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPublishingDestinationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "destinations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPublishingDestinations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListPublishingDestinations where
  toPath ListPublishingDestinations' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/publishingDestination"]

instance Lude.ToQuery ListPublishingDestinations where
  toQuery ListPublishingDestinations' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListPublishingDestinationsResponse' smart constructor.
data ListPublishingDestinationsResponse = ListPublishingDestinationsResponse'
  { -- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A @Destinations@ object that includes information about each publishing destination returned.
    destinations :: [Destination],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublishingDestinationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
-- * 'destinations' - A @Destinations@ object that includes information about each publishing destination returned.
-- * 'responseStatus' - The response status code.
mkListPublishingDestinationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPublishingDestinationsResponse
mkListPublishingDestinationsResponse pResponseStatus_ =
  ListPublishingDestinationsResponse'
    { nextToken = Lude.Nothing,
      destinations = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the @NextToken@ value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsNextToken :: Lens.Lens' ListPublishingDestinationsResponse (Lude.Maybe Lude.Text)
lpdrsNextToken = Lens.lens (nextToken :: ListPublishingDestinationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublishingDestinationsResponse)
{-# DEPRECATED lpdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @Destinations@ object that includes information about each publishing destination returned.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsDestinations :: Lens.Lens' ListPublishingDestinationsResponse [Destination]
lpdrsDestinations = Lens.lens (destinations :: ListPublishingDestinationsResponse -> [Destination]) (\s a -> s {destinations = a} :: ListPublishingDestinationsResponse)
{-# DEPRECATED lpdrsDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdrsResponseStatus :: Lens.Lens' ListPublishingDestinationsResponse Lude.Int
lpdrsResponseStatus = Lens.lens (responseStatus :: ListPublishingDestinationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPublishingDestinationsResponse)
{-# DEPRECATED lpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
