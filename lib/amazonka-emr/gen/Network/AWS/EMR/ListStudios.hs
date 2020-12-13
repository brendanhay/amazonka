{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListStudios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all Amazon EMR Studios associated with the AWS account. The list includes details such as ID, Studio Access URL, and creation time for each Studio.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudios
  ( -- * Creating a request
    ListStudios (..),
    mkListStudios,

    -- ** Request lenses
    lsMarker,

    -- * Destructuring the response
    ListStudiosResponse (..),
    mkListStudiosResponse,

    -- ** Response lenses
    lsrsStudios,
    lsrsMarker,
    lsrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStudios' smart constructor.
newtype ListStudios = ListStudios'
  { -- | The pagination token that indicates the set of results to retrieve.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStudios' with the minimum fields required to make a request.
--
-- * 'marker' - The pagination token that indicates the set of results to retrieve.
mkListStudios ::
  ListStudios
mkListStudios = ListStudios' {marker = Lude.Nothing}

-- | The pagination token that indicates the set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMarker :: Lens.Lens' ListStudios (Lude.Maybe Lude.Text)
lsMarker = Lens.lens (marker :: ListStudios -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListStudios)
{-# DEPRECATED lsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListStudios where
  page rq rs
    | Page.stop (rs Lens.^. lsrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lsrsStudios) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lsMarker Lens..~ rs Lens.^. lsrsMarker

instance Lude.AWSRequest ListStudios where
  type Rs ListStudios = ListStudiosResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Lude.<$> (x Lude..?> "Studios" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStudios where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListStudios" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStudios where
  toJSON ListStudios' {..} =
    Lude.object (Lude.catMaybes [("Marker" Lude..=) Lude.<$> marker])

instance Lude.ToPath ListStudios where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStudios where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | The list of Studio summary objects.
    studios :: Lude.Maybe [StudioSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStudiosResponse' with the minimum fields required to make a request.
--
-- * 'studios' - The list of Studio summary objects.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkListStudiosResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStudiosResponse
mkListStudiosResponse pResponseStatus_ =
  ListStudiosResponse'
    { studios = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of Studio summary objects.
--
-- /Note:/ Consider using 'studios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsStudios :: Lens.Lens' ListStudiosResponse (Lude.Maybe [StudioSummary])
lsrsStudios = Lens.lens (studios :: ListStudiosResponse -> Lude.Maybe [StudioSummary]) (\s a -> s {studios = a} :: ListStudiosResponse)
{-# DEPRECATED lsrsStudios "Use generic-lens or generic-optics with 'studios' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsMarker :: Lens.Lens' ListStudiosResponse (Lude.Maybe Lude.Text)
lsrsMarker = Lens.lens (marker :: ListStudiosResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListStudiosResponse)
{-# DEPRECATED lsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrsResponseStatus :: Lens.Lens' ListStudiosResponse Lude.Int
lsrsResponseStatus = Lens.lens (responseStatus :: ListStudiosResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStudiosResponse)
{-# DEPRECATED lsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
