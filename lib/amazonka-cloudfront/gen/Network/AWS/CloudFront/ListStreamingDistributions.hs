{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List streaming distributions.
--
-- This operation returns paginated results.
module Network.AWS.CloudFront.ListStreamingDistributions
  ( -- * Creating a request
    ListStreamingDistributions (..),
    mkListStreamingDistributions,

    -- ** Request lenses
    lsdMarker,
    lsdMaxItems,

    -- * Destructuring the response
    ListStreamingDistributionsResponse (..),
    mkListStreamingDistributionsResponse,

    -- ** Response lenses
    lsdrsStreamingDistributionList,
    lsdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list your streaming distributions.
--
-- /See:/ 'mkListStreamingDistributions' smart constructor.
data ListStreamingDistributions = ListStreamingDistributions'
  { -- | The value that you provided for the @Marker@ request parameter.
    marker :: Lude.Maybe Lude.Text,
    -- | The value that you provided for the @MaxItems@ request parameter.
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamingDistributions' with the minimum fields required to make a request.
--
-- * 'marker' - The value that you provided for the @Marker@ request parameter.
-- * 'maxItems' - The value that you provided for the @MaxItems@ request parameter.
mkListStreamingDistributions ::
  ListStreamingDistributions
mkListStreamingDistributions =
  ListStreamingDistributions'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The value that you provided for the @Marker@ request parameter.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMarker :: Lens.Lens' ListStreamingDistributions (Lude.Maybe Lude.Text)
lsdMarker = Lens.lens (marker :: ListStreamingDistributions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListStreamingDistributions)
{-# DEPRECATED lsdMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The value that you provided for the @MaxItems@ request parameter.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMaxItems :: Lens.Lens' ListStreamingDistributions (Lude.Maybe Lude.Text)
lsdMaxItems = Lens.lens (maxItems :: ListStreamingDistributions -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListStreamingDistributions)
{-# DEPRECATED lsdMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListStreamingDistributions where
  page rq rs
    | Page.stop
        (rs Lens.^. lsdrsStreamingDistributionList Lude.. sdlIsTruncated) =
      Lude.Nothing
    | Lude.isNothing
        ( rs
            Lens.^? lsdrsStreamingDistributionList
              Lude.. sdlNextMarker
              Lude.. Lens._Just
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsdMarker
          Lens..~ rs
          Lens.^? lsdrsStreamingDistributionList
            Lude.. sdlNextMarker
            Lude.. Lens._Just

instance Lude.AWSRequest ListStreamingDistributions where
  type
    Rs ListStreamingDistributions =
      ListStreamingDistributionsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListStreamingDistributionsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStreamingDistributions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListStreamingDistributions where
  toPath = Lude.const "/2020-05-31/streaming-distribution"

instance Lude.ToQuery ListStreamingDistributions where
  toQuery ListStreamingDistributions' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListStreamingDistributionsResponse' smart constructor.
data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse'
  { -- | The @StreamingDistributionList@ type.
    streamingDistributionList :: StreamingDistributionList,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStreamingDistributionsResponse' with the minimum fields required to make a request.
--
-- * 'streamingDistributionList' - The @StreamingDistributionList@ type.
-- * 'responseStatus' - The response status code.
mkListStreamingDistributionsResponse ::
  -- | 'streamingDistributionList'
  StreamingDistributionList ->
  -- | 'responseStatus'
  Lude.Int ->
  ListStreamingDistributionsResponse
mkListStreamingDistributionsResponse
  pStreamingDistributionList_
  pResponseStatus_ =
    ListStreamingDistributionsResponse'
      { streamingDistributionList =
          pStreamingDistributionList_,
        responseStatus = pResponseStatus_
      }

-- | The @StreamingDistributionList@ type.
--
-- /Note:/ Consider using 'streamingDistributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrsStreamingDistributionList :: Lens.Lens' ListStreamingDistributionsResponse StreamingDistributionList
lsdrsStreamingDistributionList = Lens.lens (streamingDistributionList :: ListStreamingDistributionsResponse -> StreamingDistributionList) (\s a -> s {streamingDistributionList = a} :: ListStreamingDistributionsResponse)
{-# DEPRECATED lsdrsStreamingDistributionList "Use generic-lens or generic-optics with 'streamingDistributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrsResponseStatus :: Lens.Lens' ListStreamingDistributionsResponse Lude.Int
lsdrsResponseStatus = Lens.lens (responseStatus :: ListStreamingDistributionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStreamingDistributionsResponse)
{-# DEPRECATED lsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
