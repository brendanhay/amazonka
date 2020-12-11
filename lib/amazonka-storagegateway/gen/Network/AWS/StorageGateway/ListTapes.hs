{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListTapes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists virtual tapes in your virtual tape library (VTL) and your virtual tape shelf (VTS). You specify the tapes to list by specifying one or more tape Amazon Resource Names (ARNs). If you don't specify a tape ARN, the operation lists all virtual tapes in both your VTL and VTS.
--
-- This operation supports pagination. By default, the operation returns a maximum of up to 100 tapes. You can optionally specify the @Limit@ parameter in the body to limit the number of tapes in the response. If the number of tapes returned in the response is truncated, the response includes a @Marker@ element that you can use in your subsequent request to retrieve the next set of tapes. This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTapes
  ( -- * Creating a request
    ListTapes (..),
    mkListTapes,

    -- ** Request lenses
    ltMarker,
    ltLimit,
    ltTapeARNs,

    -- * Destructuring the response
    ListTapesResponse (..),
    mkListTapesResponse,

    -- ** Response lenses
    ltrsMarker,
    ltrsTapeInfos,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
--
--     * 'ListTapesInput$Limit'
--
--
--     * 'ListTapesInput$Marker'
--
--
--     * 'ListTapesInput$TapeARNs'
--
--
--
-- /See:/ 'mkListTapes' smart constructor.
data ListTapes = ListTapes'
  { marker :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    tapeARNs :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTapes' with the minimum fields required to make a request.
--
-- * 'limit' - An optional number limit for the tapes in the list returned by this call.
-- * 'marker' - A string that indicates the position at which to begin the returned list of tapes.
-- * 'tapeARNs' - Undocumented field.
mkListTapes ::
  ListTapes
mkListTapes =
  ListTapes'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      tapeARNs = Lude.Nothing
    }

-- | A string that indicates the position at which to begin the returned list of tapes.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMarker :: Lens.Lens' ListTapes (Lude.Maybe Lude.Text)
ltMarker = Lens.lens (marker :: ListTapes -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTapes)
{-# DEPRECATED ltMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An optional number limit for the tapes in the list returned by this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLimit :: Lens.Lens' ListTapes (Lude.Maybe Lude.Natural)
ltLimit = Lens.lens (limit :: ListTapes -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTapes)
{-# DEPRECATED ltLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTapeARNs :: Lens.Lens' ListTapes (Lude.Maybe [Lude.Text])
ltTapeARNs = Lens.lens (tapeARNs :: ListTapes -> Lude.Maybe [Lude.Text]) (\s a -> s {tapeARNs = a} :: ListTapes)
{-# DEPRECATED ltTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

instance Page.AWSPager ListTapes where
  page rq rs
    | Page.stop (rs Lens.^. ltrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTapeInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ltMarker Lens..~ rs Lens.^. ltrsMarker

instance Lude.AWSRequest ListTapes where
  type Rs ListTapes = ListTapesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTapesResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "TapeInfos" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTapes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListTapes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTapes where
  toJSON ListTapes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            ("TapeARNs" Lude..=) Lude.<$> tapeARNs
          ]
      )

instance Lude.ToPath ListTapes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTapes where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
--
--     * 'ListTapesOutput$Marker'
--
--
--     * 'ListTapesOutput$VolumeInfos'
--
--
--
-- /See:/ 'mkListTapesResponse' smart constructor.
data ListTapesResponse = ListTapesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    tapeInfos :: Lude.Maybe [TapeInfo],
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

-- | Creates a value of 'ListTapesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
-- * 'responseStatus' - The response status code.
-- * 'tapeInfos' - Undocumented field.
mkListTapesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTapesResponse
mkListTapesResponse pResponseStatus_ =
  ListTapesResponse'
    { marker = Lude.Nothing,
      tapeInfos = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates the position at which to begin returning the next list of tapes. Use the marker in your next request to continue pagination of tapes. If there are no more tapes to list, this element does not appear in the response body.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsMarker :: Lens.Lens' ListTapesResponse (Lude.Maybe Lude.Text)
ltrsMarker = Lens.lens (marker :: ListTapesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTapesResponse)
{-# DEPRECATED ltrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tapeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTapeInfos :: Lens.Lens' ListTapesResponse (Lude.Maybe [TapeInfo])
ltrsTapeInfos = Lens.lens (tapeInfos :: ListTapesResponse -> Lude.Maybe [TapeInfo]) (\s a -> s {tapeInfos = a} :: ListTapesResponse)
{-# DEPRECATED ltrsTapeInfos "Use generic-lens or generic-optics with 'tapeInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTapesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTapesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTapesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
