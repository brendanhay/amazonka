{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of specified virtual tapes in the virtual tape shelf (VTS). This operation is only supported in the tape gateway type.
--
-- If a specific @TapeARN@ is not specified, AWS Storage Gateway returns a description of all virtual tapes found in the VTS associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeTapeArchives
  ( -- * Creating a request
    DescribeTapeArchives (..),
    mkDescribeTapeArchives,

    -- ** Request lenses
    dtaMarker,
    dtaLimit,
    dtaTapeARNs,

    -- * Destructuring the response
    DescribeTapeArchivesResponse (..),
    mkDescribeTapeArchivesResponse,

    -- ** Response lenses
    dtarsTapeArchives,
    dtarsMarker,
    dtarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DescribeTapeArchivesInput
--
-- /See:/ 'mkDescribeTapeArchives' smart constructor.
data DescribeTapeArchives = DescribeTapeArchives'
  { marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeTapeArchives' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies that the number of virtual tapes described be limited to the specified number.
-- * 'marker' - An opaque string that indicates the position at which to begin describing virtual tapes.
-- * 'tapeARNs' - Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
mkDescribeTapeArchives ::
  DescribeTapeArchives
mkDescribeTapeArchives =
  DescribeTapeArchives'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      tapeARNs = Lude.Nothing
    }

-- | An opaque string that indicates the position at which to begin describing virtual tapes.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaMarker :: Lens.Lens' DescribeTapeArchives (Lude.Maybe Lude.Text)
dtaMarker = Lens.lens (marker :: DescribeTapeArchives -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTapeArchives)
{-# DEPRECATED dtaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the number of virtual tapes described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaLimit :: Lens.Lens' DescribeTapeArchives (Lude.Maybe Lude.Natural)
dtaLimit = Lens.lens (limit :: DescribeTapeArchives -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeTapeArchives)
{-# DEPRECATED dtaLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Specifies one or more unique Amazon Resource Names (ARNs) that represent the virtual tapes you want to describe.
--
-- /Note:/ Consider using 'tapeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtaTapeARNs :: Lens.Lens' DescribeTapeArchives (Lude.Maybe [Lude.Text])
dtaTapeARNs = Lens.lens (tapeARNs :: DescribeTapeArchives -> Lude.Maybe [Lude.Text]) (\s a -> s {tapeARNs = a} :: DescribeTapeArchives)
{-# DEPRECATED dtaTapeARNs "Use generic-lens or generic-optics with 'tapeARNs' instead." #-}

instance Page.AWSPager DescribeTapeArchives where
  page rq rs
    | Page.stop (rs Lens.^. dtarsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dtarsTapeArchives) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& dtaMarker Lens..~ rs Lens.^. dtarsMarker

instance Lude.AWSRequest DescribeTapeArchives where
  type Rs DescribeTapeArchives = DescribeTapeArchivesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTapeArchivesResponse'
            Lude.<$> (x Lude..?> "TapeArchives" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTapeArchives where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeTapeArchives" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTapeArchives where
  toJSON DescribeTapeArchives' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            ("TapeARNs" Lude..=) Lude.<$> tapeARNs
          ]
      )

instance Lude.ToPath DescribeTapeArchives where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTapeArchives where
  toQuery = Lude.const Lude.mempty

-- | DescribeTapeArchivesOutput
--
-- /See:/ 'mkDescribeTapeArchivesResponse' smart constructor.
data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse'
  { tapeArchives ::
      Lude.Maybe [TapeArchive],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeTapeArchivesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
-- * 'responseStatus' - The response status code.
-- * 'tapeArchives' - An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name (ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description, and tape barcode.
mkDescribeTapeArchivesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTapeArchivesResponse
mkDescribeTapeArchivesResponse pResponseStatus_ =
  DescribeTapeArchivesResponse'
    { tapeArchives = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of virtual tape objects in the virtual tape shelf (VTS). The description includes of the Amazon Resource Name (ARN) of the virtual tapes. The information returned includes the Amazon Resource Names (ARNs) of the tapes, size of the tapes, status of the tapes, progress of the description, and tape barcode.
--
-- /Note:/ Consider using 'tapeArchives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarsTapeArchives :: Lens.Lens' DescribeTapeArchivesResponse (Lude.Maybe [TapeArchive])
dtarsTapeArchives = Lens.lens (tapeArchives :: DescribeTapeArchivesResponse -> Lude.Maybe [TapeArchive]) (\s a -> s {tapeArchives = a} :: DescribeTapeArchivesResponse)
{-# DEPRECATED dtarsTapeArchives "Use generic-lens or generic-optics with 'tapeArchives' instead." #-}

-- | An opaque string that indicates the position at which the virtual tapes that were fetched for description ended. Use this marker in your next request to fetch the next set of virtual tapes in the virtual tape shelf (VTS). If there are no more virtual tapes to describe, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarsMarker :: Lens.Lens' DescribeTapeArchivesResponse (Lude.Maybe Lude.Text)
dtarsMarker = Lens.lens (marker :: DescribeTapeArchivesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeTapeArchivesResponse)
{-# DEPRECATED dtarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtarsResponseStatus :: Lens.Lens' DescribeTapeArchivesResponse Lude.Int
dtarsResponseStatus = Lens.lens (responseStatus :: DescribeTapeArchivesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTapeArchivesResponse)
{-# DEPRECATED dtarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
