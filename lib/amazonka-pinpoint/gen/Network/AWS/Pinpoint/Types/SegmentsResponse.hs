{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentsResponse
  ( SegmentsResponse (..),

    -- * Smart constructor
    mkSegmentsResponse,

    -- * Lenses
    sNextToken,
    sItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the segments that are associated with an application.
--
-- /See:/ 'mkSegmentsResponse' smart constructor.
data SegmentsResponse = SegmentsResponse'
  { -- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
    item :: [SegmentResponse]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
-- * 'item' - An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
mkSegmentsResponse ::
  SegmentsResponse
mkSegmentsResponse =
  SegmentsResponse' {nextToken = Lude.Nothing, item = Lude.mempty}

-- | The string to use in a subsequent request to get the next page of results in a paginated response. This value is null if there are no additional pages.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sNextToken :: Lens.Lens' SegmentsResponse (Lude.Maybe Lude.Text)
sNextToken = Lens.lens (nextToken :: SegmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SegmentsResponse)
{-# DEPRECATED sNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of responses, one for each segment that's associated with the application (Segments resource) or each version of a segment that's associated with the application (Segment Versions resource).
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sItem :: Lens.Lens' SegmentsResponse [SegmentResponse]
sItem = Lens.lens (item :: SegmentsResponse -> [SegmentResponse]) (\s a -> s {item = a} :: SegmentsResponse)
{-# DEPRECATED sItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON SegmentsResponse where
  parseJSON =
    Lude.withObject
      "SegmentsResponse"
      ( \x ->
          SegmentsResponse'
            Lude.<$> (x Lude..:? "NextToken")
            Lude.<*> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
