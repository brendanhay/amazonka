{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types.StartSelector
  ( StartSelector (..),

    -- * Smart constructor
    mkStartSelector,

    -- * Lenses
    ssStartSelectorType,
    ssAfterFragmentNumber,
    ssContinuationToken,
    ssStartTimestamp,
  )
where

import qualified Network.AWS.KinesisVideoMedia.Types.AfterFragmentNumber as Types
import qualified Network.AWS.KinesisVideoMedia.Types.ContinuationToken as Types
import qualified Network.AWS.KinesisVideoMedia.Types.StartSelectorType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies the chunk on the Kinesis video stream where you want the @GetMedia@ API to start returning media data. You have the following options to identify the starting chunk:
--
--
--     * Choose the latest (or oldest) chunk.
--
--
--     * Identify a specific chunk. You can identify a specific chunk either by providing a fragment number or timestamp (server or producer).
--
--
--     * Each chunk's metadata includes a continuation token as a Matroska (MKV) tag (@AWS_KINESISVIDEO_CONTINUATION_TOKEN@ ). If your previous @GetMedia@ request terminated, you can use this tag value in your next @GetMedia@ request. The API then starts returning chunks starting where the last API ended.
--
--
--
-- /See:/ 'mkStartSelector' smart constructor.
data StartSelector = StartSelector'
  { -- | Identifies the fragment on the Kinesis video stream where you want to start getting the data from.
    --
    --
    --     * NOW - Start with the latest chunk on the stream.
    --
    --
    --     * EARLIEST - Start with earliest available chunk on the stream.
    --
    --
    --     * FRAGMENT_NUMBER - Start with the chunk after a specific fragment. You must also specify the @AfterFragmentNumber@ parameter.
    --
    --
    --     * PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk containing a fragment with the specified producer or server timestamp. You specify the timestamp by adding @StartTimestamp@ .
    --
    --
    --     * CONTINUATION_TOKEN - Read using the specified continuation token.
    startSelectorType :: Types.StartSelectorType,
    -- | Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
    afterFragmentNumber :: Core.Maybe Types.AfterFragmentNumber,
    -- | Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
    continuationToken :: Core.Maybe Types.ContinuationToken,
    -- | A timestamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified timestamp.
    startTimestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartSelector' value with any optional fields omitted.
mkStartSelector ::
  -- | 'startSelectorType'
  Types.StartSelectorType ->
  StartSelector
mkStartSelector startSelectorType =
  StartSelector'
    { startSelectorType,
      afterFragmentNumber = Core.Nothing,
      continuationToken = Core.Nothing,
      startTimestamp = Core.Nothing
    }

-- | Identifies the fragment on the Kinesis video stream where you want to start getting the data from.
--
--
--     * NOW - Start with the latest chunk on the stream.
--
--
--     * EARLIEST - Start with earliest available chunk on the stream.
--
--
--     * FRAGMENT_NUMBER - Start with the chunk after a specific fragment. You must also specify the @AfterFragmentNumber@ parameter.
--
--
--     * PRODUCER_TIMESTAMP or SERVER_TIMESTAMP - Start with the chunk containing a fragment with the specified producer or server timestamp. You specify the timestamp by adding @StartTimestamp@ .
--
--
--     * CONTINUATION_TOKEN - Read using the specified continuation token.
--
--
--
-- /Note:/ Consider using 'startSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStartSelectorType :: Lens.Lens' StartSelector Types.StartSelectorType
ssStartSelectorType = Lens.field @"startSelectorType"
{-# DEPRECATED ssStartSelectorType "Use generic-lens or generic-optics with 'startSelectorType' instead." #-}

-- | Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
--
-- /Note:/ Consider using 'afterFragmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAfterFragmentNumber :: Lens.Lens' StartSelector (Core.Maybe Types.AfterFragmentNumber)
ssAfterFragmentNumber = Lens.field @"afterFragmentNumber"
{-# DEPRECATED ssAfterFragmentNumber "Use generic-lens or generic-optics with 'afterFragmentNumber' instead." #-}

-- | Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssContinuationToken :: Lens.Lens' StartSelector (Core.Maybe Types.ContinuationToken)
ssContinuationToken = Lens.field @"continuationToken"
{-# DEPRECATED ssContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | A timestamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified timestamp.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStartTimestamp :: Lens.Lens' StartSelector (Core.Maybe Core.NominalDiffTime)
ssStartTimestamp = Lens.field @"startTimestamp"
{-# DEPRECATED ssStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

instance Core.FromJSON StartSelector where
  toJSON StartSelector {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartSelectorType" Core..= startSelectorType),
            ("AfterFragmentNumber" Core..=) Core.<$> afterFragmentNumber,
            ("ContinuationToken" Core..=) Core.<$> continuationToken,
            ("StartTimestamp" Core..=) Core.<$> startTimestamp
          ]
      )
