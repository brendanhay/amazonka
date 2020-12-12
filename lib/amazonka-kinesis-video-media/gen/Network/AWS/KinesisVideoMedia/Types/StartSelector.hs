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
    ssContinuationToken,
    ssAfterFragmentNumber,
    ssStartTimestamp,
    ssStartSelectorType,
  )
where

import Network.AWS.KinesisVideoMedia.Types.StartSelectorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { continuationToken ::
      Lude.Maybe Lude.Text,
    afterFragmentNumber :: Lude.Maybe Lude.Text,
    startTimestamp :: Lude.Maybe Lude.Timestamp,
    startSelectorType :: StartSelectorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartSelector' with the minimum fields required to make a request.
--
-- * 'afterFragmentNumber' - Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
-- * 'continuationToken' - Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
-- * 'startSelectorType' - Identifies the fragment on the Kinesis video stream where you want to start getting the data from.
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
-- * 'startTimestamp' - A timestamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified timestamp.
mkStartSelector ::
  -- | 'startSelectorType'
  StartSelectorType ->
  StartSelector
mkStartSelector pStartSelectorType_ =
  StartSelector'
    { continuationToken = Lude.Nothing,
      afterFragmentNumber = Lude.Nothing,
      startTimestamp = Lude.Nothing,
      startSelectorType = pStartSelectorType_
    }

-- | Continuation token that Kinesis Video Streams returned in the previous @GetMedia@ response. The @GetMedia@ API then starts with the chunk identified by the continuation token.
--
-- /Note:/ Consider using 'continuationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssContinuationToken :: Lens.Lens' StartSelector (Lude.Maybe Lude.Text)
ssContinuationToken = Lens.lens (continuationToken :: StartSelector -> Lude.Maybe Lude.Text) (\s a -> s {continuationToken = a} :: StartSelector)
{-# DEPRECATED ssContinuationToken "Use generic-lens or generic-optics with 'continuationToken' instead." #-}

-- | Specifies the fragment number from where you want the @GetMedia@ API to start returning the fragments.
--
-- /Note:/ Consider using 'afterFragmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAfterFragmentNumber :: Lens.Lens' StartSelector (Lude.Maybe Lude.Text)
ssAfterFragmentNumber = Lens.lens (afterFragmentNumber :: StartSelector -> Lude.Maybe Lude.Text) (\s a -> s {afterFragmentNumber = a} :: StartSelector)
{-# DEPRECATED ssAfterFragmentNumber "Use generic-lens or generic-optics with 'afterFragmentNumber' instead." #-}

-- | A timestamp value. This value is required if you choose the PRODUCER_TIMESTAMP or the SERVER_TIMESTAMP as the @startSelectorType@ . The @GetMedia@ API then starts with the chunk containing the fragment that has the specified timestamp.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStartTimestamp :: Lens.Lens' StartSelector (Lude.Maybe Lude.Timestamp)
ssStartTimestamp = Lens.lens (startTimestamp :: StartSelector -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTimestamp = a} :: StartSelector)
{-# DEPRECATED ssStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

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
ssStartSelectorType :: Lens.Lens' StartSelector StartSelectorType
ssStartSelectorType = Lens.lens (startSelectorType :: StartSelector -> StartSelectorType) (\s a -> s {startSelectorType = a} :: StartSelector)
{-# DEPRECATED ssStartSelectorType "Use generic-lens or generic-optics with 'startSelectorType' instead." #-}

instance Lude.ToJSON StartSelector where
  toJSON StartSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ContinuationToken" Lude..=) Lude.<$> continuationToken,
            ("AfterFragmentNumber" Lude..=) Lude.<$> afterFragmentNumber,
            ("StartTimestamp" Lude..=) Lude.<$> startTimestamp,
            Lude.Just ("StartSelectorType" Lude..= startSelectorType)
          ]
      )
