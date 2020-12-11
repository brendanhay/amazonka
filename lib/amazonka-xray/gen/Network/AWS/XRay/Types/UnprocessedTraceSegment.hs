-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedTraceSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedTraceSegment
  ( UnprocessedTraceSegment (..),

    -- * Smart constructor
    mkUnprocessedTraceSegment,

    -- * Lenses
    utsErrorCode,
    utsId,
    utsMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a segment that failed processing.
--
-- /See:/ 'mkUnprocessedTraceSegment' smart constructor.
data UnprocessedTraceSegment = UnprocessedTraceSegment'
  { errorCode ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedTraceSegment' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error that caused processing to fail.
-- * 'id' - The segment's ID.
-- * 'message' - The error message.
mkUnprocessedTraceSegment ::
  UnprocessedTraceSegment
mkUnprocessedTraceSegment =
  UnprocessedTraceSegment'
    { errorCode = Lude.Nothing,
      id = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error that caused processing to fail.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsErrorCode :: Lens.Lens' UnprocessedTraceSegment (Lude.Maybe Lude.Text)
utsErrorCode = Lens.lens (errorCode :: UnprocessedTraceSegment -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: UnprocessedTraceSegment)
{-# DEPRECATED utsErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The segment's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsId :: Lens.Lens' UnprocessedTraceSegment (Lude.Maybe Lude.Text)
utsId = Lens.lens (id :: UnprocessedTraceSegment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UnprocessedTraceSegment)
{-# DEPRECATED utsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utsMessage :: Lens.Lens' UnprocessedTraceSegment (Lude.Maybe Lude.Text)
utsMessage = Lens.lens (message :: UnprocessedTraceSegment -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UnprocessedTraceSegment)
{-# DEPRECATED utsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON UnprocessedTraceSegment where
  parseJSON =
    Lude.withObject
      "UnprocessedTraceSegment"
      ( \x ->
          UnprocessedTraceSegment'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Message")
      )
