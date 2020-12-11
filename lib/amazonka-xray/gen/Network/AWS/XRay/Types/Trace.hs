-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Trace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Trace
  ( Trace (..),

    -- * Smart constructor
    mkTrace,

    -- * Lenses
    tLimitExceeded,
    tId,
    tSegments,
    tDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.Segment

-- | A collection of segment documents with matching trace IDs.
--
-- /See:/ 'mkTrace' smart constructor.
data Trace = Trace'
  { limitExceeded :: Lude.Maybe Lude.Bool,
    id :: Lude.Maybe Lude.Text,
    segments :: Lude.Maybe [Segment],
    duration :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trace' with the minimum fields required to make a request.
--
-- * 'duration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
-- * 'id' - The unique identifier for the request that generated the trace's segments and subsegments.
-- * 'limitExceeded' - LimitExceeded is set to true when the trace has exceeded one of the defined quotas. For more information about quotas, see <https://docs.aws.amazon.com/general/latest/gr/xray.html AWS X-Ray endpoints and quotas> .
-- * 'segments' - Segment documents for the segments and subsegments that comprise the trace.
mkTrace ::
  Trace
mkTrace =
  Trace'
    { limitExceeded = Lude.Nothing,
      id = Lude.Nothing,
      segments = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | LimitExceeded is set to true when the trace has exceeded one of the defined quotas. For more information about quotas, see <https://docs.aws.amazon.com/general/latest/gr/xray.html AWS X-Ray endpoints and quotas> .
--
-- /Note:/ Consider using 'limitExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLimitExceeded :: Lens.Lens' Trace (Lude.Maybe Lude.Bool)
tLimitExceeded = Lens.lens (limitExceeded :: Trace -> Lude.Maybe Lude.Bool) (\s a -> s {limitExceeded = a} :: Trace)
{-# DEPRECATED tLimitExceeded "Use generic-lens or generic-optics with 'limitExceeded' instead." #-}

-- | The unique identifier for the request that generated the trace's segments and subsegments.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tId :: Lens.Lens' Trace (Lude.Maybe Lude.Text)
tId = Lens.lens (id :: Trace -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Trace)
{-# DEPRECATED tId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Segment documents for the segments and subsegments that comprise the trace.
--
-- /Note:/ Consider using 'segments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSegments :: Lens.Lens' Trace (Lude.Maybe [Segment])
tSegments = Lens.lens (segments :: Trace -> Lude.Maybe [Segment]) (\s a -> s {segments = a} :: Trace)
{-# DEPRECATED tSegments "Use generic-lens or generic-optics with 'segments' instead." #-}

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDuration :: Lens.Lens' Trace (Lude.Maybe Lude.Double)
tDuration = Lens.lens (duration :: Trace -> Lude.Maybe Lude.Double) (\s a -> s {duration = a} :: Trace)
{-# DEPRECATED tDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromJSON Trace where
  parseJSON =
    Lude.withObject
      "Trace"
      ( \x ->
          Trace'
            Lude.<$> (x Lude..:? "LimitExceeded")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Segments" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Duration")
      )
