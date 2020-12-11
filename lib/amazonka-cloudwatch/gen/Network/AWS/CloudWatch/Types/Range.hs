-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Range
  ( Range (..),

    -- * Smart constructor
    mkRange,

    -- * Lenses
    rStartTime,
    rEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies one range of days or times to exclude from use for training an anomaly detection model.
--
-- /See:/ 'mkRange' smart constructor.
data Range = Range'
  { startTime :: Lude.ISO8601,
    endTime :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- * 'endTime' - The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
-- * 'startTime' - The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
mkRange ::
  -- | 'startTime'
  Lude.ISO8601 ->
  -- | 'endTime'
  Lude.ISO8601 ->
  Range
mkRange pStartTime_ pEndTime_ =
  Range' {startTime = pStartTime_, endTime = pEndTime_}

-- | The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStartTime :: Lens.Lens' Range Lude.ISO8601
rStartTime = Lens.lens (startTime :: Range -> Lude.ISO8601) (\s a -> s {startTime = a} :: Range)
{-# DEPRECATED rStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEndTime :: Lens.Lens' Range Lude.ISO8601
rEndTime = Lens.lens (endTime :: Range -> Lude.ISO8601) (\s a -> s {endTime = a} :: Range)
{-# DEPRECATED rEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromXML Range where
  parseXML x =
    Range'
      Lude.<$> (x Lude..@ "StartTime") Lude.<*> (x Lude..@ "EndTime")

instance Lude.ToQuery Range where
  toQuery Range' {..} =
    Lude.mconcat
      ["StartTime" Lude.=: startTime, "EndTime" Lude.=: endTime]
