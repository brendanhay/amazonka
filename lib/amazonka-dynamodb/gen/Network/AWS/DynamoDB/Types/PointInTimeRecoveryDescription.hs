{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
  ( PointInTimeRecoveryDescription (..),

    -- * Smart constructor
    mkPointInTimeRecoveryDescription,

    -- * Lenses
    pitrdPointInTimeRecoveryStatus,
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,
  )
where

import Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of the point in time settings applied to the table.
--
-- /See:/ 'mkPointInTimeRecoveryDescription' smart constructor.
data PointInTimeRecoveryDescription = PointInTimeRecoveryDescription'
  { -- | The current state of point in time recovery:
    --
    --
    --     * @ENABLING@ - Point in time recovery is being enabled.
    --
    --
    --     * @ENABLED@ - Point in time recovery is enabled.
    --
    --
    --     * @DISABLED@ - Point in time recovery is disabled.
    pointInTimeRecoveryStatus :: Lude.Maybe PointInTimeRecoveryStatus,
    -- | Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
    earliestRestorableDateTime :: Lude.Maybe Lude.Timestamp,
    -- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
    latestRestorableDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PointInTimeRecoveryDescription' with the minimum fields required to make a request.
--
-- * 'pointInTimeRecoveryStatus' - The current state of point in time recovery:
--
--
--     * @ENABLING@ - Point in time recovery is being enabled.
--
--
--     * @ENABLED@ - Point in time recovery is enabled.
--
--
--     * @DISABLED@ - Point in time recovery is disabled.
--
--
-- * 'earliestRestorableDateTime' - Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
-- * 'latestRestorableDateTime' - @LatestRestorableDateTime@ is typically 5 minutes before the current time.
mkPointInTimeRecoveryDescription ::
  PointInTimeRecoveryDescription
mkPointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { pointInTimeRecoveryStatus =
        Lude.Nothing,
      earliestRestorableDateTime = Lude.Nothing,
      latestRestorableDateTime = Lude.Nothing
    }

-- | The current state of point in time recovery:
--
--
--     * @ENABLING@ - Point in time recovery is being enabled.
--
--
--     * @ENABLED@ - Point in time recovery is enabled.
--
--
--     * @DISABLED@ - Point in time recovery is disabled.
--
--
--
-- /Note:/ Consider using 'pointInTimeRecoveryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrdPointInTimeRecoveryStatus :: Lens.Lens' PointInTimeRecoveryDescription (Lude.Maybe PointInTimeRecoveryStatus)
pitrdPointInTimeRecoveryStatus = Lens.lens (pointInTimeRecoveryStatus :: PointInTimeRecoveryDescription -> Lude.Maybe PointInTimeRecoveryStatus) (\s a -> s {pointInTimeRecoveryStatus = a} :: PointInTimeRecoveryDescription)
{-# DEPRECATED pitrdPointInTimeRecoveryStatus "Use generic-lens or generic-optics with 'pointInTimeRecoveryStatus' instead." #-}

-- | Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
--
-- /Note:/ Consider using 'earliestRestorableDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrdEarliestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Lude.Maybe Lude.Timestamp)
pitrdEarliestRestorableDateTime = Lens.lens (earliestRestorableDateTime :: PointInTimeRecoveryDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {earliestRestorableDateTime = a} :: PointInTimeRecoveryDescription)
{-# DEPRECATED pitrdEarliestRestorableDateTime "Use generic-lens or generic-optics with 'earliestRestorableDateTime' instead." #-}

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- /Note:/ Consider using 'latestRestorableDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrdLatestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Lude.Maybe Lude.Timestamp)
pitrdLatestRestorableDateTime = Lens.lens (latestRestorableDateTime :: PointInTimeRecoveryDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {latestRestorableDateTime = a} :: PointInTimeRecoveryDescription)
{-# DEPRECATED pitrdLatestRestorableDateTime "Use generic-lens or generic-optics with 'latestRestorableDateTime' instead." #-}

instance Lude.FromJSON PointInTimeRecoveryDescription where
  parseJSON =
    Lude.withObject
      "PointInTimeRecoveryDescription"
      ( \x ->
          PointInTimeRecoveryDescription'
            Lude.<$> (x Lude..:? "PointInTimeRecoveryStatus")
            Lude.<*> (x Lude..:? "EarliestRestorableDateTime")
            Lude.<*> (x Lude..:? "LatestRestorableDateTime")
      )
