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
    pitrdEarliestRestorableDateTime,
    pitrdLatestRestorableDateTime,
    pitrdPointInTimeRecoveryStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of the point in time settings applied to the table.
--
-- /See:/ 'mkPointInTimeRecoveryDescription' smart constructor.
data PointInTimeRecoveryDescription = PointInTimeRecoveryDescription'
  { -- | Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
    earliestRestorableDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
    latestRestorableDateTime :: Core.Maybe Core.NominalDiffTime,
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
    pointInTimeRecoveryStatus :: Core.Maybe Types.PointInTimeRecoveryStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PointInTimeRecoveryDescription' value with any optional fields omitted.
mkPointInTimeRecoveryDescription ::
  PointInTimeRecoveryDescription
mkPointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { earliestRestorableDateTime =
        Core.Nothing,
      latestRestorableDateTime = Core.Nothing,
      pointInTimeRecoveryStatus = Core.Nothing
    }

-- | Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
--
-- /Note:/ Consider using 'earliestRestorableDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrdEarliestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Core.Maybe Core.NominalDiffTime)
pitrdEarliestRestorableDateTime = Lens.field @"earliestRestorableDateTime"
{-# DEPRECATED pitrdEarliestRestorableDateTime "Use generic-lens or generic-optics with 'earliestRestorableDateTime' instead." #-}

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- /Note:/ Consider using 'latestRestorableDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrdLatestRestorableDateTime :: Lens.Lens' PointInTimeRecoveryDescription (Core.Maybe Core.NominalDiffTime)
pitrdLatestRestorableDateTime = Lens.field @"latestRestorableDateTime"
{-# DEPRECATED pitrdLatestRestorableDateTime "Use generic-lens or generic-optics with 'latestRestorableDateTime' instead." #-}

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
pitrdPointInTimeRecoveryStatus :: Lens.Lens' PointInTimeRecoveryDescription (Core.Maybe Types.PointInTimeRecoveryStatus)
pitrdPointInTimeRecoveryStatus = Lens.field @"pointInTimeRecoveryStatus"
{-# DEPRECATED pitrdPointInTimeRecoveryStatus "Use generic-lens or generic-optics with 'pointInTimeRecoveryStatus' instead." #-}

instance Core.FromJSON PointInTimeRecoveryDescription where
  parseJSON =
    Core.withObject "PointInTimeRecoveryDescription" Core.$
      \x ->
        PointInTimeRecoveryDescription'
          Core.<$> (x Core..:? "EarliestRestorableDateTime")
          Core.<*> (x Core..:? "LatestRestorableDateTime")
          Core.<*> (x Core..:? "PointInTimeRecoveryStatus")
