{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ScheduledAuditMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ScheduledAuditMetadata
  ( ScheduledAuditMetadata (..)
  -- * Smart constructor
  , mkScheduledAuditMetadata
  -- * Lenses
  , samDayOfMonth
  , samDayOfWeek
  , samFrequency
  , samScheduledAuditArn
  , samScheduledAuditName
  ) where

import qualified Network.AWS.IoT.Types.AuditFrequency as Types
import qualified Network.AWS.IoT.Types.DayOfMonth as Types
import qualified Network.AWS.IoT.Types.DayOfWeek as Types
import qualified Network.AWS.IoT.Types.ScheduledAuditArn as Types
import qualified Network.AWS.IoT.Types.ScheduledAuditName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the scheduled audit.
--
-- /See:/ 'mkScheduledAuditMetadata' smart constructor.
data ScheduledAuditMetadata = ScheduledAuditMetadata'
  { dayOfMonth :: Core.Maybe Types.DayOfMonth
    -- ^ The day of the month on which the scheduled audit is run (if the @frequency@ is "MONTHLY"). If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
  , dayOfWeek :: Core.Maybe Types.DayOfWeek
    -- ^ The day of the week on which the scheduled audit is run (if the @frequency@ is "WEEKLY" or "BIWEEKLY").
  , frequency :: Core.Maybe Types.AuditFrequency
    -- ^ How often the scheduled audit occurs.
  , scheduledAuditArn :: Core.Maybe Types.ScheduledAuditArn
    -- ^ The ARN of the scheduled audit.
  , scheduledAuditName :: Core.Maybe Types.ScheduledAuditName
    -- ^ The name of the scheduled audit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledAuditMetadata' value with any optional fields omitted.
mkScheduledAuditMetadata
    :: ScheduledAuditMetadata
mkScheduledAuditMetadata
  = ScheduledAuditMetadata'{dayOfMonth = Core.Nothing,
                            dayOfWeek = Core.Nothing, frequency = Core.Nothing,
                            scheduledAuditArn = Core.Nothing,
                            scheduledAuditName = Core.Nothing}

-- | The day of the month on which the scheduled audit is run (if the @frequency@ is "MONTHLY"). If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samDayOfMonth :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Types.DayOfMonth)
samDayOfMonth = Lens.field @"dayOfMonth"
{-# INLINEABLE samDayOfMonth #-}
{-# DEPRECATED dayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead"  #-}

-- | The day of the week on which the scheduled audit is run (if the @frequency@ is "WEEKLY" or "BIWEEKLY").
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samDayOfWeek :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Types.DayOfWeek)
samDayOfWeek = Lens.field @"dayOfWeek"
{-# INLINEABLE samDayOfWeek #-}
{-# DEPRECATED dayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead"  #-}

-- | How often the scheduled audit occurs.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samFrequency :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Types.AuditFrequency)
samFrequency = Lens.field @"frequency"
{-# INLINEABLE samFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samScheduledAuditArn :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Types.ScheduledAuditArn)
samScheduledAuditArn = Lens.field @"scheduledAuditArn"
{-# INLINEABLE samScheduledAuditArn #-}
{-# DEPRECATED scheduledAuditArn "Use generic-lens or generic-optics with 'scheduledAuditArn' instead"  #-}

-- | The name of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samScheduledAuditName :: Lens.Lens' ScheduledAuditMetadata (Core.Maybe Types.ScheduledAuditName)
samScheduledAuditName = Lens.field @"scheduledAuditName"
{-# INLINEABLE samScheduledAuditName #-}
{-# DEPRECATED scheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead"  #-}

instance Core.FromJSON ScheduledAuditMetadata where
        parseJSON
          = Core.withObject "ScheduledAuditMetadata" Core.$
              \ x ->
                ScheduledAuditMetadata' Core.<$>
                  (x Core..:? "dayOfMonth") Core.<*> x Core..:? "dayOfWeek" Core.<*>
                    x Core..:? "frequency"
                    Core.<*> x Core..:? "scheduledAuditArn"
                    Core.<*> x Core..:? "scheduledAuditName"
