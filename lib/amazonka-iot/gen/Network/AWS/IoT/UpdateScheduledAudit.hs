{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduled audit, including which checks are performed and how often the audit takes place.
module Network.AWS.IoT.UpdateScheduledAudit
  ( -- * Creating a request
    UpdateScheduledAudit (..),
    mkUpdateScheduledAudit,

    -- ** Request lenses
    usaScheduledAuditName,
    usaDayOfMonth,
    usaDayOfWeek,
    usaFrequency,
    usaTargetCheckNames,

    -- * Destructuring the response
    UpdateScheduledAuditResponse (..),
    mkUpdateScheduledAuditResponse,

    -- ** Response lenses
    usarrsScheduledAuditArn,
    usarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateScheduledAudit' smart constructor.
data UpdateScheduledAudit = UpdateScheduledAudit'
  { -- | The name of the scheduled audit. (Max. 128 chars)
    scheduledAuditName :: Types.ScheduledAuditName,
    -- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
    dayOfMonth :: Core.Maybe Types.DayOfMonth,
    -- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
    dayOfWeek :: Core.Maybe Types.DayOfWeek,
    -- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
    frequency :: Core.Maybe Types.AuditFrequency,
    -- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
    targetCheckNames :: Core.Maybe [Types.AuditCheckName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateScheduledAudit' value with any optional fields omitted.
mkUpdateScheduledAudit ::
  -- | 'scheduledAuditName'
  Types.ScheduledAuditName ->
  UpdateScheduledAudit
mkUpdateScheduledAudit scheduledAuditName =
  UpdateScheduledAudit'
    { scheduledAuditName,
      dayOfMonth = Core.Nothing,
      dayOfWeek = Core.Nothing,
      frequency = Core.Nothing,
      targetCheckNames = Core.Nothing
    }

-- | The name of the scheduled audit. (Max. 128 chars)
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaScheduledAuditName :: Lens.Lens' UpdateScheduledAudit Types.ScheduledAuditName
usaScheduledAuditName = Lens.field @"scheduledAuditName"
{-# DEPRECATED usaScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDayOfMonth :: Lens.Lens' UpdateScheduledAudit (Core.Maybe Types.DayOfMonth)
usaDayOfMonth = Lens.field @"dayOfMonth"
{-# DEPRECATED usaDayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead." #-}

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaDayOfWeek :: Lens.Lens' UpdateScheduledAudit (Core.Maybe Types.DayOfWeek)
usaDayOfWeek = Lens.field @"dayOfWeek"
{-# DEPRECATED usaDayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead." #-}

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY", or "MONTHLY". The start time of each audit is determined by the system.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaFrequency :: Lens.Lens' UpdateScheduledAudit (Core.Maybe Types.AuditFrequency)
usaFrequency = Lens.field @"frequency"
{-# DEPRECATED usaFrequency "Use generic-lens or generic-optics with 'frequency' instead." #-}

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usaTargetCheckNames :: Lens.Lens' UpdateScheduledAudit (Core.Maybe [Types.AuditCheckName])
usaTargetCheckNames = Lens.field @"targetCheckNames"
{-# DEPRECATED usaTargetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead." #-}

instance Core.FromJSON UpdateScheduledAudit where
  toJSON UpdateScheduledAudit {..} =
    Core.object
      ( Core.catMaybes
          [ ("dayOfMonth" Core..=) Core.<$> dayOfMonth,
            ("dayOfWeek" Core..=) Core.<$> dayOfWeek,
            ("frequency" Core..=) Core.<$> frequency,
            ("targetCheckNames" Core..=) Core.<$> targetCheckNames
          ]
      )

instance Core.AWSRequest UpdateScheduledAudit where
  type Rs UpdateScheduledAudit = UpdateScheduledAuditResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ( "/audit/scheduledaudits/"
                Core.<> (Core.toText scheduledAuditName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateScheduledAuditResponse'
            Core.<$> (x Core..:? "scheduledAuditArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateScheduledAuditResponse' smart constructor.
data UpdateScheduledAuditResponse = UpdateScheduledAuditResponse'
  { -- | The ARN of the scheduled audit.
    scheduledAuditArn :: Core.Maybe Types.ScheduledAuditArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateScheduledAuditResponse' value with any optional fields omitted.
mkUpdateScheduledAuditResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateScheduledAuditResponse
mkUpdateScheduledAuditResponse responseStatus =
  UpdateScheduledAuditResponse'
    { scheduledAuditArn = Core.Nothing,
      responseStatus
    }

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarrsScheduledAuditArn :: Lens.Lens' UpdateScheduledAuditResponse (Core.Maybe Types.ScheduledAuditArn)
usarrsScheduledAuditArn = Lens.field @"scheduledAuditArn"
{-# DEPRECATED usarrsScheduledAuditArn "Use generic-lens or generic-optics with 'scheduledAuditArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usarrsResponseStatus :: Lens.Lens' UpdateScheduledAuditResponse Core.Int
usarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
