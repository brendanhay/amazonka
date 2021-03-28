{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scheduled audit that is run at a specified time interval.
module Network.AWS.IoT.CreateScheduledAudit
    (
    -- * Creating a request
      CreateScheduledAudit (..)
    , mkCreateScheduledAudit
    -- ** Request lenses
    , csaFrequency
    , csaTargetCheckNames
    , csaScheduledAuditName
    , csaDayOfMonth
    , csaDayOfWeek
    , csaTags

    -- * Destructuring the response
    , CreateScheduledAuditResponse (..)
    , mkCreateScheduledAuditResponse
    -- ** Response lenses
    , csarrsScheduledAuditArn
    , csarrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateScheduledAudit' smart constructor.
data CreateScheduledAudit = CreateScheduledAudit'
  { frequency :: Types.AuditFrequency
    -- ^ How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
  , targetCheckNames :: [Types.AuditCheckName]
    -- ^ Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
  , scheduledAuditName :: Types.ScheduledAuditName
    -- ^ The name you want to give to the scheduled audit. (Max. 128 chars)
  , dayOfMonth :: Core.Maybe Types.DayOfMonth
    -- ^ The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
  , dayOfWeek :: Core.Maybe Types.DayOfWeek
    -- ^ The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata that can be used to manage the scheduled audit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScheduledAudit' value with any optional fields omitted.
mkCreateScheduledAudit
    :: Types.AuditFrequency -- ^ 'frequency'
    -> Types.ScheduledAuditName -- ^ 'scheduledAuditName'
    -> CreateScheduledAudit
mkCreateScheduledAudit frequency scheduledAuditName
  = CreateScheduledAudit'{frequency, targetCheckNames = Core.mempty,
                          scheduledAuditName, dayOfMonth = Core.Nothing,
                          dayOfWeek = Core.Nothing, tags = Core.Nothing}

-- | How often the scheduled audit takes place. Can be one of "DAILY", "WEEKLY", "BIWEEKLY" or "MONTHLY". The start time of each audit is determined by the system.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaFrequency :: Lens.Lens' CreateScheduledAudit Types.AuditFrequency
csaFrequency = Lens.field @"frequency"
{-# INLINEABLE csaFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

-- | Which checks are performed during the scheduled audit. Checks must be enabled for your account. (Use @DescribeAccountAuditConfiguration@ to see the list of all checks, including those that are enabled or use @UpdateAccountAuditConfiguration@ to select which checks are enabled.)
--
-- /Note:/ Consider using 'targetCheckNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTargetCheckNames :: Lens.Lens' CreateScheduledAudit [Types.AuditCheckName]
csaTargetCheckNames = Lens.field @"targetCheckNames"
{-# INLINEABLE csaTargetCheckNames #-}
{-# DEPRECATED targetCheckNames "Use generic-lens or generic-optics with 'targetCheckNames' instead"  #-}

-- | The name you want to give to the scheduled audit. (Max. 128 chars)
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaScheduledAuditName :: Lens.Lens' CreateScheduledAudit Types.ScheduledAuditName
csaScheduledAuditName = Lens.field @"scheduledAuditName"
{-# INLINEABLE csaScheduledAuditName #-}
{-# DEPRECATED scheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead"  #-}

-- | The day of the month on which the scheduled audit takes place. Can be "1" through "31" or "LAST". This field is required if the "frequency" parameter is set to "MONTHLY". If days 29-31 are specified, and the month does not have that many days, the audit takes place on the "LAST" day of the month.
--
-- /Note:/ Consider using 'dayOfMonth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDayOfMonth :: Lens.Lens' CreateScheduledAudit (Core.Maybe Types.DayOfMonth)
csaDayOfMonth = Lens.field @"dayOfMonth"
{-# INLINEABLE csaDayOfMonth #-}
{-# DEPRECATED dayOfMonth "Use generic-lens or generic-optics with 'dayOfMonth' instead"  #-}

-- | The day of the week on which the scheduled audit takes place. Can be one of "SUN", "MON", "TUE", "WED", "THU", "FRI", or "SAT". This field is required if the "frequency" parameter is set to "WEEKLY" or "BIWEEKLY".
--
-- /Note:/ Consider using 'dayOfWeek' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDayOfWeek :: Lens.Lens' CreateScheduledAudit (Core.Maybe Types.DayOfWeek)
csaDayOfWeek = Lens.field @"dayOfWeek"
{-# INLINEABLE csaDayOfWeek #-}
{-# DEPRECATED dayOfWeek "Use generic-lens or generic-optics with 'dayOfWeek' instead"  #-}

-- | Metadata that can be used to manage the scheduled audit.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaTags :: Lens.Lens' CreateScheduledAudit (Core.Maybe [Types.Tag])
csaTags = Lens.field @"tags"
{-# INLINEABLE csaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateScheduledAudit where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateScheduledAudit where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateScheduledAudit where
        toJSON CreateScheduledAudit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("frequency" Core..= frequency),
                  Core.Just ("targetCheckNames" Core..= targetCheckNames),
                  ("dayOfMonth" Core..=) Core.<$> dayOfMonth,
                  ("dayOfWeek" Core..=) Core.<$> dayOfWeek,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateScheduledAudit where
        type Rs CreateScheduledAudit = CreateScheduledAuditResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/audit/scheduledaudits/" Core.<> Core.toText scheduledAuditName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateScheduledAuditResponse' Core.<$>
                   (x Core..:? "scheduledAuditArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateScheduledAuditResponse' smart constructor.
data CreateScheduledAuditResponse = CreateScheduledAuditResponse'
  { scheduledAuditArn :: Core.Maybe Types.ScheduledAuditArn
    -- ^ The ARN of the scheduled audit.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateScheduledAuditResponse' value with any optional fields omitted.
mkCreateScheduledAuditResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateScheduledAuditResponse
mkCreateScheduledAuditResponse responseStatus
  = CreateScheduledAuditResponse'{scheduledAuditArn = Core.Nothing,
                                  responseStatus}

-- | The ARN of the scheduled audit.
--
-- /Note:/ Consider using 'scheduledAuditArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarrsScheduledAuditArn :: Lens.Lens' CreateScheduledAuditResponse (Core.Maybe Types.ScheduledAuditArn)
csarrsScheduledAuditArn = Lens.field @"scheduledAuditArn"
{-# INLINEABLE csarrsScheduledAuditArn #-}
{-# DEPRECATED scheduledAuditArn "Use generic-lens or generic-optics with 'scheduledAuditArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarrsResponseStatus :: Lens.Lens' CreateScheduledAuditResponse Core.Int
csarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
