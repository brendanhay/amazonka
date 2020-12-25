{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.Event
  ( Event (..),

    -- * Smart constructor
    mkEvent,

    -- * Lenses
    eArn,
    eAvailabilityZone,
    eEndTime,
    eEventScopeCode,
    eEventTypeCategory,
    eEventTypeCode,
    eLastUpdatedTime,
    eRegion,
    eService,
    eStartTime,
    eStatusCode,
  )
where

import qualified Network.AWS.AWSHealth.Types.Arn as Types
import qualified Network.AWS.AWSHealth.Types.AvailabilityZone as Types
import qualified Network.AWS.AWSHealth.Types.EventScopeCode as Types
import qualified Network.AWS.AWSHealth.Types.EventStatusCode as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCategory as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCode as Types
import qualified Network.AWS.AWSHealth.Types.Region as Types
import qualified Network.AWS.AWSHealth.Types.Service as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about an AWS Health event.
--
-- AWS Health events can be public or account-specific:
--
--     * /Public events/ might be service events that are not specific to an AWS account. For example, if there is an issue with an AWS Region, AWS Health provides information about the event, even if you don't use services or resources in that Region.
--
--
--     * /Account-specific/ events are specific to either your AWS account or an account in your organization. For example, if there's an issue with Amazon Elastic Compute Cloud in a Region that you use, AWS Health provides information about the event and the affected resources in the account.
--
--
-- You can determine if an event is public or account-specific by using the @eventScopeCode@ parameter. For more information, see <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html#AWSHealth-Type-Event-eventScopeCode eventScopeCode> .
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Core.Maybe Types.Arn,
    -- | The AWS Availability Zone of the event. For example, us-east-1a.
    availabilityZone :: Core.Maybe Types.AvailabilityZone,
    -- | The date and time that the event ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
    --
    --
    --     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
    --
    --
    --     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
    --
    --
    --     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
    eventScopeCode :: Core.Maybe Types.EventScopeCode,
    -- | The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
    eventTypeCategory :: Core.Maybe Types.EventTypeCategory,
    -- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
    eventTypeCode :: Core.Maybe Types.EventTypeCode,
    -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS region name of the event.
    region :: Core.Maybe Types.Region,
    -- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
    service :: Core.Maybe Types.Service,
    -- | The date and time that the event began.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
    statusCode :: Core.Maybe Types.EventStatusCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent ::
  Event
mkEvent =
  Event'
    { arn = Core.Nothing,
      availabilityZone = Core.Nothing,
      endTime = Core.Nothing,
      eventScopeCode = Core.Nothing,
      eventTypeCategory = Core.Nothing,
      eventTypeCode = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      region = Core.Nothing,
      service = Core.Nothing,
      startTime = Core.Nothing,
      statusCode = Core.Nothing
    }

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Event (Core.Maybe Types.Arn)
eArn = Lens.field @"arn"
{-# DEPRECATED eArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The AWS Availability Zone of the event. For example, us-east-1a.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAvailabilityZone :: Lens.Lens' Event (Core.Maybe Types.AvailabilityZone)
eAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED eAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time that the event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eEndTime = Lens.field @"endTime"
{-# DEPRECATED eEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
--
--
--     * If the @eventScopeCode@ value is @PUBLIC@ , then the @affectedAccounts@ value is always empty.
--
--
--     * If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@ , then the @affectedAccounts@ value lists the affected AWS accounts in your organization. For example, if an event affects a service such as Amazon Elastic Compute Cloud and you have AWS accounts that use that service, those account IDs appear in the response.
--
--
--     * If the @eventScopeCode@ value is @NONE@ , then the @eventArn@ that you specified in the request is invalid or doesn't exist.
--
--
--
-- /Note:/ Consider using 'eventScopeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventScopeCode :: Lens.Lens' Event (Core.Maybe Types.EventScopeCode)
eEventScopeCode = Lens.field @"eventScopeCode"
{-# DEPRECATED eEventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead." #-}

-- | The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
--
-- /Note:/ Consider using 'eventTypeCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCategory :: Lens.Lens' Event (Core.Maybe Types.EventTypeCategory)
eEventTypeCategory = Lens.field @"eventTypeCategory"
{-# DEPRECATED eEventTypeCategory "Use generic-lens or generic-optics with 'eventTypeCategory' instead." #-}

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'eventTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCode :: Lens.Lens' Event (Core.Maybe Types.EventTypeCode)
eEventTypeCode = Lens.field @"eventTypeCode"
{-# DEPRECATED eEventTypeCode "Use generic-lens or generic-optics with 'eventTypeCode' instead." #-}

-- | The most recent date and time that the event was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED eLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The AWS region name of the event.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRegion :: Lens.Lens' Event (Core.Maybe Types.Region)
eRegion = Lens.field @"region"
{-# DEPRECATED eRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eService :: Lens.Lens' Event (Core.Maybe Types.Service)
eService = Lens.field @"service"
{-# DEPRECATED eService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The date and time that the event began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartTime :: Lens.Lens' Event (Core.Maybe Core.NominalDiffTime)
eStartTime = Lens.field @"startTime"
{-# DEPRECATED eStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatusCode :: Lens.Lens' Event (Core.Maybe Types.EventStatusCode)
eStatusCode = Lens.field @"statusCode"
{-# DEPRECATED eStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject "Event" Core.$
      \x ->
        Event'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "availabilityZone")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "eventScopeCode")
          Core.<*> (x Core..:? "eventTypeCategory")
          Core.<*> (x Core..:? "eventTypeCode")
          Core.<*> (x Core..:? "lastUpdatedTime")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "service")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "statusCode")
