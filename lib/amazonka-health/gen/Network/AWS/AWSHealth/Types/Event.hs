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
    eLastUpdatedTime,
    eArn,
    eService,
    eStartTime,
    eEventScopeCode,
    eEventTypeCode,
    eEventTypeCategory,
    eAvailabilityZone,
    eEndTime,
    eRegion,
    eStatusCode,
  )
where

import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Lude.Maybe Lude.Timestamp,
    -- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Lude.Maybe Lude.Text,
    -- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
    service :: Lude.Maybe Lude.Text,
    -- | The date and time that the event began.
    startTime :: Lude.Maybe Lude.Timestamp,
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
    eventScopeCode :: Lude.Maybe EventScopeCode,
    -- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
    eventTypeCode :: Lude.Maybe Lude.Text,
    -- | The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
    eventTypeCategory :: Lude.Maybe EventTypeCategory,
    -- | The AWS Availability Zone of the event. For example, us-east-1a.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The date and time that the event ended.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The AWS region name of the event.
    region :: Lude.Maybe Lude.Text,
    -- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
    statusCode :: Lude.Maybe EventStatusCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- * 'lastUpdatedTime' - The most recent date and time that the event was updated.
-- * 'arn' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
-- * 'service' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
-- * 'startTime' - The date and time that the event began.
-- * 'eventScopeCode' - This parameter specifies if the AWS Health event is a public AWS service event or an account-specific event.
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
-- * 'eventTypeCode' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
-- * 'eventTypeCategory' - The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
-- * 'availabilityZone' - The AWS Availability Zone of the event. For example, us-east-1a.
-- * 'endTime' - The date and time that the event ended.
-- * 'region' - The AWS region name of the event.
-- * 'statusCode' - The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
mkEvent ::
  Event
mkEvent =
  Event'
    { lastUpdatedTime = Lude.Nothing,
      arn = Lude.Nothing,
      service = Lude.Nothing,
      startTime = Lude.Nothing,
      eventScopeCode = Lude.Nothing,
      eventTypeCode = Lude.Nothing,
      eventTypeCategory = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      endTime = Lude.Nothing,
      region = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The most recent date and time that the event was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedTime :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eLastUpdatedTime = Lens.lens (lastUpdatedTime :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: Event)
{-# DEPRECATED eLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eArn = Lens.lens (arn :: Event -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Event)
{-# DEPRECATED eArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eService :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eService = Lens.lens (service :: Event -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: Event)
{-# DEPRECATED eService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The date and time that the event began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartTime :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eStartTime = Lens.lens (startTime :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Event)
{-# DEPRECATED eStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
eEventScopeCode :: Lens.Lens' Event (Lude.Maybe EventScopeCode)
eEventScopeCode = Lens.lens (eventScopeCode :: Event -> Lude.Maybe EventScopeCode) (\s a -> s {eventScopeCode = a} :: Event)
{-# DEPRECATED eEventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead." #-}

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'eventTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCode :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eEventTypeCode = Lens.lens (eventTypeCode :: Event -> Lude.Maybe Lude.Text) (\s a -> s {eventTypeCode = a} :: Event)
{-# DEPRECATED eEventTypeCode "Use generic-lens or generic-optics with 'eventTypeCode' instead." #-}

-- | The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
--
-- /Note:/ Consider using 'eventTypeCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCategory :: Lens.Lens' Event (Lude.Maybe EventTypeCategory)
eEventTypeCategory = Lens.lens (eventTypeCategory :: Event -> Lude.Maybe EventTypeCategory) (\s a -> s {eventTypeCategory = a} :: Event)
{-# DEPRECATED eEventTypeCategory "Use generic-lens or generic-optics with 'eventTypeCategory' instead." #-}

-- | The AWS Availability Zone of the event. For example, us-east-1a.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAvailabilityZone :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eAvailabilityZone = Lens.lens (availabilityZone :: Event -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Event)
{-# DEPRECATED eAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The date and time that the event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTime :: Lens.Lens' Event (Lude.Maybe Lude.Timestamp)
eEndTime = Lens.lens (endTime :: Event -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: Event)
{-# DEPRECATED eEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The AWS region name of the event.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRegion :: Lens.Lens' Event (Lude.Maybe Lude.Text)
eRegion = Lens.lens (region :: Event -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Event)
{-# DEPRECATED eRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStatusCode :: Lens.Lens' Event (Lude.Maybe EventStatusCode)
eStatusCode = Lens.lens (statusCode :: Event -> Lude.Maybe EventStatusCode) (\s a -> s {statusCode = a} :: Event)
{-# DEPRECATED eStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON Event where
  parseJSON =
    Lude.withObject
      "Event"
      ( \x ->
          Event'
            Lude.<$> (x Lude..:? "lastUpdatedTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "service")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "eventScopeCode")
            Lude.<*> (x Lude..:? "eventTypeCode")
            Lude.<*> (x Lude..:? "eventTypeCategory")
            Lude.<*> (x Lude..:? "availabilityZone")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "statusCode")
      )
