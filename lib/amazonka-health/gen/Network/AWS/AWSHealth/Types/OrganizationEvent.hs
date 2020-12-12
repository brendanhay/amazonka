{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEvent
  ( OrganizationEvent (..),

    -- * Smart constructor
    mkOrganizationEvent,

    -- * Lenses
    oeLastUpdatedTime,
    oeArn,
    oeService,
    oeStartTime,
    oeEventScopeCode,
    oeEventTypeCode,
    oeEventTypeCategory,
    oeEndTime,
    oeRegion,
    oeStatusCode,
  )
where

import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about an event, returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization> operation.
--
-- /See:/ 'mkOrganizationEvent' smart constructor.
data OrganizationEvent = OrganizationEvent'
  { lastUpdatedTime ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    service :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    eventScopeCode :: Lude.Maybe EventScopeCode,
    eventTypeCode :: Lude.Maybe Lude.Text,
    eventTypeCategory :: Lude.Maybe EventTypeCategory,
    endTime :: Lude.Maybe Lude.Timestamp,
    region :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe EventStatusCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationEvent' with the minimum fields required to make a request.
--
-- * 'arn' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
-- * 'endTime' - The date and time that the event ended.
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
-- * 'eventTypeCategory' - The category of the event type.
-- * 'eventTypeCode' - The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
-- * 'lastUpdatedTime' - The most recent date and time that the event was updated.
-- * 'region' - The AWS Region name of the event.
-- * 'service' - The AWS service that is affected by the event. For example, EC2, RDS.
-- * 'startTime' - The date and time that the event began.
-- * 'statusCode' - The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
mkOrganizationEvent ::
  OrganizationEvent
mkOrganizationEvent =
  OrganizationEvent'
    { lastUpdatedTime = Lude.Nothing,
      arn = Lude.Nothing,
      service = Lude.Nothing,
      startTime = Lude.Nothing,
      eventScopeCode = Lude.Nothing,
      eventTypeCode = Lude.Nothing,
      eventTypeCategory = Lude.Nothing,
      endTime = Lude.Nothing,
      region = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The most recent date and time that the event was updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeLastUpdatedTime :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Timestamp)
oeLastUpdatedTime = Lens.lens (lastUpdatedTime :: OrganizationEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedTime = a} :: OrganizationEvent)
{-# DEPRECATED oeLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeArn :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Text)
oeArn = Lens.lens (arn :: OrganizationEvent -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: OrganizationEvent)
{-# DEPRECATED oeArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The AWS service that is affected by the event. For example, EC2, RDS.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeService :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Text)
oeService = Lens.lens (service :: OrganizationEvent -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: OrganizationEvent)
{-# DEPRECATED oeService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The date and time that the event began.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStartTime :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Timestamp)
oeStartTime = Lens.lens (startTime :: OrganizationEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: OrganizationEvent)
{-# DEPRECATED oeStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

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
oeEventScopeCode :: Lens.Lens' OrganizationEvent (Lude.Maybe EventScopeCode)
oeEventScopeCode = Lens.lens (eventScopeCode :: OrganizationEvent -> Lude.Maybe EventScopeCode) (\s a -> s {eventScopeCode = a} :: OrganizationEvent)
{-# DEPRECATED oeEventScopeCode "Use generic-lens or generic-optics with 'eventScopeCode' instead." #-}

-- | The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- /Note:/ Consider using 'eventTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEventTypeCode :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Text)
oeEventTypeCode = Lens.lens (eventTypeCode :: OrganizationEvent -> Lude.Maybe Lude.Text) (\s a -> s {eventTypeCode = a} :: OrganizationEvent)
{-# DEPRECATED oeEventTypeCode "Use generic-lens or generic-optics with 'eventTypeCode' instead." #-}

-- | The category of the event type.
--
-- /Note:/ Consider using 'eventTypeCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEventTypeCategory :: Lens.Lens' OrganizationEvent (Lude.Maybe EventTypeCategory)
oeEventTypeCategory = Lens.lens (eventTypeCategory :: OrganizationEvent -> Lude.Maybe EventTypeCategory) (\s a -> s {eventTypeCategory = a} :: OrganizationEvent)
{-# DEPRECATED oeEventTypeCategory "Use generic-lens or generic-optics with 'eventTypeCategory' instead." #-}

-- | The date and time that the event ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeEndTime :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Timestamp)
oeEndTime = Lens.lens (endTime :: OrganizationEvent -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: OrganizationEvent)
{-# DEPRECATED oeEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The AWS Region name of the event.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeRegion :: Lens.Lens' OrganizationEvent (Lude.Maybe Lude.Text)
oeRegion = Lens.lens (region :: OrganizationEvent -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: OrganizationEvent)
{-# DEPRECATED oeRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStatusCode :: Lens.Lens' OrganizationEvent (Lude.Maybe EventStatusCode)
oeStatusCode = Lens.lens (statusCode :: OrganizationEvent -> Lude.Maybe EventStatusCode) (\s a -> s {statusCode = a} :: OrganizationEvent)
{-# DEPRECATED oeStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON OrganizationEvent where
  parseJSON =
    Lude.withObject
      "OrganizationEvent"
      ( \x ->
          OrganizationEvent'
            Lude.<$> (x Lude..:? "lastUpdatedTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "service")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "eventScopeCode")
            Lude.<*> (x Lude..:? "eventTypeCode")
            Lude.<*> (x Lude..:? "eventTypeCategory")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "statusCode")
      )
