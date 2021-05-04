{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.Event where

import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about an AWS Health event.
--
-- AWS Health events can be public or account-specific:
--
-- -   /Public events/ might be service events that are not specific to an
--     AWS account. For example, if there is an issue with an AWS Region,
--     AWS Health provides information about the event, even if you don\'t
--     use services or resources in that Region.
--
-- -   /Account-specific/ events are specific to either your AWS account or
--     an account in your organization. For example, if there\'s an issue
--     with Amazon Elastic Compute Cloud in a Region that you use, AWS
--     Health provides information about the event and the affected
--     resources in the account.
--
-- You can determine if an event is public or account-specific by using the
-- @eventScopeCode@ parameter. For more information, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html#AWSHealth-Type-Event-eventScopeCode eventScopeCode>.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The category of the event. Possible values are @issue@,
    -- @scheduledChange@, and @accountNotification@.
    eventTypeCategory :: Prelude.Maybe EventTypeCategory,
    -- | This parameter specifies if the AWS Health event is a public AWS service
    -- event or an account-specific event.
    --
    -- -   If the @eventScopeCode@ value is @PUBLIC@, then the
    --     @affectedAccounts@ value is always empty.
    --
    -- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
    --     @affectedAccounts@ value lists the affected AWS accounts in your
    --     organization. For example, if an event affects a service such as
    --     Amazon Elastic Compute Cloud and you have AWS accounts that use that
    --     service, those account IDs appear in the response.
    --
    -- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
    --     you specified in the request is invalid or doesn\'t exist.
    eventScopeCode :: Prelude.Maybe EventScopeCode,
    -- | The date and time that the event began.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS service that is affected by the event. For example, @EC2@,
    -- @RDS@.
    service :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS Availability Zone of the event. For example, us-east-1a.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The most recent status of the event. Possible values are @open@,
    -- @closed@, and @upcoming@.
    statusCode :: Prelude.Maybe EventStatusCode,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION @; for example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    eventTypeCode :: Prelude.Maybe Prelude.Text,
    -- | The AWS region name of the event.
    region :: Prelude.Maybe Prelude.Text,
    -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypeCategory', 'event_eventTypeCategory' - The category of the event. Possible values are @issue@,
-- @scheduledChange@, and @accountNotification@.
--
-- 'eventScopeCode', 'event_eventScopeCode' - This parameter specifies if the AWS Health event is a public AWS service
-- event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected AWS accounts in your
--     organization. For example, if an event affects a service such as
--     Amazon Elastic Compute Cloud and you have AWS accounts that use that
--     service, those account IDs appear in the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
--
-- 'startTime', 'event_startTime' - The date and time that the event began.
--
-- 'service', 'event_service' - The AWS service that is affected by the event. For example, @EC2@,
-- @RDS@.
--
-- 'arn', 'event_arn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'endTime', 'event_endTime' - The date and time that the event ended.
--
-- 'availabilityZone', 'event_availabilityZone' - The AWS Availability Zone of the event. For example, us-east-1a.
--
-- 'statusCode', 'event_statusCode' - The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
--
-- 'eventTypeCode', 'event_eventTypeCode' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'region', 'event_region' - The AWS region name of the event.
--
-- 'lastUpdatedTime', 'event_lastUpdatedTime' - The most recent date and time that the event was updated.
newEvent ::
  Event
newEvent =
  Event'
    { eventTypeCategory = Prelude.Nothing,
      eventScopeCode = Prelude.Nothing,
      startTime = Prelude.Nothing,
      service = Prelude.Nothing,
      arn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      eventTypeCode = Prelude.Nothing,
      region = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The category of the event. Possible values are @issue@,
-- @scheduledChange@, and @accountNotification@.
event_eventTypeCategory :: Lens.Lens' Event (Prelude.Maybe EventTypeCategory)
event_eventTypeCategory = Lens.lens (\Event' {eventTypeCategory} -> eventTypeCategory) (\s@Event' {} a -> s {eventTypeCategory = a} :: Event)

-- | This parameter specifies if the AWS Health event is a public AWS service
-- event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected AWS accounts in your
--     organization. For example, if an event affects a service such as
--     Amazon Elastic Compute Cloud and you have AWS accounts that use that
--     service, those account IDs appear in the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
event_eventScopeCode :: Lens.Lens' Event (Prelude.Maybe EventScopeCode)
event_eventScopeCode = Lens.lens (\Event' {eventScopeCode} -> eventScopeCode) (\s@Event' {} a -> s {eventScopeCode = a} :: Event)

-- | The date and time that the event began.
event_startTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_startTime = Lens.lens (\Event' {startTime} -> startTime) (\s@Event' {} a -> s {startTime = a} :: Event) Prelude.. Lens.mapping Prelude._Time

-- | The AWS service that is affected by the event. For example, @EC2@,
-- @RDS@.
event_service :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_service = Lens.lens (\Event' {service} -> service) (\s@Event' {} a -> s {service = a} :: Event)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
event_arn :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_arn = Lens.lens (\Event' {arn} -> arn) (\s@Event' {} a -> s {arn = a} :: Event)

-- | The date and time that the event ended.
event_endTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_endTime = Lens.lens (\Event' {endTime} -> endTime) (\s@Event' {} a -> s {endTime = a} :: Event) Prelude.. Lens.mapping Prelude._Time

-- | The AWS Availability Zone of the event. For example, us-east-1a.
event_availabilityZone :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_availabilityZone = Lens.lens (\Event' {availabilityZone} -> availabilityZone) (\s@Event' {} a -> s {availabilityZone = a} :: Event)

-- | The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
event_statusCode :: Lens.Lens' Event (Prelude.Maybe EventStatusCode)
event_statusCode = Lens.lens (\Event' {statusCode} -> statusCode) (\s@Event' {} a -> s {statusCode = a} :: Event)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
event_eventTypeCode :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventTypeCode = Lens.lens (\Event' {eventTypeCode} -> eventTypeCode) (\s@Event' {} a -> s {eventTypeCode = a} :: Event)

-- | The AWS region name of the event.
event_region :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_region = Lens.lens (\Event' {region} -> region) (\s@Event' {} a -> s {region = a} :: Event)

-- | The most recent date and time that the event was updated.
event_lastUpdatedTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_lastUpdatedTime = Lens.lens (\Event' {lastUpdatedTime} -> lastUpdatedTime) (\s@Event' {} a -> s {lastUpdatedTime = a} :: Event) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Event where
  parseJSON =
    Prelude.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Prelude..:? "eventTypeCategory")
            Prelude.<*> (x Prelude..:? "eventScopeCode")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "service")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "availabilityZone")
            Prelude.<*> (x Prelude..:? "statusCode")
            Prelude.<*> (x Prelude..:? "eventTypeCode")
            Prelude.<*> (x Prelude..:? "region")
            Prelude.<*> (x Prelude..:? "lastUpdatedTime")
      )

instance Prelude.Hashable Event

instance Prelude.NFData Event
