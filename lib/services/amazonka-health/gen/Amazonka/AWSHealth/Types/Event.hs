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
-- Module      : Amazonka.AWSHealth.Types.Event
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.Event where

import Amazonka.AWSHealth.Types.EventScopeCode
import Amazonka.AWSHealth.Types.EventStatusCode
import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an Health event.
--
-- Health events can be public or account-specific:
--
-- -   /Public events/ might be service events that are not specific to an
--     Amazon Web Services account. For example, if there is an issue with
--     an Amazon Web Services Region, Health provides information about the
--     event, even if you don\'t use services or resources in that Region.
--
-- -   /Account-specific/ events are specific to either your Amazon Web
--     Services account or an account in your organization. For example, if
--     there\'s an issue with Amazon Elastic Compute Cloud in a Region that
--     you use, Health provides information about the event and the
--     affected resources in the account.
--
-- You can determine if an event is public or account-specific by using the
-- @eventScopeCode@ parameter. For more information, see
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html#AWSHealth-Type-Event-eventScopeCode eventScopeCode>.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Availability Zone of the event. For example,
    -- us-east-1a.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | This parameter specifies if the Health event is a public Amazon Web
    -- Services service event or an account-specific event.
    --
    -- -   If the @eventScopeCode@ value is @PUBLIC@, then the
    --     @affectedAccounts@ value is always empty.
    --
    -- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
    --     @affectedAccounts@ value lists the affected Amazon Web Services
    --     accounts in your organization. For example, if an event affects a
    --     service such as Amazon Elastic Compute Cloud and you have Amazon Web
    --     Services accounts that use that service, those account IDs appear in
    --     the response.
    --
    -- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
    --     you specified in the request is invalid or doesn\'t exist.
    eventScopeCode :: Prelude.Maybe EventScopeCode,
    -- | A list of event type category codes. Possible values are @issue@,
    -- @accountNotification@, or @scheduledChange@. Currently, the
    -- @investigation@ value isn\'t supported at this time.
    eventTypeCategory :: Prelude.Maybe EventTypeCategory,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION @; for example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    eventTypeCode :: Prelude.Maybe Prelude.Text,
    -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services Region name of the event.
    region :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service that is affected by the event. For
    -- example, @EC2@, @RDS@.
    service :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event began.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The most recent status of the event. Possible values are @open@,
    -- @closed@, and @upcoming@.
    statusCode :: Prelude.Maybe EventStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'event_arn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'availabilityZone', 'event_availabilityZone' - The Amazon Web Services Availability Zone of the event. For example,
-- us-east-1a.
--
-- 'endTime', 'event_endTime' - The date and time that the event ended.
--
-- 'eventScopeCode', 'event_eventScopeCode' - This parameter specifies if the Health event is a public Amazon Web
-- Services service event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected Amazon Web Services
--     accounts in your organization. For example, if an event affects a
--     service such as Amazon Elastic Compute Cloud and you have Amazon Web
--     Services accounts that use that service, those account IDs appear in
--     the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
--
-- 'eventTypeCategory', 'event_eventTypeCategory' - A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
--
-- 'eventTypeCode', 'event_eventTypeCode' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'lastUpdatedTime', 'event_lastUpdatedTime' - The most recent date and time that the event was updated.
--
-- 'region', 'event_region' - The Amazon Web Services Region name of the event.
--
-- 'service', 'event_service' - The Amazon Web Services service that is affected by the event. For
-- example, @EC2@, @RDS@.
--
-- 'startTime', 'event_startTime' - The date and time that the event began.
--
-- 'statusCode', 'event_statusCode' - The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
newEvent ::
  Event
newEvent =
  Event'
    { arn = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      endTime = Prelude.Nothing,
      eventScopeCode = Prelude.Nothing,
      eventTypeCategory = Prelude.Nothing,
      eventTypeCode = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      region = Prelude.Nothing,
      service = Prelude.Nothing,
      startTime = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
event_arn :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_arn = Lens.lens (\Event' {arn} -> arn) (\s@Event' {} a -> s {arn = a} :: Event)

-- | The Amazon Web Services Availability Zone of the event. For example,
-- us-east-1a.
event_availabilityZone :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_availabilityZone = Lens.lens (\Event' {availabilityZone} -> availabilityZone) (\s@Event' {} a -> s {availabilityZone = a} :: Event)

-- | The date and time that the event ended.
event_endTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_endTime = Lens.lens (\Event' {endTime} -> endTime) (\s@Event' {} a -> s {endTime = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | This parameter specifies if the Health event is a public Amazon Web
-- Services service event or an account-specific event.
--
-- -   If the @eventScopeCode@ value is @PUBLIC@, then the
--     @affectedAccounts@ value is always empty.
--
-- -   If the @eventScopeCode@ value is @ACCOUNT_SPECIFIC@, then the
--     @affectedAccounts@ value lists the affected Amazon Web Services
--     accounts in your organization. For example, if an event affects a
--     service such as Amazon Elastic Compute Cloud and you have Amazon Web
--     Services accounts that use that service, those account IDs appear in
--     the response.
--
-- -   If the @eventScopeCode@ value is @NONE@, then the @eventArn@ that
--     you specified in the request is invalid or doesn\'t exist.
event_eventScopeCode :: Lens.Lens' Event (Prelude.Maybe EventScopeCode)
event_eventScopeCode = Lens.lens (\Event' {eventScopeCode} -> eventScopeCode) (\s@Event' {} a -> s {eventScopeCode = a} :: Event)

-- | A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
event_eventTypeCategory :: Lens.Lens' Event (Prelude.Maybe EventTypeCategory)
event_eventTypeCategory = Lens.lens (\Event' {eventTypeCategory} -> eventTypeCategory) (\s@Event' {} a -> s {eventTypeCategory = a} :: Event)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION @; for example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
event_eventTypeCode :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_eventTypeCode = Lens.lens (\Event' {eventTypeCode} -> eventTypeCode) (\s@Event' {} a -> s {eventTypeCode = a} :: Event)

-- | The most recent date and time that the event was updated.
event_lastUpdatedTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_lastUpdatedTime = Lens.lens (\Event' {lastUpdatedTime} -> lastUpdatedTime) (\s@Event' {} a -> s {lastUpdatedTime = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services Region name of the event.
event_region :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_region = Lens.lens (\Event' {region} -> region) (\s@Event' {} a -> s {region = a} :: Event)

-- | The Amazon Web Services service that is affected by the event. For
-- example, @EC2@, @RDS@.
event_service :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_service = Lens.lens (\Event' {service} -> service) (\s@Event' {} a -> s {service = a} :: Event)

-- | The date and time that the event began.
event_startTime :: Lens.Lens' Event (Prelude.Maybe Prelude.UTCTime)
event_startTime = Lens.lens (\Event' {startTime} -> startTime) (\s@Event' {} a -> s {startTime = a} :: Event) Prelude.. Lens.mapping Data._Time

-- | The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
event_statusCode :: Lens.Lens' Event (Prelude.Maybe EventStatusCode)
event_statusCode = Lens.lens (\Event' {statusCode} -> statusCode) (\s@Event' {} a -> s {statusCode = a} :: Event)

instance Data.FromJSON Event where
  parseJSON =
    Data.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "availabilityZone")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "eventScopeCode")
            Prelude.<*> (x Data..:? "eventTypeCategory")
            Prelude.<*> (x Data..:? "eventTypeCode")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "service")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "statusCode")
      )

instance Prelude.Hashable Event where
  hashWithSalt _salt Event' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` eventScopeCode
      `Prelude.hashWithSalt` eventTypeCategory
      `Prelude.hashWithSalt` eventTypeCode
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf eventScopeCode
      `Prelude.seq` Prelude.rnf eventTypeCategory
      `Prelude.seq` Prelude.rnf eventTypeCode
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf statusCode
