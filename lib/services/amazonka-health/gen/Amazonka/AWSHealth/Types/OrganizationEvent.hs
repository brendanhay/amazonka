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
-- Module      : Amazonka.AWSHealth.Types.OrganizationEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.OrganizationEvent where

import Amazonka.AWSHealth.Types.EventScopeCode
import Amazonka.AWSHealth.Types.EventStatusCode
import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an event, returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization>
-- operation.
--
-- /See:/ 'newOrganizationEvent' smart constructor.
data OrganizationEvent = OrganizationEvent'
  { -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Prelude.Maybe Prelude.Text,
    -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the event ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Web Services service that is affected by the event, such as
    -- EC2 and RDS.
    service :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Web Services Region name of the event.
    region :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION@. For example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    eventTypeCode :: Prelude.Maybe Prelude.Text,
    -- | A list of event type category codes. Possible values are @issue@,
    -- @accountNotification@, or @scheduledChange@. Currently, the
    -- @investigation@ value isn\'t supported at this time.
    eventTypeCategory :: Prelude.Maybe EventTypeCategory,
    -- | The most recent status of the event. Possible values are @open@,
    -- @closed@, and @upcoming@.
    statusCode :: Prelude.Maybe EventStatusCode,
    -- | The date and time that the event began.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'organizationEvent_arn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'lastUpdatedTime', 'organizationEvent_lastUpdatedTime' - The most recent date and time that the event was updated.
--
-- 'endTime', 'organizationEvent_endTime' - The date and time that the event ended.
--
-- 'service', 'organizationEvent_service' - The Amazon Web Services service that is affected by the event, such as
-- EC2 and RDS.
--
-- 'eventScopeCode', 'organizationEvent_eventScopeCode' - This parameter specifies if the Health event is a public Amazon Web
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
-- 'region', 'organizationEvent_region' - The Amazon Web Services Region name of the event.
--
-- 'eventTypeCode', 'organizationEvent_eventTypeCode' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'eventTypeCategory', 'organizationEvent_eventTypeCategory' - A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
--
-- 'statusCode', 'organizationEvent_statusCode' - The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
--
-- 'startTime', 'organizationEvent_startTime' - The date and time that the event began.
newOrganizationEvent ::
  OrganizationEvent
newOrganizationEvent =
  OrganizationEvent'
    { arn = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      service = Prelude.Nothing,
      eventScopeCode = Prelude.Nothing,
      region = Prelude.Nothing,
      eventTypeCode = Prelude.Nothing,
      eventTypeCategory = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationEvent_arn :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_arn = Lens.lens (\OrganizationEvent' {arn} -> arn) (\s@OrganizationEvent' {} a -> s {arn = a} :: OrganizationEvent)

-- | The most recent date and time that the event was updated.
organizationEvent_lastUpdatedTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_lastUpdatedTime = Lens.lens (\OrganizationEvent' {lastUpdatedTime} -> lastUpdatedTime) (\s@OrganizationEvent' {} a -> s {lastUpdatedTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Data._Time

-- | The date and time that the event ended.
organizationEvent_endTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_endTime = Lens.lens (\OrganizationEvent' {endTime} -> endTime) (\s@OrganizationEvent' {} a -> s {endTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services service that is affected by the event, such as
-- EC2 and RDS.
organizationEvent_service :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_service = Lens.lens (\OrganizationEvent' {service} -> service) (\s@OrganizationEvent' {} a -> s {service = a} :: OrganizationEvent)

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
organizationEvent_eventScopeCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventScopeCode)
organizationEvent_eventScopeCode = Lens.lens (\OrganizationEvent' {eventScopeCode} -> eventScopeCode) (\s@OrganizationEvent' {} a -> s {eventScopeCode = a} :: OrganizationEvent)

-- | The Amazon Web Services Region name of the event.
organizationEvent_region :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_region = Lens.lens (\OrganizationEvent' {region} -> region) (\s@OrganizationEvent' {} a -> s {region = a} :: OrganizationEvent)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
organizationEvent_eventTypeCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_eventTypeCode = Lens.lens (\OrganizationEvent' {eventTypeCode} -> eventTypeCode) (\s@OrganizationEvent' {} a -> s {eventTypeCode = a} :: OrganizationEvent)

-- | A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
organizationEvent_eventTypeCategory :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventTypeCategory)
organizationEvent_eventTypeCategory = Lens.lens (\OrganizationEvent' {eventTypeCategory} -> eventTypeCategory) (\s@OrganizationEvent' {} a -> s {eventTypeCategory = a} :: OrganizationEvent)

-- | The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
organizationEvent_statusCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventStatusCode)
organizationEvent_statusCode = Lens.lens (\OrganizationEvent' {statusCode} -> statusCode) (\s@OrganizationEvent' {} a -> s {statusCode = a} :: OrganizationEvent)

-- | The date and time that the event began.
organizationEvent_startTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_startTime = Lens.lens (\OrganizationEvent' {startTime} -> startTime) (\s@OrganizationEvent' {} a -> s {startTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON OrganizationEvent where
  parseJSON =
    Data.withObject
      "OrganizationEvent"
      ( \x ->
          OrganizationEvent'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "service")
            Prelude.<*> (x Data..:? "eventScopeCode")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "eventTypeCode")
            Prelude.<*> (x Data..:? "eventTypeCategory")
            Prelude.<*> (x Data..:? "statusCode")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable OrganizationEvent where
  hashWithSalt _salt OrganizationEvent' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` eventScopeCode
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` eventTypeCode
      `Prelude.hashWithSalt` eventTypeCategory
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData OrganizationEvent where
  rnf OrganizationEvent' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf eventScopeCode
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf eventTypeCode
      `Prelude.seq` Prelude.rnf eventTypeCategory
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf startTime
