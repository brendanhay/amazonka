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
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEvent where

import Network.AWS.AWSHealth.Types.EventScopeCode
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about an event, returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization>
-- operation.
--
-- /See:/ 'newOrganizationEvent' smart constructor.
data OrganizationEvent = OrganizationEvent'
  { -- | The category of the event type.
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
    -- | The AWS service that is affected by the event. For example, EC2, RDS.
    service :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event ended.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The most recent status of the event. Possible values are @open@,
    -- @closed@, and @upcoming@.
    statusCode :: Prelude.Maybe EventStatusCode,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION@. For example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    eventTypeCode :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region name of the event.
    region :: Prelude.Maybe Prelude.Text,
    -- | The most recent date and time that the event was updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypeCategory', 'organizationEvent_eventTypeCategory' - The category of the event type.
--
-- 'eventScopeCode', 'organizationEvent_eventScopeCode' - This parameter specifies if the AWS Health event is a public AWS service
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
-- 'startTime', 'organizationEvent_startTime' - The date and time that the event began.
--
-- 'service', 'organizationEvent_service' - The AWS service that is affected by the event. For example, EC2, RDS.
--
-- 'arn', 'organizationEvent_arn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'endTime', 'organizationEvent_endTime' - The date and time that the event ended.
--
-- 'statusCode', 'organizationEvent_statusCode' - The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
--
-- 'eventTypeCode', 'organizationEvent_eventTypeCode' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'region', 'organizationEvent_region' - The AWS Region name of the event.
--
-- 'lastUpdatedTime', 'organizationEvent_lastUpdatedTime' - The most recent date and time that the event was updated.
newOrganizationEvent ::
  OrganizationEvent
newOrganizationEvent =
  OrganizationEvent'
    { eventTypeCategory =
        Prelude.Nothing,
      eventScopeCode = Prelude.Nothing,
      startTime = Prelude.Nothing,
      service = Prelude.Nothing,
      arn = Prelude.Nothing,
      endTime = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      eventTypeCode = Prelude.Nothing,
      region = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The category of the event type.
organizationEvent_eventTypeCategory :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventTypeCategory)
organizationEvent_eventTypeCategory = Lens.lens (\OrganizationEvent' {eventTypeCategory} -> eventTypeCategory) (\s@OrganizationEvent' {} a -> s {eventTypeCategory = a} :: OrganizationEvent)

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
organizationEvent_eventScopeCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventScopeCode)
organizationEvent_eventScopeCode = Lens.lens (\OrganizationEvent' {eventScopeCode} -> eventScopeCode) (\s@OrganizationEvent' {} a -> s {eventScopeCode = a} :: OrganizationEvent)

-- | The date and time that the event began.
organizationEvent_startTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_startTime = Lens.lens (\OrganizationEvent' {startTime} -> startTime) (\s@OrganizationEvent' {} a -> s {startTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Prelude._Time

-- | The AWS service that is affected by the event. For example, EC2, RDS.
organizationEvent_service :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_service = Lens.lens (\OrganizationEvent' {service} -> service) (\s@OrganizationEvent' {} a -> s {service = a} :: OrganizationEvent)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationEvent_arn :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_arn = Lens.lens (\OrganizationEvent' {arn} -> arn) (\s@OrganizationEvent' {} a -> s {arn = a} :: OrganizationEvent)

-- | The date and time that the event ended.
organizationEvent_endTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_endTime = Lens.lens (\OrganizationEvent' {endTime} -> endTime) (\s@OrganizationEvent' {} a -> s {endTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Prelude._Time

-- | The most recent status of the event. Possible values are @open@,
-- @closed@, and @upcoming@.
organizationEvent_statusCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe EventStatusCode)
organizationEvent_statusCode = Lens.lens (\OrganizationEvent' {statusCode} -> statusCode) (\s@OrganizationEvent' {} a -> s {statusCode = a} :: OrganizationEvent)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
organizationEvent_eventTypeCode :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_eventTypeCode = Lens.lens (\OrganizationEvent' {eventTypeCode} -> eventTypeCode) (\s@OrganizationEvent' {} a -> s {eventTypeCode = a} :: OrganizationEvent)

-- | The AWS Region name of the event.
organizationEvent_region :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.Text)
organizationEvent_region = Lens.lens (\OrganizationEvent' {region} -> region) (\s@OrganizationEvent' {} a -> s {region = a} :: OrganizationEvent)

-- | The most recent date and time that the event was updated.
organizationEvent_lastUpdatedTime :: Lens.Lens' OrganizationEvent (Prelude.Maybe Prelude.UTCTime)
organizationEvent_lastUpdatedTime = Lens.lens (\OrganizationEvent' {lastUpdatedTime} -> lastUpdatedTime) (\s@OrganizationEvent' {} a -> s {lastUpdatedTime = a} :: OrganizationEvent) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON OrganizationEvent where
  parseJSON =
    Prelude.withObject
      "OrganizationEvent"
      ( \x ->
          OrganizationEvent'
            Prelude.<$> (x Prelude..:? "eventTypeCategory")
            Prelude.<*> (x Prelude..:? "eventScopeCode")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "service")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "statusCode")
            Prelude.<*> (x Prelude..:? "eventTypeCode")
            Prelude.<*> (x Prelude..:? "region")
            Prelude.<*> (x Prelude..:? "lastUpdatedTime")
      )

instance Prelude.Hashable OrganizationEvent

instance Prelude.NFData OrganizationEvent
