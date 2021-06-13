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
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The values to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization>
-- operation.
--
-- /See:/ 'newOrganizationEventFilter' smart constructor.
data OrganizationEventFilter = OrganizationEventFilter'
  { -- | The AWS services associated with the event. For example, @EC2@, @RDS@.
    services :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    startTime :: Prelude.Maybe DateTimeRange,
    -- | A list of entity ARNs (unique identifiers).
    entityArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of unique identifiers for event types. For example,
    -- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
    eventTypeCodes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    endTime :: Prelude.Maybe DateTimeRange,
    -- | A list of event status codes.
    eventStatusCodes :: Prelude.Maybe (Prelude.NonEmpty EventStatusCode),
    -- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
    -- EBS volumes (vol-426ab23e).
    entityValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of AWS Regions.
    regions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of event type category codes (issue, scheduledChange, or
    -- accountNotification).
    eventTypeCategories :: Prelude.Maybe (Prelude.NonEmpty EventTypeCategory),
    -- | A list of 12-digit AWS account numbers that contains the affected
    -- entities.
    awsAccountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    lastUpdatedTime :: Prelude.Maybe DateTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'organizationEventFilter_services' - The AWS services associated with the event. For example, @EC2@, @RDS@.
--
-- 'startTime', 'organizationEventFilter_startTime' - Undocumented member.
--
-- 'entityArns', 'organizationEventFilter_entityArns' - A list of entity ARNs (unique identifiers).
--
-- 'eventTypeCodes', 'organizationEventFilter_eventTypeCodes' - A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
--
-- 'endTime', 'organizationEventFilter_endTime' - Undocumented member.
--
-- 'eventStatusCodes', 'organizationEventFilter_eventStatusCodes' - A list of event status codes.
--
-- 'entityValues', 'organizationEventFilter_entityValues' - A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
-- EBS volumes (vol-426ab23e).
--
-- 'regions', 'organizationEventFilter_regions' - A list of AWS Regions.
--
-- 'eventTypeCategories', 'organizationEventFilter_eventTypeCategories' - A list of event type category codes (issue, scheduledChange, or
-- accountNotification).
--
-- 'awsAccountIds', 'organizationEventFilter_awsAccountIds' - A list of 12-digit AWS account numbers that contains the affected
-- entities.
--
-- 'lastUpdatedTime', 'organizationEventFilter_lastUpdatedTime' - Undocumented member.
newOrganizationEventFilter ::
  OrganizationEventFilter
newOrganizationEventFilter =
  OrganizationEventFilter'
    { services =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      entityArns = Prelude.Nothing,
      eventTypeCodes = Prelude.Nothing,
      endTime = Prelude.Nothing,
      eventStatusCodes = Prelude.Nothing,
      entityValues = Prelude.Nothing,
      regions = Prelude.Nothing,
      eventTypeCategories = Prelude.Nothing,
      awsAccountIds = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The AWS services associated with the event. For example, @EC2@, @RDS@.
organizationEventFilter_services :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_services = Lens.lens (\OrganizationEventFilter' {services} -> services) (\s@OrganizationEventFilter' {} a -> s {services = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
organizationEventFilter_startTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_startTime = Lens.lens (\OrganizationEventFilter' {startTime} -> startTime) (\s@OrganizationEventFilter' {} a -> s {startTime = a} :: OrganizationEventFilter)

-- | A list of entity ARNs (unique identifiers).
organizationEventFilter_entityArns :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_entityArns = Lens.lens (\OrganizationEventFilter' {entityArns} -> entityArns) (\s@OrganizationEventFilter' {} a -> s {entityArns = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
organizationEventFilter_eventTypeCodes :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_eventTypeCodes = Lens.lens (\OrganizationEventFilter' {eventTypeCodes} -> eventTypeCodes) (\s@OrganizationEventFilter' {} a -> s {eventTypeCodes = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
organizationEventFilter_endTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_endTime = Lens.lens (\OrganizationEventFilter' {endTime} -> endTime) (\s@OrganizationEventFilter' {} a -> s {endTime = a} :: OrganizationEventFilter)

-- | A list of event status codes.
organizationEventFilter_eventStatusCodes :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty EventStatusCode))
organizationEventFilter_eventStatusCodes = Lens.lens (\OrganizationEventFilter' {eventStatusCodes} -> eventStatusCodes) (\s@OrganizationEventFilter' {} a -> s {eventStatusCodes = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
-- EBS volumes (vol-426ab23e).
organizationEventFilter_entityValues :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_entityValues = Lens.lens (\OrganizationEventFilter' {entityValues} -> entityValues) (\s@OrganizationEventFilter' {} a -> s {entityValues = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | A list of AWS Regions.
organizationEventFilter_regions :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_regions = Lens.lens (\OrganizationEventFilter' {regions} -> regions) (\s@OrganizationEventFilter' {} a -> s {regions = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | A list of event type category codes (issue, scheduledChange, or
-- accountNotification).
organizationEventFilter_eventTypeCategories :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty EventTypeCategory))
organizationEventFilter_eventTypeCategories = Lens.lens (\OrganizationEventFilter' {eventTypeCategories} -> eventTypeCategories) (\s@OrganizationEventFilter' {} a -> s {eventTypeCategories = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | A list of 12-digit AWS account numbers that contains the affected
-- entities.
organizationEventFilter_awsAccountIds :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_awsAccountIds = Lens.lens (\OrganizationEventFilter' {awsAccountIds} -> awsAccountIds) (\s@OrganizationEventFilter' {} a -> s {awsAccountIds = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
organizationEventFilter_lastUpdatedTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_lastUpdatedTime = Lens.lens (\OrganizationEventFilter' {lastUpdatedTime} -> lastUpdatedTime) (\s@OrganizationEventFilter' {} a -> s {lastUpdatedTime = a} :: OrganizationEventFilter)

instance Prelude.Hashable OrganizationEventFilter

instance Prelude.NFData OrganizationEventFilter

instance Core.ToJSON OrganizationEventFilter where
  toJSON OrganizationEventFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("services" Core..=) Prelude.<$> services,
            ("startTime" Core..=) Prelude.<$> startTime,
            ("entityArns" Core..=) Prelude.<$> entityArns,
            ("eventTypeCodes" Core..=)
              Prelude.<$> eventTypeCodes,
            ("endTime" Core..=) Prelude.<$> endTime,
            ("eventStatusCodes" Core..=)
              Prelude.<$> eventStatusCodes,
            ("entityValues" Core..=) Prelude.<$> entityValues,
            ("regions" Core..=) Prelude.<$> regions,
            ("eventTypeCategories" Core..=)
              Prelude.<$> eventTypeCategories,
            ("awsAccountIds" Core..=) Prelude.<$> awsAccountIds,
            ("lastUpdatedTime" Core..=)
              Prelude.<$> lastUpdatedTime
          ]
      )
