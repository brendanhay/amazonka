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
-- Module      : Amazonka.AWSHealth.Types.OrganizationEventFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.OrganizationEventFilter where

import Amazonka.AWSHealth.Types.DateTimeRange
import Amazonka.AWSHealth.Types.EventStatusCode
import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization>
-- operation.
--
-- /See:/ 'newOrganizationEventFilter' smart constructor.
data OrganizationEventFilter = OrganizationEventFilter'
  { -- | A list of 12-digit Amazon Web Services account numbers that contains the
    -- affected entities.
    awsAccountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of entity ARNs (unique identifiers).
    entityArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of Amazon Web Services Regions.
    regions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    lastUpdatedTime :: Prelude.Maybe DateTimeRange,
    endTime :: Prelude.Maybe DateTimeRange,
    -- | The Amazon Web Services services associated with the event. For example,
    -- @EC2@, @RDS@.
    services :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of event status codes.
    eventStatusCodes :: Prelude.Maybe (Prelude.NonEmpty EventStatusCode),
    -- | A list of unique identifiers for event types. For example,
    -- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
    eventTypeCodes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
    -- EBS volumes (vol-426ab23e).
    entityValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of event type category codes. Possible values are @issue@,
    -- @accountNotification@, or @scheduledChange@. Currently, the
    -- @investigation@ value isn\'t supported at this time.
    eventTypeCategories :: Prelude.Maybe (Prelude.NonEmpty EventTypeCategory),
    startTime :: Prelude.Maybe DateTimeRange
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
-- 'awsAccountIds', 'organizationEventFilter_awsAccountIds' - A list of 12-digit Amazon Web Services account numbers that contains the
-- affected entities.
--
-- 'entityArns', 'organizationEventFilter_entityArns' - A list of entity ARNs (unique identifiers).
--
-- 'regions', 'organizationEventFilter_regions' - A list of Amazon Web Services Regions.
--
-- 'lastUpdatedTime', 'organizationEventFilter_lastUpdatedTime' - Undocumented member.
--
-- 'endTime', 'organizationEventFilter_endTime' - Undocumented member.
--
-- 'services', 'organizationEventFilter_services' - The Amazon Web Services services associated with the event. For example,
-- @EC2@, @RDS@.
--
-- 'eventStatusCodes', 'organizationEventFilter_eventStatusCodes' - A list of event status codes.
--
-- 'eventTypeCodes', 'organizationEventFilter_eventTypeCodes' - A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
--
-- 'entityValues', 'organizationEventFilter_entityValues' - A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
-- EBS volumes (vol-426ab23e).
--
-- 'eventTypeCategories', 'organizationEventFilter_eventTypeCategories' - A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
--
-- 'startTime', 'organizationEventFilter_startTime' - Undocumented member.
newOrganizationEventFilter ::
  OrganizationEventFilter
newOrganizationEventFilter =
  OrganizationEventFilter'
    { awsAccountIds =
        Prelude.Nothing,
      entityArns = Prelude.Nothing,
      regions = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      services = Prelude.Nothing,
      eventStatusCodes = Prelude.Nothing,
      eventTypeCodes = Prelude.Nothing,
      entityValues = Prelude.Nothing,
      eventTypeCategories = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | A list of 12-digit Amazon Web Services account numbers that contains the
-- affected entities.
organizationEventFilter_awsAccountIds :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_awsAccountIds = Lens.lens (\OrganizationEventFilter' {awsAccountIds} -> awsAccountIds) (\s@OrganizationEventFilter' {} a -> s {awsAccountIds = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of entity ARNs (unique identifiers).
organizationEventFilter_entityArns :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_entityArns = Lens.lens (\OrganizationEventFilter' {entityArns} -> entityArns) (\s@OrganizationEventFilter' {} a -> s {entityArns = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Web Services Regions.
organizationEventFilter_regions :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_regions = Lens.lens (\OrganizationEventFilter' {regions} -> regions) (\s@OrganizationEventFilter' {} a -> s {regions = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
organizationEventFilter_lastUpdatedTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_lastUpdatedTime = Lens.lens (\OrganizationEventFilter' {lastUpdatedTime} -> lastUpdatedTime) (\s@OrganizationEventFilter' {} a -> s {lastUpdatedTime = a} :: OrganizationEventFilter)

-- | Undocumented member.
organizationEventFilter_endTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_endTime = Lens.lens (\OrganizationEventFilter' {endTime} -> endTime) (\s@OrganizationEventFilter' {} a -> s {endTime = a} :: OrganizationEventFilter)

-- | The Amazon Web Services services associated with the event. For example,
-- @EC2@, @RDS@.
organizationEventFilter_services :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_services = Lens.lens (\OrganizationEventFilter' {services} -> services) (\s@OrganizationEventFilter' {} a -> s {services = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of event status codes.
organizationEventFilter_eventStatusCodes :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty EventStatusCode))
organizationEventFilter_eventStatusCodes = Lens.lens (\OrganizationEventFilter' {eventStatusCodes} -> eventStatusCodes) (\s@OrganizationEventFilter' {} a -> s {eventStatusCodes = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
organizationEventFilter_eventTypeCodes :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_eventTypeCodes = Lens.lens (\OrganizationEventFilter' {eventTypeCodes} -> eventTypeCodes) (\s@OrganizationEventFilter' {} a -> s {eventTypeCodes = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or
-- EBS volumes (vol-426ab23e).
organizationEventFilter_entityValues :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationEventFilter_entityValues = Lens.lens (\OrganizationEventFilter' {entityValues} -> entityValues) (\s@OrganizationEventFilter' {} a -> s {entityValues = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of event type category codes. Possible values are @issue@,
-- @accountNotification@, or @scheduledChange@. Currently, the
-- @investigation@ value isn\'t supported at this time.
organizationEventFilter_eventTypeCategories :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe (Prelude.NonEmpty EventTypeCategory))
organizationEventFilter_eventTypeCategories = Lens.lens (\OrganizationEventFilter' {eventTypeCategories} -> eventTypeCategories) (\s@OrganizationEventFilter' {} a -> s {eventTypeCategories = a} :: OrganizationEventFilter) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
organizationEventFilter_startTime :: Lens.Lens' OrganizationEventFilter (Prelude.Maybe DateTimeRange)
organizationEventFilter_startTime = Lens.lens (\OrganizationEventFilter' {startTime} -> startTime) (\s@OrganizationEventFilter' {} a -> s {startTime = a} :: OrganizationEventFilter)

instance Prelude.Hashable OrganizationEventFilter where
  hashWithSalt _salt OrganizationEventFilter' {..} =
    _salt `Prelude.hashWithSalt` awsAccountIds
      `Prelude.hashWithSalt` entityArns
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` services
      `Prelude.hashWithSalt` eventStatusCodes
      `Prelude.hashWithSalt` eventTypeCodes
      `Prelude.hashWithSalt` entityValues
      `Prelude.hashWithSalt` eventTypeCategories
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData OrganizationEventFilter where
  rnf OrganizationEventFilter' {..} =
    Prelude.rnf awsAccountIds
      `Prelude.seq` Prelude.rnf entityArns
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf eventStatusCodes
      `Prelude.seq` Prelude.rnf eventTypeCodes
      `Prelude.seq` Prelude.rnf entityValues
      `Prelude.seq` Prelude.rnf eventTypeCategories
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON OrganizationEventFilter where
  toJSON OrganizationEventFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccountIds" Data..=) Prelude.<$> awsAccountIds,
            ("entityArns" Data..=) Prelude.<$> entityArns,
            ("regions" Data..=) Prelude.<$> regions,
            ("lastUpdatedTime" Data..=)
              Prelude.<$> lastUpdatedTime,
            ("endTime" Data..=) Prelude.<$> endTime,
            ("services" Data..=) Prelude.<$> services,
            ("eventStatusCodes" Data..=)
              Prelude.<$> eventStatusCodes,
            ("eventTypeCodes" Data..=)
              Prelude.<$> eventTypeCodes,
            ("entityValues" Data..=) Prelude.<$> entityValues,
            ("eventTypeCategories" Data..=)
              Prelude.<$> eventTypeCategories,
            ("startTime" Data..=) Prelude.<$> startTime
          ]
      )
