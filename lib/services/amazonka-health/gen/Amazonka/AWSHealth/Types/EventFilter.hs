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
-- Module      : Amazonka.AWSHealth.Types.EventFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.EventFilter where

import Amazonka.AWSHealth.Types.DateTimeRange
import Amazonka.AWSHealth.Types.EventStatusCode
import Amazonka.AWSHealth.Types.EventTypeCategory
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The values to use to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents>
-- and
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates>
-- operations.
--
-- /See:/ 'newEventFilter' smart constructor.
data EventFilter = EventFilter'
  { -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of event type category codes (@issue@, @scheduledChange@, or
    -- @accountNotification@).
    eventTypeCategories :: Prelude.Maybe (Prelude.NonEmpty EventTypeCategory),
    -- | A list of unique identifiers for event types. For example,
    -- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
    eventTypeCodes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of AWS Regions.
    regions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of event status codes.
    eventStatusCodes :: Prelude.Maybe (Prelude.NonEmpty EventStatusCode),
    -- | A list of dates and times that the event ended.
    endTimes :: Prelude.Maybe (Prelude.NonEmpty DateTimeRange),
    -- | A list of AWS Availability Zones.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A list of entity ARNs (unique identifiers).
    entityArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
    -- EBS volumes (@vol-426ab23e@).
    entityValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of dates and times that the event began.
    startTimes :: Prelude.Maybe (Prelude.NonEmpty DateTimeRange),
    -- | The AWS services associated with the event. For example, @EC2@, @RDS@.
    services :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A map of entity tags attached to the affected entity.
    --
    -- Currently, the @tags@ property isn\'t supported.
    tags :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | A list of dates and times that the event was last updated.
    lastUpdatedTimes :: Prelude.Maybe (Prelude.NonEmpty DateTimeRange)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventArns', 'eventFilter_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
--
-- 'eventTypeCategories', 'eventFilter_eventTypeCategories' - A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
--
-- 'eventTypeCodes', 'eventFilter_eventTypeCodes' - A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
--
-- 'regions', 'eventFilter_regions' - A list of AWS Regions.
--
-- 'eventStatusCodes', 'eventFilter_eventStatusCodes' - A list of event status codes.
--
-- 'endTimes', 'eventFilter_endTimes' - A list of dates and times that the event ended.
--
-- 'availabilityZones', 'eventFilter_availabilityZones' - A list of AWS Availability Zones.
--
-- 'entityArns', 'eventFilter_entityArns' - A list of entity ARNs (unique identifiers).
--
-- 'entityValues', 'eventFilter_entityValues' - A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
-- EBS volumes (@vol-426ab23e@).
--
-- 'startTimes', 'eventFilter_startTimes' - A list of dates and times that the event began.
--
-- 'services', 'eventFilter_services' - The AWS services associated with the event. For example, @EC2@, @RDS@.
--
-- 'tags', 'eventFilter_tags' - A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
--
-- 'lastUpdatedTimes', 'eventFilter_lastUpdatedTimes' - A list of dates and times that the event was last updated.
newEventFilter ::
  EventFilter
newEventFilter =
  EventFilter'
    { eventArns = Prelude.Nothing,
      eventTypeCategories = Prelude.Nothing,
      eventTypeCodes = Prelude.Nothing,
      regions = Prelude.Nothing,
      eventStatusCodes = Prelude.Nothing,
      endTimes = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      entityArns = Prelude.Nothing,
      entityValues = Prelude.Nothing,
      startTimes = Prelude.Nothing,
      services = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastUpdatedTimes = Prelude.Nothing
    }

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
eventFilter_eventArns :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_eventArns = Lens.lens (\EventFilter' {eventArns} -> eventArns) (\s@EventFilter' {} a -> s {eventArns = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
eventFilter_eventTypeCategories :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty EventTypeCategory))
eventFilter_eventTypeCategories = Lens.lens (\EventFilter' {eventTypeCategories} -> eventTypeCategories) (\s@EventFilter' {} a -> s {eventTypeCategories = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
eventFilter_eventTypeCodes :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_eventTypeCodes = Lens.lens (\EventFilter' {eventTypeCodes} -> eventTypeCodes) (\s@EventFilter' {} a -> s {eventTypeCodes = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of AWS Regions.
eventFilter_regions :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_regions = Lens.lens (\EventFilter' {regions} -> regions) (\s@EventFilter' {} a -> s {regions = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of event status codes.
eventFilter_eventStatusCodes :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty EventStatusCode))
eventFilter_eventStatusCodes = Lens.lens (\EventFilter' {eventStatusCodes} -> eventStatusCodes) (\s@EventFilter' {} a -> s {eventStatusCodes = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of dates and times that the event ended.
eventFilter_endTimes :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty DateTimeRange))
eventFilter_endTimes = Lens.lens (\EventFilter' {endTimes} -> endTimes) (\s@EventFilter' {} a -> s {endTimes = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of AWS Availability Zones.
eventFilter_availabilityZones :: Lens.Lens' EventFilter (Prelude.Maybe [Prelude.Text])
eventFilter_availabilityZones = Lens.lens (\EventFilter' {availabilityZones} -> availabilityZones) (\s@EventFilter' {} a -> s {availabilityZones = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of entity ARNs (unique identifiers).
eventFilter_entityArns :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_entityArns = Lens.lens (\EventFilter' {entityArns} -> entityArns) (\s@EventFilter' {} a -> s {entityArns = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
-- EBS volumes (@vol-426ab23e@).
eventFilter_entityValues :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_entityValues = Lens.lens (\EventFilter' {entityValues} -> entityValues) (\s@EventFilter' {} a -> s {entityValues = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of dates and times that the event began.
eventFilter_startTimes :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty DateTimeRange))
eventFilter_startTimes = Lens.lens (\EventFilter' {startTimes} -> startTimes) (\s@EventFilter' {} a -> s {startTimes = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | The AWS services associated with the event. For example, @EC2@, @RDS@.
eventFilter_services :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventFilter_services = Lens.lens (\EventFilter' {services} -> services) (\s@EventFilter' {} a -> s {services = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
eventFilter_tags :: Lens.Lens' EventFilter (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
eventFilter_tags = Lens.lens (\EventFilter' {tags} -> tags) (\s@EventFilter' {} a -> s {tags = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

-- | A list of dates and times that the event was last updated.
eventFilter_lastUpdatedTimes :: Lens.Lens' EventFilter (Prelude.Maybe (Prelude.NonEmpty DateTimeRange))
eventFilter_lastUpdatedTimes = Lens.lens (\EventFilter' {lastUpdatedTimes} -> lastUpdatedTimes) (\s@EventFilter' {} a -> s {lastUpdatedTimes = a} :: EventFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EventFilter where
  hashWithSalt salt' EventFilter' {..} =
    salt' `Prelude.hashWithSalt` lastUpdatedTimes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` services
      `Prelude.hashWithSalt` startTimes
      `Prelude.hashWithSalt` entityValues
      `Prelude.hashWithSalt` entityArns
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` endTimes
      `Prelude.hashWithSalt` eventStatusCodes
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` eventTypeCodes
      `Prelude.hashWithSalt` eventTypeCategories
      `Prelude.hashWithSalt` eventArns

instance Prelude.NFData EventFilter where
  rnf EventFilter' {..} =
    Prelude.rnf eventArns
      `Prelude.seq` Prelude.rnf lastUpdatedTimes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf services
      `Prelude.seq` Prelude.rnf startTimes
      `Prelude.seq` Prelude.rnf entityValues
      `Prelude.seq` Prelude.rnf entityArns
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf endTimes
      `Prelude.seq` Prelude.rnf eventStatusCodes
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf eventTypeCodes
      `Prelude.seq` Prelude.rnf eventTypeCategories

instance Core.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("eventArns" Core..=) Prelude.<$> eventArns,
            ("eventTypeCategories" Core..=)
              Prelude.<$> eventTypeCategories,
            ("eventTypeCodes" Core..=)
              Prelude.<$> eventTypeCodes,
            ("regions" Core..=) Prelude.<$> regions,
            ("eventStatusCodes" Core..=)
              Prelude.<$> eventStatusCodes,
            ("endTimes" Core..=) Prelude.<$> endTimes,
            ("availabilityZones" Core..=)
              Prelude.<$> availabilityZones,
            ("entityArns" Core..=) Prelude.<$> entityArns,
            ("entityValues" Core..=) Prelude.<$> entityValues,
            ("startTimes" Core..=) Prelude.<$> startTimes,
            ("services" Core..=) Prelude.<$> services,
            ("tags" Core..=) Prelude.<$> tags,
            ("lastUpdatedTimes" Core..=)
              Prelude.<$> lastUpdatedTimes
          ]
      )
