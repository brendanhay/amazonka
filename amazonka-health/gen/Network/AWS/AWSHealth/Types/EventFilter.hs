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
-- Module      : Network.AWS.AWSHealth.Types.EventFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventFilter where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The values to use to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents>
-- and
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates>
-- operations.
--
-- /See:/ 'newEventFilter' smart constructor.
data EventFilter = EventFilter'
  { -- | A list of AWS availability zones.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | A list of dates and times that the event ended.
    endTimes :: Core.Maybe (Core.NonEmpty DateTimeRange),
    -- | A list of dates and times that the event began.
    startTimes :: Core.Maybe (Core.NonEmpty DateTimeRange),
    -- | The AWS services associated with the event. For example, @EC2@, @RDS@.
    services :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of entity ARNs (unique identifiers).
    entityArns :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of unique identifiers for event types. For example,
    -- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
    eventTypeCodes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of dates and times that the event was last updated.
    lastUpdatedTimes :: Core.Maybe (Core.NonEmpty DateTimeRange),
    -- | A map of entity tags attached to the affected entity.
    --
    -- Currently, the @tags@ property isn\'t supported.
    tags :: Core.Maybe [Core.HashMap Core.Text Core.Text],
    -- | A list of event status codes.
    eventStatusCodes :: Core.Maybe (Core.NonEmpty EventStatusCode),
    -- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
    -- EBS volumes (@vol-426ab23e@).
    entityValues :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of AWS regions.
    regions :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of event type category codes (@issue@, @scheduledChange@, or
    -- @accountNotification@).
    eventTypeCategories :: Core.Maybe (Core.NonEmpty EventTypeCategory)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'eventFilter_availabilityZones' - A list of AWS availability zones.
--
-- 'endTimes', 'eventFilter_endTimes' - A list of dates and times that the event ended.
--
-- 'startTimes', 'eventFilter_startTimes' - A list of dates and times that the event began.
--
-- 'services', 'eventFilter_services' - The AWS services associated with the event. For example, @EC2@, @RDS@.
--
-- 'entityArns', 'eventFilter_entityArns' - A list of entity ARNs (unique identifiers).
--
-- 'eventTypeCodes', 'eventFilter_eventTypeCodes' - A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
--
-- 'lastUpdatedTimes', 'eventFilter_lastUpdatedTimes' - A list of dates and times that the event was last updated.
--
-- 'tags', 'eventFilter_tags' - A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
--
-- 'eventStatusCodes', 'eventFilter_eventStatusCodes' - A list of event status codes.
--
-- 'entityValues', 'eventFilter_entityValues' - A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
-- EBS volumes (@vol-426ab23e@).
--
-- 'regions', 'eventFilter_regions' - A list of AWS regions.
--
-- 'eventArns', 'eventFilter_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
--
-- 'eventTypeCategories', 'eventFilter_eventTypeCategories' - A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
newEventFilter ::
  EventFilter
newEventFilter =
  EventFilter'
    { availabilityZones = Core.Nothing,
      endTimes = Core.Nothing,
      startTimes = Core.Nothing,
      services = Core.Nothing,
      entityArns = Core.Nothing,
      eventTypeCodes = Core.Nothing,
      lastUpdatedTimes = Core.Nothing,
      tags = Core.Nothing,
      eventStatusCodes = Core.Nothing,
      entityValues = Core.Nothing,
      regions = Core.Nothing,
      eventArns = Core.Nothing,
      eventTypeCategories = Core.Nothing
    }

-- | A list of AWS availability zones.
eventFilter_availabilityZones :: Lens.Lens' EventFilter (Core.Maybe [Core.Text])
eventFilter_availabilityZones = Lens.lens (\EventFilter' {availabilityZones} -> availabilityZones) (\s@EventFilter' {} a -> s {availabilityZones = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of dates and times that the event ended.
eventFilter_endTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty DateTimeRange))
eventFilter_endTimes = Lens.lens (\EventFilter' {endTimes} -> endTimes) (\s@EventFilter' {} a -> s {endTimes = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of dates and times that the event began.
eventFilter_startTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty DateTimeRange))
eventFilter_startTimes = Lens.lens (\EventFilter' {startTimes} -> startTimes) (\s@EventFilter' {} a -> s {startTimes = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | The AWS services associated with the event. For example, @EC2@, @RDS@.
eventFilter_services :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_services = Lens.lens (\EventFilter' {services} -> services) (\s@EventFilter' {} a -> s {services = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of entity ARNs (unique identifiers).
eventFilter_entityArns :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_entityArns = Lens.lens (\EventFilter' {entityArns} -> entityArns) (\s@EventFilter' {} a -> s {entityArns = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of unique identifiers for event types. For example,
-- @\"AWS_EC2_SYSTEM_MAINTENANCE_EVENT\",\"AWS_RDS_MAINTENANCE_SCHEDULED\".@
eventFilter_eventTypeCodes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_eventTypeCodes = Lens.lens (\EventFilter' {eventTypeCodes} -> eventTypeCodes) (\s@EventFilter' {} a -> s {eventTypeCodes = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of dates and times that the event was last updated.
eventFilter_lastUpdatedTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty DateTimeRange))
eventFilter_lastUpdatedTimes = Lens.lens (\EventFilter' {lastUpdatedTimes} -> lastUpdatedTimes) (\s@EventFilter' {} a -> s {lastUpdatedTimes = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A map of entity tags attached to the affected entity.
--
-- Currently, the @tags@ property isn\'t supported.
eventFilter_tags :: Lens.Lens' EventFilter (Core.Maybe [Core.HashMap Core.Text Core.Text])
eventFilter_tags = Lens.lens (\EventFilter' {tags} -> tags) (\s@EventFilter' {} a -> s {tags = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event status codes.
eventFilter_eventStatusCodes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty EventStatusCode))
eventFilter_eventStatusCodes = Lens.lens (\EventFilter' {eventStatusCodes} -> eventStatusCodes) (\s@EventFilter' {} a -> s {eventStatusCodes = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@) or
-- EBS volumes (@vol-426ab23e@).
eventFilter_entityValues :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_entityValues = Lens.lens (\EventFilter' {entityValues} -> entityValues) (\s@EventFilter' {} a -> s {entityValues = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of AWS regions.
eventFilter_regions :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_regions = Lens.lens (\EventFilter' {regions} -> regions) (\s@EventFilter' {} a -> s {regions = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
eventFilter_eventArns :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Core.Text))
eventFilter_eventArns = Lens.lens (\EventFilter' {eventArns} -> eventArns) (\s@EventFilter' {} a -> s {eventArns = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

-- | A list of event type category codes (@issue@, @scheduledChange@, or
-- @accountNotification@).
eventFilter_eventTypeCategories :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty EventTypeCategory))
eventFilter_eventTypeCategories = Lens.lens (\EventFilter' {eventTypeCategories} -> eventTypeCategories) (\s@EventFilter' {} a -> s {eventTypeCategories = a} :: EventFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable EventFilter

instance Core.NFData EventFilter

instance Core.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("availabilityZones" Core..=)
              Core.<$> availabilityZones,
            ("endTimes" Core..=) Core.<$> endTimes,
            ("startTimes" Core..=) Core.<$> startTimes,
            ("services" Core..=) Core.<$> services,
            ("entityArns" Core..=) Core.<$> entityArns,
            ("eventTypeCodes" Core..=) Core.<$> eventTypeCodes,
            ("lastUpdatedTimes" Core..=)
              Core.<$> lastUpdatedTimes,
            ("tags" Core..=) Core.<$> tags,
            ("eventStatusCodes" Core..=)
              Core.<$> eventStatusCodes,
            ("entityValues" Core..=) Core.<$> entityValues,
            ("regions" Core..=) Core.<$> regions,
            ("eventArns" Core..=) Core.<$> eventArns,
            ("eventTypeCategories" Core..=)
              Core.<$> eventTypeCategories
          ]
      )
