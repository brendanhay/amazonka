{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventFilter
  ( EventFilter (..),

    -- * Smart constructor
    mkEventFilter,

    -- * Lenses
    eAvailabilityZones,
    eEndTimes,
    eEntityArns,
    eEntityValues,
    eEventArns,
    eEventStatusCodes,
    eEventTypeCategories,
    eEventTypeCodes,
    eLastUpdatedTimes,
    eRegions,
    eServices,
    eStartTimes,
    eTags,
  )
where

import qualified Network.AWS.AWSHealth.Types.AvailabilityZone as Types
import qualified Network.AWS.AWSHealth.Types.DateTimeRange as Types
import qualified Network.AWS.AWSHealth.Types.EntityArn as Types
import qualified Network.AWS.AWSHealth.Types.EntityValue as Types
import qualified Network.AWS.AWSHealth.Types.EventArn as Types
import qualified Network.AWS.AWSHealth.Types.EventStatusCode as Types
import qualified Network.AWS.AWSHealth.Types.EventType as Types
import qualified Network.AWS.AWSHealth.Types.EventTypeCategory as Types
import qualified Network.AWS.AWSHealth.Types.Region as Types
import qualified Network.AWS.AWSHealth.Types.Service as Types
import qualified Network.AWS.AWSHealth.Types.TagKey as Types
import qualified Network.AWS.AWSHealth.Types.TagValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates> operations.
--
-- /See:/ 'mkEventFilter' smart constructor.
data EventFilter = EventFilter'
  { -- | A list of AWS availability zones.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | A list of dates and times that the event ended.
    endTimes :: Core.Maybe (Core.NonEmpty Types.DateTimeRange),
    -- | A list of entity ARNs (unique identifiers).
    entityArns :: Core.Maybe (Core.NonEmpty Types.EntityArn),
    -- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
    entityValues :: Core.Maybe (Core.NonEmpty Types.EntityValue),
    -- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
    eventArns :: Core.Maybe (Core.NonEmpty Types.EventArn),
    -- | A list of event status codes.
    eventStatusCodes :: Core.Maybe (Core.NonEmpty Types.EventStatusCode),
    -- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
    eventTypeCategories :: Core.Maybe (Core.NonEmpty Types.EventTypeCategory),
    -- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
    eventTypeCodes :: Core.Maybe (Core.NonEmpty Types.EventType),
    -- | A list of dates and times that the event was last updated.
    lastUpdatedTimes :: Core.Maybe (Core.NonEmpty Types.DateTimeRange),
    -- | A list of AWS regions.
    regions :: Core.Maybe (Core.NonEmpty Types.Region),
    -- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
    services :: Core.Maybe (Core.NonEmpty Types.Service),
    -- | A list of dates and times that the event began.
    startTimes :: Core.Maybe (Core.NonEmpty Types.DateTimeRange),
    -- | A map of entity tags attached to the affected entity.
    tags :: Core.Maybe [Core.HashMap Types.TagKey Types.TagValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EventFilter' value with any optional fields omitted.
mkEventFilter ::
  EventFilter
mkEventFilter =
  EventFilter'
    { availabilityZones = Core.Nothing,
      endTimes = Core.Nothing,
      entityArns = Core.Nothing,
      entityValues = Core.Nothing,
      eventArns = Core.Nothing,
      eventStatusCodes = Core.Nothing,
      eventTypeCategories = Core.Nothing,
      eventTypeCodes = Core.Nothing,
      lastUpdatedTimes = Core.Nothing,
      regions = Core.Nothing,
      services = Core.Nothing,
      startTimes = Core.Nothing,
      tags = Core.Nothing
    }

-- | A list of AWS availability zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAvailabilityZones :: Lens.Lens' EventFilter (Core.Maybe [Types.AvailabilityZone])
eAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED eAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of dates and times that the event ended.
--
-- /Note:/ Consider using 'endTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEndTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.DateTimeRange))
eEndTimes = Lens.field @"endTimes"
{-# DEPRECATED eEndTimes "Use generic-lens or generic-optics with 'endTimes' instead." #-}

-- | A list of entity ARNs (unique identifiers).
--
-- /Note:/ Consider using 'entityArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityArns :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EntityArn))
eEntityArns = Lens.field @"entityArns"
{-# DEPRECATED eEntityArns "Use generic-lens or generic-optics with 'entityArns' instead." #-}

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
--
-- /Note:/ Consider using 'entityValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEntityValues :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EntityValue))
eEntityValues = Lens.field @"entityValues"
{-# DEPRECATED eEntityValues "Use generic-lens or generic-optics with 'entityValues' instead." #-}

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- /Note:/ Consider using 'eventArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventArns :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EventArn))
eEventArns = Lens.field @"eventArns"
{-# DEPRECATED eEventArns "Use generic-lens or generic-optics with 'eventArns' instead." #-}

-- | A list of event status codes.
--
-- /Note:/ Consider using 'eventStatusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventStatusCodes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EventStatusCode))
eEventStatusCodes = Lens.field @"eventStatusCodes"
{-# DEPRECATED eEventStatusCodes "Use generic-lens or generic-optics with 'eventStatusCodes' instead." #-}

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'eventTypeCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCategories :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EventTypeCategory))
eEventTypeCategories = Lens.field @"eventTypeCategories"
{-# DEPRECATED eEventTypeCategories "Use generic-lens or generic-optics with 'eventTypeCategories' instead." #-}

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
--
-- /Note:/ Consider using 'eventTypeCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eEventTypeCodes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.EventType))
eEventTypeCodes = Lens.field @"eventTypeCodes"
{-# DEPRECATED eEventTypeCodes "Use generic-lens or generic-optics with 'eventTypeCodes' instead." #-}

-- | A list of dates and times that the event was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eLastUpdatedTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.DateTimeRange))
eLastUpdatedTimes = Lens.field @"lastUpdatedTimes"
{-# DEPRECATED eLastUpdatedTimes "Use generic-lens or generic-optics with 'lastUpdatedTimes' instead." #-}

-- | A list of AWS regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRegions :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.Region))
eRegions = Lens.field @"regions"
{-# DEPRECATED eRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eServices :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.Service))
eServices = Lens.field @"services"
{-# DEPRECATED eServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | A list of dates and times that the event began.
--
-- /Note:/ Consider using 'startTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eStartTimes :: Lens.Lens' EventFilter (Core.Maybe (Core.NonEmpty Types.DateTimeRange))
eStartTimes = Lens.field @"startTimes"
{-# DEPRECATED eStartTimes "Use generic-lens or generic-optics with 'startTimes' instead." #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' EventFilter (Core.Maybe [Core.HashMap Types.TagKey Types.TagValue])
eTags = Lens.field @"tags"
{-# DEPRECATED eTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON EventFilter where
  toJSON EventFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("availabilityZones" Core..=) Core.<$> availabilityZones,
            ("endTimes" Core..=) Core.<$> endTimes,
            ("entityArns" Core..=) Core.<$> entityArns,
            ("entityValues" Core..=) Core.<$> entityValues,
            ("eventArns" Core..=) Core.<$> eventArns,
            ("eventStatusCodes" Core..=) Core.<$> eventStatusCodes,
            ("eventTypeCategories" Core..=) Core.<$> eventTypeCategories,
            ("eventTypeCodes" Core..=) Core.<$> eventTypeCodes,
            ("lastUpdatedTimes" Core..=) Core.<$> lastUpdatedTimes,
            ("regions" Core..=) Core.<$> regions,
            ("services" Core..=) Core.<$> services,
            ("startTimes" Core..=) Core.<$> startTimes,
            ("tags" Core..=) Core.<$> tags
          ]
      )
