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
    efEventARNs,
    efEventTypeCategories,
    efEventTypeCodes,
    efRegions,
    efEventStatusCodes,
    efEndTimes,
    efAvailabilityZones,
    efEntityARNs,
    efEntityValues,
    efStartTimes,
    efServices,
    efTags,
    efLastUpdatedTimes,
  )
where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values to use to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEvents.html DescribeEvents> and <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventAggregates.html DescribeEventAggregates> operations.
--
-- /See:/ 'mkEventFilter' smart constructor.
data EventFilter = EventFilter'
  { eventARNs ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    eventTypeCategories ::
      Lude.Maybe (Lude.NonEmpty EventTypeCategory),
    eventTypeCodes :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    regions :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    eventStatusCodes :: Lude.Maybe (Lude.NonEmpty EventStatusCode),
    endTimes :: Lude.Maybe (Lude.NonEmpty DateTimeRange),
    availabilityZones :: Lude.Maybe [Lude.Text],
    entityARNs :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    entityValues :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    startTimes :: Lude.Maybe (Lude.NonEmpty DateTimeRange),
    services :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    tags :: Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)],
    lastUpdatedTimes :: Lude.Maybe (Lude.NonEmpty DateTimeRange)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventFilter' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - A list of AWS availability zones.
-- * 'endTimes' - A list of dates and times that the event ended.
-- * 'entityARNs' - A list of entity ARNs (unique identifiers).
-- * 'entityValues' - A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
-- * 'eventARNs' - A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
-- * 'eventStatusCodes' - A list of event status codes.
-- * 'eventTypeCategories' - A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
-- * 'eventTypeCodes' - A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
-- * 'lastUpdatedTimes' - A list of dates and times that the event was last updated.
-- * 'regions' - A list of AWS regions.
-- * 'services' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
-- * 'startTimes' - A list of dates and times that the event began.
-- * 'tags' - A map of entity tags attached to the affected entity.
mkEventFilter ::
  EventFilter
mkEventFilter =
  EventFilter'
    { eventARNs = Lude.Nothing,
      eventTypeCategories = Lude.Nothing,
      eventTypeCodes = Lude.Nothing,
      regions = Lude.Nothing,
      eventStatusCodes = Lude.Nothing,
      endTimes = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      entityARNs = Lude.Nothing,
      entityValues = Lude.Nothing,
      startTimes = Lude.Nothing,
      services = Lude.Nothing,
      tags = Lude.Nothing,
      lastUpdatedTimes = Lude.Nothing
    }

-- | A list of event ARNs (unique identifiers). For example: @"arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456", "arn:aws:health:us-west-1::event/EBS/AWS_EBS_LOST_VOLUME/AWS_EBS_LOST_VOLUME_CHI789_JKL101"@
--
-- /Note:/ Consider using 'eventARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEventARNs :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efEventARNs = Lens.lens (eventARNs :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {eventARNs = a} :: EventFilter)
{-# DEPRECATED efEventARNs "Use generic-lens or generic-optics with 'eventARNs' instead." #-}

-- | A list of event type category codes (@issue@ , @scheduledChange@ , or @accountNotification@ ).
--
-- /Note:/ Consider using 'eventTypeCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEventTypeCategories :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty EventTypeCategory))
efEventTypeCategories = Lens.lens (eventTypeCategories :: EventFilter -> Lude.Maybe (Lude.NonEmpty EventTypeCategory)) (\s a -> s {eventTypeCategories = a} :: EventFilter)
{-# DEPRECATED efEventTypeCategories "Use generic-lens or generic-optics with 'eventTypeCategories' instead." #-}

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
--
-- /Note:/ Consider using 'eventTypeCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEventTypeCodes :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efEventTypeCodes = Lens.lens (eventTypeCodes :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {eventTypeCodes = a} :: EventFilter)
{-# DEPRECATED efEventTypeCodes "Use generic-lens or generic-optics with 'eventTypeCodes' instead." #-}

-- | A list of AWS regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efRegions :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efRegions = Lens.lens (regions :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {regions = a} :: EventFilter)
{-# DEPRECATED efRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | A list of event status codes.
--
-- /Note:/ Consider using 'eventStatusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEventStatusCodes :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty EventStatusCode))
efEventStatusCodes = Lens.lens (eventStatusCodes :: EventFilter -> Lude.Maybe (Lude.NonEmpty EventStatusCode)) (\s a -> s {eventStatusCodes = a} :: EventFilter)
{-# DEPRECATED efEventStatusCodes "Use generic-lens or generic-optics with 'eventStatusCodes' instead." #-}

-- | A list of dates and times that the event ended.
--
-- /Note:/ Consider using 'endTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEndTimes :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty DateTimeRange))
efEndTimes = Lens.lens (endTimes :: EventFilter -> Lude.Maybe (Lude.NonEmpty DateTimeRange)) (\s a -> s {endTimes = a} :: EventFilter)
{-# DEPRECATED efEndTimes "Use generic-lens or generic-optics with 'endTimes' instead." #-}

-- | A list of AWS availability zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efAvailabilityZones :: Lens.Lens' EventFilter (Lude.Maybe [Lude.Text])
efAvailabilityZones = Lens.lens (availabilityZones :: EventFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: EventFilter)
{-# DEPRECATED efAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of entity ARNs (unique identifiers).
--
-- /Note:/ Consider using 'entityARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEntityARNs :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efEntityARNs = Lens.lens (entityARNs :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityARNs = a} :: EventFilter)
{-# DEPRECATED efEntityARNs "Use generic-lens or generic-optics with 'entityARNs' instead." #-}

-- | A list of entity identifiers, such as EC2 instance IDs (@i-34ab692e@ ) or EBS volumes (@vol-426ab23e@ ).
--
-- /Note:/ Consider using 'entityValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efEntityValues :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efEntityValues = Lens.lens (entityValues :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityValues = a} :: EventFilter)
{-# DEPRECATED efEntityValues "Use generic-lens or generic-optics with 'entityValues' instead." #-}

-- | A list of dates and times that the event began.
--
-- /Note:/ Consider using 'startTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efStartTimes :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty DateTimeRange))
efStartTimes = Lens.lens (startTimes :: EventFilter -> Lude.Maybe (Lude.NonEmpty DateTimeRange)) (\s a -> s {startTimes = a} :: EventFilter)
{-# DEPRECATED efStartTimes "Use generic-lens or generic-optics with 'startTimes' instead." #-}

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efServices :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
efServices = Lens.lens (services :: EventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {services = a} :: EventFilter)
{-# DEPRECATED efServices "Use generic-lens or generic-optics with 'services' instead." #-}

-- | A map of entity tags attached to the affected entity.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efTags :: Lens.Lens' EventFilter (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
efTags = Lens.lens (tags :: EventFilter -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {tags = a} :: EventFilter)
{-# DEPRECATED efTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of dates and times that the event was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efLastUpdatedTimes :: Lens.Lens' EventFilter (Lude.Maybe (Lude.NonEmpty DateTimeRange))
efLastUpdatedTimes = Lens.lens (lastUpdatedTimes :: EventFilter -> Lude.Maybe (Lude.NonEmpty DateTimeRange)) (\s a -> s {lastUpdatedTimes = a} :: EventFilter)
{-# DEPRECATED efLastUpdatedTimes "Use generic-lens or generic-optics with 'lastUpdatedTimes' instead." #-}

instance Lude.ToJSON EventFilter where
  toJSON EventFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("eventArns" Lude..=) Lude.<$> eventARNs,
            ("eventTypeCategories" Lude..=) Lude.<$> eventTypeCategories,
            ("eventTypeCodes" Lude..=) Lude.<$> eventTypeCodes,
            ("regions" Lude..=) Lude.<$> regions,
            ("eventStatusCodes" Lude..=) Lude.<$> eventStatusCodes,
            ("endTimes" Lude..=) Lude.<$> endTimes,
            ("availabilityZones" Lude..=) Lude.<$> availabilityZones,
            ("entityArns" Lude..=) Lude.<$> entityARNs,
            ("entityValues" Lude..=) Lude.<$> entityValues,
            ("startTimes" Lude..=) Lude.<$> startTimes,
            ("services" Lude..=) Lude.<$> services,
            ("tags" Lude..=) Lude.<$> tags,
            ("lastUpdatedTimes" Lude..=) Lude.<$> lastUpdatedTimes
          ]
      )
