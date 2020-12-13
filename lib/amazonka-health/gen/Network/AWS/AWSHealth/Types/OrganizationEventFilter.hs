{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventFilter
  ( OrganizationEventFilter (..),

    -- * Smart constructor
    mkOrganizationEventFilter,

    -- * Lenses
    oefLastUpdatedTime,
    oefAwsAccountIds,
    oefEventTypeCategories,
    oefEventTypeCodes,
    oefStartTime,
    oefRegions,
    oefEventStatusCodes,
    oefEndTime,
    oefEntityARNs,
    oefEntityValues,
    oefServices,
  )
where

import Network.AWS.AWSHealth.Types.DateTimeRange
import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values to filter results from the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventsForOrganization.html DescribeEventsForOrganization> operation.
--
-- /See:/ 'mkOrganizationEventFilter' smart constructor.
data OrganizationEventFilter = OrganizationEventFilter'
  { lastUpdatedTime :: Lude.Maybe DateTimeRange,
    -- | A list of 12-digit AWS account numbers that contains the affected entities.
    awsAccountIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of event type category codes (issue, scheduledChange, or accountNotification).
    eventTypeCategories :: Lude.Maybe (Lude.NonEmpty EventTypeCategory),
    -- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
    eventTypeCodes :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    startTime :: Lude.Maybe DateTimeRange,
    -- | A list of AWS Regions.
    regions :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of event status codes.
    eventStatusCodes :: Lude.Maybe (Lude.NonEmpty EventStatusCode),
    endTime :: Lude.Maybe DateTimeRange,
    -- | A list of entity ARNs (unique identifiers).
    entityARNs :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or EBS volumes (vol-426ab23e).
    entityValues :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
    services :: Lude.Maybe (Lude.NonEmpty Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationEventFilter' with the minimum fields required to make a request.
--
-- * 'lastUpdatedTime' -
-- * 'awsAccountIds' - A list of 12-digit AWS account numbers that contains the affected entities.
-- * 'eventTypeCategories' - A list of event type category codes (issue, scheduledChange, or accountNotification).
-- * 'eventTypeCodes' - A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
-- * 'startTime' -
-- * 'regions' - A list of AWS Regions.
-- * 'eventStatusCodes' - A list of event status codes.
-- * 'endTime' -
-- * 'entityARNs' - A list of entity ARNs (unique identifiers).
-- * 'entityValues' - A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or EBS volumes (vol-426ab23e).
-- * 'services' - The AWS services associated with the event. For example, @EC2@ , @RDS@ .
mkOrganizationEventFilter ::
  OrganizationEventFilter
mkOrganizationEventFilter =
  OrganizationEventFilter'
    { lastUpdatedTime = Lude.Nothing,
      awsAccountIds = Lude.Nothing,
      eventTypeCategories = Lude.Nothing,
      eventTypeCodes = Lude.Nothing,
      startTime = Lude.Nothing,
      regions = Lude.Nothing,
      eventStatusCodes = Lude.Nothing,
      endTime = Lude.Nothing,
      entityARNs = Lude.Nothing,
      entityValues = Lude.Nothing,
      services = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefLastUpdatedTime :: Lens.Lens' OrganizationEventFilter (Lude.Maybe DateTimeRange)
oefLastUpdatedTime = Lens.lens (lastUpdatedTime :: OrganizationEventFilter -> Lude.Maybe DateTimeRange) (\s a -> s {lastUpdatedTime = a} :: OrganizationEventFilter)
{-# DEPRECATED oefLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | A list of 12-digit AWS account numbers that contains the affected entities.
--
-- /Note:/ Consider using 'awsAccountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefAwsAccountIds :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefAwsAccountIds = Lens.lens (awsAccountIds :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {awsAccountIds = a} :: OrganizationEventFilter)
{-# DEPRECATED oefAwsAccountIds "Use generic-lens or generic-optics with 'awsAccountIds' instead." #-}

-- | A list of event type category codes (issue, scheduledChange, or accountNotification).
--
-- /Note:/ Consider using 'eventTypeCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEventTypeCategories :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty EventTypeCategory))
oefEventTypeCategories = Lens.lens (eventTypeCategories :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty EventTypeCategory)) (\s a -> s {eventTypeCategories = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEventTypeCategories "Use generic-lens or generic-optics with 'eventTypeCategories' instead." #-}

-- | A list of unique identifiers for event types. For example, @"AWS_EC2_SYSTEM_MAINTENANCE_EVENT","AWS_RDS_MAINTENANCE_SCHEDULED".@
--
-- /Note:/ Consider using 'eventTypeCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEventTypeCodes :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefEventTypeCodes = Lens.lens (eventTypeCodes :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {eventTypeCodes = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEventTypeCodes "Use generic-lens or generic-optics with 'eventTypeCodes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefStartTime :: Lens.Lens' OrganizationEventFilter (Lude.Maybe DateTimeRange)
oefStartTime = Lens.lens (startTime :: OrganizationEventFilter -> Lude.Maybe DateTimeRange) (\s a -> s {startTime = a} :: OrganizationEventFilter)
{-# DEPRECATED oefStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A list of AWS Regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefRegions :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefRegions = Lens.lens (regions :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {regions = a} :: OrganizationEventFilter)
{-# DEPRECATED oefRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | A list of event status codes.
--
-- /Note:/ Consider using 'eventStatusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEventStatusCodes :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty EventStatusCode))
oefEventStatusCodes = Lens.lens (eventStatusCodes :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty EventStatusCode)) (\s a -> s {eventStatusCodes = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEventStatusCodes "Use generic-lens or generic-optics with 'eventStatusCodes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEndTime :: Lens.Lens' OrganizationEventFilter (Lude.Maybe DateTimeRange)
oefEndTime = Lens.lens (endTime :: OrganizationEventFilter -> Lude.Maybe DateTimeRange) (\s a -> s {endTime = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A list of entity ARNs (unique identifiers).
--
-- /Note:/ Consider using 'entityARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEntityARNs :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefEntityARNs = Lens.lens (entityARNs :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityARNs = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEntityARNs "Use generic-lens or generic-optics with 'entityARNs' instead." #-}

-- | A list of entity identifiers, such as EC2 instance IDs (i-34ab692e) or EBS volumes (vol-426ab23e).
--
-- /Note:/ Consider using 'entityValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefEntityValues :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefEntityValues = Lens.lens (entityValues :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {entityValues = a} :: OrganizationEventFilter)
{-# DEPRECATED oefEntityValues "Use generic-lens or generic-optics with 'entityValues' instead." #-}

-- | The AWS services associated with the event. For example, @EC2@ , @RDS@ .
--
-- /Note:/ Consider using 'services' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oefServices :: Lens.Lens' OrganizationEventFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
oefServices = Lens.lens (services :: OrganizationEventFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {services = a} :: OrganizationEventFilter)
{-# DEPRECATED oefServices "Use generic-lens or generic-optics with 'services' instead." #-}

instance Lude.ToJSON OrganizationEventFilter where
  toJSON OrganizationEventFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("lastUpdatedTime" Lude..=) Lude.<$> lastUpdatedTime,
            ("awsAccountIds" Lude..=) Lude.<$> awsAccountIds,
            ("eventTypeCategories" Lude..=) Lude.<$> eventTypeCategories,
            ("eventTypeCodes" Lude..=) Lude.<$> eventTypeCodes,
            ("startTime" Lude..=) Lude.<$> startTime,
            ("regions" Lude..=) Lude.<$> regions,
            ("eventStatusCodes" Lude..=) Lude.<$> eventStatusCodes,
            ("endTime" Lude..=) Lude.<$> endTime,
            ("entityArns" Lude..=) Lude.<$> entityARNs,
            ("entityValues" Lude..=) Lude.<$> entityValues,
            ("services" Lude..=) Lude.<$> services
          ]
      )
