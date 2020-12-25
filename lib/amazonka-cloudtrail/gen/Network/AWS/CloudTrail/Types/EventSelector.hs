{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.EventSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.EventSelector
  ( EventSelector (..),

    -- * Smart constructor
    mkEventSelector,

    -- * Lenses
    esDataResources,
    esExcludeManagementEventSources,
    esIncludeManagementEvents,
    esReadWriteType,
  )
where

import qualified Network.AWS.CloudTrail.Types.DataResource as Types
import qualified Network.AWS.CloudTrail.Types.ReadWriteType as Types
import qualified Network.AWS.CloudTrail.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use event selectors to further specify the management and data event settings for your trail. By default, trails created without specific event selectors will be configured to log all read and write management events, and no data events. When an event occurs in your account, CloudTrail evaluates the event selector for all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
--
-- You can configure up to five event selectors for a trail.
--
-- /See:/ 'mkEventSelector' smart constructor.
data EventSelector = EventSelector'
  { -- | CloudTrail supports data event logging for Amazon S3 objects and AWS Lambda functions. You can specify up to 250 resources for an individual event selector, but the total number of data resources cannot exceed 250 across all event selectors in a trail. This limit does not apply if you configure resource logging for all data events.
    --
    -- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
    dataResources :: Core.Maybe [Types.DataResource],
    -- | An optional list of service event sources from which you do not want management events to be logged on your trail. In this release, the list can be empty (disables the filter), or it can filter out AWS Key Management Service events by containing @"kms.amazonaws.com"@ . By default, @ExcludeManagementEventSources@ is empty, and AWS KMS events are included in events that are logged to your trail.
    excludeManagementEventSources :: Core.Maybe [Types.String],
    -- | Specify if you want your event selector to include management events for your trail.
    --
    -- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ .
    -- By default, the value is @true@ .
    -- The first copy of management events is free. You are charged for additional copies of management events that you are logging on any subsequent trail in the same region. For more information about CloudTrail pricing, see <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing> .
    includeManagementEvents :: Core.Maybe Core.Bool,
    -- | Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation.
    --
    -- By default, the value is @All@ .
    readWriteType :: Core.Maybe Types.ReadWriteType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventSelector' value with any optional fields omitted.
mkEventSelector ::
  EventSelector
mkEventSelector =
  EventSelector'
    { dataResources = Core.Nothing,
      excludeManagementEventSources = Core.Nothing,
      includeManagementEvents = Core.Nothing,
      readWriteType = Core.Nothing
    }

-- | CloudTrail supports data event logging for Amazon S3 objects and AWS Lambda functions. You can specify up to 250 resources for an individual event selector, but the total number of data resources cannot exceed 250 across all event selectors in a trail. This limit does not apply if you configure resource logging for all data events.
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-data-events Data Events> and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
--
-- /Note:/ Consider using 'dataResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esDataResources :: Lens.Lens' EventSelector (Core.Maybe [Types.DataResource])
esDataResources = Lens.field @"dataResources"
{-# DEPRECATED esDataResources "Use generic-lens or generic-optics with 'dataResources' instead." #-}

-- | An optional list of service event sources from which you do not want management events to be logged on your trail. In this release, the list can be empty (disables the filter), or it can filter out AWS Key Management Service events by containing @"kms.amazonaws.com"@ . By default, @ExcludeManagementEventSources@ is empty, and AWS KMS events are included in events that are logged to your trail.
--
-- /Note:/ Consider using 'excludeManagementEventSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExcludeManagementEventSources :: Lens.Lens' EventSelector (Core.Maybe [Types.String])
esExcludeManagementEventSources = Lens.field @"excludeManagementEventSources"
{-# DEPRECATED esExcludeManagementEventSources "Use generic-lens or generic-optics with 'excludeManagementEventSources' instead." #-}

-- | Specify if you want your event selector to include management events for your trail.
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html#logging-management-events Management Events> in the /AWS CloudTrail User Guide/ .
-- By default, the value is @true@ .
-- The first copy of management events is free. You are charged for additional copies of management events that you are logging on any subsequent trail in the same region. For more information about CloudTrail pricing, see <http://aws.amazon.com/cloudtrail/pricing/ AWS CloudTrail Pricing> .
--
-- /Note:/ Consider using 'includeManagementEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esIncludeManagementEvents :: Lens.Lens' EventSelector (Core.Maybe Core.Bool)
esIncludeManagementEvents = Lens.field @"includeManagementEvents"
{-# DEPRECATED esIncludeManagementEvents "Use generic-lens or generic-optics with 'includeManagementEvents' instead." #-}

-- | Specify if you want your trail to log read-only events, write-only events, or all. For example, the EC2 @GetConsoleOutput@ is a read-only API operation and @RunInstances@ is a write-only API operation.
--
-- By default, the value is @All@ .
--
-- /Note:/ Consider using 'readWriteType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esReadWriteType :: Lens.Lens' EventSelector (Core.Maybe Types.ReadWriteType)
esReadWriteType = Lens.field @"readWriteType"
{-# DEPRECATED esReadWriteType "Use generic-lens or generic-optics with 'readWriteType' instead." #-}

instance Core.FromJSON EventSelector where
  toJSON EventSelector {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataResources" Core..=) Core.<$> dataResources,
            ("ExcludeManagementEventSources" Core..=)
              Core.<$> excludeManagementEventSources,
            ("IncludeManagementEvents" Core..=)
              Core.<$> includeManagementEvents,
            ("ReadWriteType" Core..=) Core.<$> readWriteType
          ]
      )

instance Core.FromJSON EventSelector where
  parseJSON =
    Core.withObject "EventSelector" Core.$
      \x ->
        EventSelector'
          Core.<$> (x Core..:? "DataResources")
          Core.<*> (x Core..:? "ExcludeManagementEventSources")
          Core.<*> (x Core..:? "IncludeManagementEvents")
          Core.<*> (x Core..:? "ReadWriteType")
