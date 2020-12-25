{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.PutEventSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an event selector for your trail. Use event selectors to further specify the management and data event settings for your trail. By default, trails created without specific event selectors will be configured to log all read and write management events, and no data events.
--
-- When an event occurs in your account, CloudTrail evaluates the event selectors in all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.
-- Example
--
--     * You create an event selector for a trail and specify that you want write-only events.
--
--
--     * The EC2 @GetConsoleOutput@ and @RunInstances@ API operations occur in your account.
--
--
--     * CloudTrail evaluates whether the events match your event selectors.
--
--
--     * The @RunInstances@ is a write-only event and it matches your event selector. The trail logs the event.
--
--
--     * The @GetConsoleOutput@ is a read-only event but it doesn't match your event selector. The trail doesn't log the event.
--
--
-- The @PutEventSelectors@ operation must be called from the region in which the trail was created; otherwise, an @InvalidHomeRegionException@ is thrown.
-- You can configure up to five event selectors for each trail. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > and <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Limits in AWS CloudTrail> in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.PutEventSelectors
  ( -- * Creating a request
    PutEventSelectors (..),
    mkPutEventSelectors,

    -- ** Request lenses
    pesTrailName,
    pesAdvancedEventSelectors,
    pesEventSelectors,

    -- * Destructuring the response
    PutEventSelectorsResponse (..),
    mkPutEventSelectorsResponse,

    -- ** Response lenses
    pesrrsAdvancedEventSelectors,
    pesrrsEventSelectors,
    pesrrsTrailARN,
    pesrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutEventSelectors' smart constructor.
data PutEventSelectors = PutEventSelectors'
  { -- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:
    --
    --
    --     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
    --
    --
    --     * Start with a letter or number, and end with a letter or number
    --
    --
    --     * Be between 3 and 128 characters
    --
    --
    --     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
    --
    --
    --     * Not be in IP address format (for example, 192.168.5.4)
    --
    --
    -- If you specify a trail ARN, it must be in the format:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailName :: Types.String,
    advancedEventSelectors :: Core.Maybe [Types.AdvancedEventSelector],
    -- | Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
    eventSelectors :: Core.Maybe [Types.EventSelector]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventSelectors' value with any optional fields omitted.
mkPutEventSelectors ::
  -- | 'trailName'
  Types.String ->
  PutEventSelectors
mkPutEventSelectors trailName =
  PutEventSelectors'
    { trailName,
      advancedEventSelectors = Core.Nothing,
      eventSelectors = Core.Nothing
    }

-- | Specifies the name of the trail or trail ARN. If you specify a trail name, the string must meet the following requirements:
--
--
--     * Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)
--
--
--     * Start with a letter or number, and end with a letter or number
--
--
--     * Be between 3 and 128 characters
--
--
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are invalid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesTrailName :: Lens.Lens' PutEventSelectors Types.String
pesTrailName = Lens.field @"trailName"
{-# DEPRECATED pesTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesAdvancedEventSelectors :: Lens.Lens' PutEventSelectors (Core.Maybe [Types.AdvancedEventSelector])
pesAdvancedEventSelectors = Lens.field @"advancedEventSelectors"
{-# DEPRECATED pesAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | Specifies the settings for your event selectors. You can configure up to five event selectors for a trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesEventSelectors :: Lens.Lens' PutEventSelectors (Core.Maybe [Types.EventSelector])
pesEventSelectors = Lens.field @"eventSelectors"
{-# DEPRECATED pesEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

instance Core.FromJSON PutEventSelectors where
  toJSON PutEventSelectors {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrailName" Core..= trailName),
            ("AdvancedEventSelectors" Core..=) Core.<$> advancedEventSelectors,
            ("EventSelectors" Core..=) Core.<$> eventSelectors
          ]
      )

instance Core.AWSRequest PutEventSelectors where
  type Rs PutEventSelectors = PutEventSelectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutEventSelectors"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventSelectorsResponse'
            Core.<$> (x Core..:? "AdvancedEventSelectors")
            Core.<*> (x Core..:? "EventSelectors")
            Core.<*> (x Core..:? "TrailARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutEventSelectorsResponse' smart constructor.
data PutEventSelectorsResponse = PutEventSelectorsResponse'
  { advancedEventSelectors :: Core.Maybe [Types.AdvancedEventSelector],
    -- | Specifies the event selectors configured for your trail.
    eventSelectors :: Core.Maybe [Types.EventSelector],
    -- | Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutEventSelectorsResponse' value with any optional fields omitted.
mkPutEventSelectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutEventSelectorsResponse
mkPutEventSelectorsResponse responseStatus =
  PutEventSelectorsResponse'
    { advancedEventSelectors = Core.Nothing,
      eventSelectors = Core.Nothing,
      trailARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsAdvancedEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Core.Maybe [Types.AdvancedEventSelector])
pesrrsAdvancedEventSelectors = Lens.field @"advancedEventSelectors"
{-# DEPRECATED pesrrsAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | Specifies the event selectors configured for your trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Core.Maybe [Types.EventSelector])
pesrrsEventSelectors = Lens.field @"eventSelectors"
{-# DEPRECATED pesrrsEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

-- | Specifies the ARN of the trail that was updated with event selectors. The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsTrailARN :: Lens.Lens' PutEventSelectorsResponse (Core.Maybe Types.String)
pesrrsTrailARN = Lens.field @"trailARN"
{-# DEPRECATED pesrrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pesrrsResponseStatus :: Lens.Lens' PutEventSelectorsResponse Core.Int
pesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
