{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetEventSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the event selectors that you configured for your trail. The information returned for your event selectors includes the following:
--
--
--     * If your event selector includes read-only events, write-only events, or all events. This applies to both management events and data events.
--
--
--     * If your event selector includes management events.
--
--
--     * If your event selector includes data events, the Amazon S3 objects or AWS Lambda functions that you are logging for data events.
--
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails > in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.GetEventSelectors
  ( -- * Creating a request
    GetEventSelectors (..),
    mkGetEventSelectors,

    -- ** Request lenses
    gesTrailName,

    -- * Destructuring the response
    GetEventSelectorsResponse (..),
    mkGetEventSelectorsResponse,

    -- ** Response lenses
    gesrrsAdvancedEventSelectors,
    gesrrsEventSelectors,
    gesrrsTrailARN,
    gesrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEventSelectors' smart constructor.
newtype GetEventSelectors = GetEventSelectors'
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
    --     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are not valid.
    --
    --
    --     * Not be in IP address format (for example, 192.168.5.4)
    --
    --
    -- If you specify a trail ARN, it must be in the format:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
    trailName :: Types.TrailName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetEventSelectors' value with any optional fields omitted.
mkGetEventSelectors ::
  -- | 'trailName'
  Types.TrailName ->
  GetEventSelectors
mkGetEventSelectors trailName = GetEventSelectors' {trailName}

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
--     * Have no adjacent periods, underscores or dashes. Names like @my-_namespace@ and @my--namespace@ are not valid.
--
--
--     * Not be in IP address format (for example, 192.168.5.4)
--
--
-- If you specify a trail ARN, it must be in the format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail/MyTrail@
--
-- /Note:/ Consider using 'trailName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesTrailName :: Lens.Lens' GetEventSelectors Types.TrailName
gesTrailName = Lens.field @"trailName"
{-# DEPRECATED gesTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

instance Core.FromJSON GetEventSelectors where
  toJSON GetEventSelectors {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TrailName" Core..= trailName)])

instance Core.AWSRequest GetEventSelectors where
  type Rs GetEventSelectors = GetEventSelectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventSelectors"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventSelectorsResponse'
            Core.<$> (x Core..:? "AdvancedEventSelectors")
            Core.<*> (x Core..:? "EventSelectors")
            Core.<*> (x Core..:? "TrailARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetEventSelectorsResponse' smart constructor.
data GetEventSelectorsResponse = GetEventSelectorsResponse'
  { advancedEventSelectors :: Core.Maybe [Types.AdvancedEventSelector],
    -- | The event selectors that are configured for the trail.
    eventSelectors :: Core.Maybe [Types.EventSelector],
    -- | The specified trail ARN that has the event selectors.
    trailARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEventSelectorsResponse' value with any optional fields omitted.
mkGetEventSelectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetEventSelectorsResponse
mkGetEventSelectorsResponse responseStatus =
  GetEventSelectorsResponse'
    { advancedEventSelectors = Core.Nothing,
      eventSelectors = Core.Nothing,
      trailARN = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'advancedEventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsAdvancedEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Core.Maybe [Types.AdvancedEventSelector])
gesrrsAdvancedEventSelectors = Lens.field @"advancedEventSelectors"
{-# DEPRECATED gesrrsAdvancedEventSelectors "Use generic-lens or generic-optics with 'advancedEventSelectors' instead." #-}

-- | The event selectors that are configured for the trail.
--
-- /Note:/ Consider using 'eventSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Core.Maybe [Types.EventSelector])
gesrrsEventSelectors = Lens.field @"eventSelectors"
{-# DEPRECATED gesrrsEventSelectors "Use generic-lens or generic-optics with 'eventSelectors' instead." #-}

-- | The specified trail ARN that has the event selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsTrailARN :: Lens.Lens' GetEventSelectorsResponse (Core.Maybe Types.String)
gesrrsTrailARN = Lens.field @"trailARN"
{-# DEPRECATED gesrrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gesrrsResponseStatus :: Lens.Lens' GetEventSelectorsResponse Core.Int
gesrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gesrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
