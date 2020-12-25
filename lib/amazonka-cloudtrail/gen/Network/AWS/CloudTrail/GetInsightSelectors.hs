{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetInsightSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the Insights event selectors that you configured for your trail. @GetInsightSelectors@ shows if CloudTrail Insights event logging is enabled on the trail, and if it is, which insight types are enabled. If you run @GetInsightSelectors@ on a trail that does not have Insights events enabled, the operation throws the exception @InsightNotEnabledException@
--
-- For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-insights-events-with-cloudtrail.html Logging CloudTrail Insights Events for Trails > in the /AWS CloudTrail User Guide/ .
module Network.AWS.CloudTrail.GetInsightSelectors
  ( -- * Creating a request
    GetInsightSelectors (..),
    mkGetInsightSelectors,

    -- ** Request lenses
    gisTrailName,

    -- * Destructuring the response
    GetInsightSelectorsResponse (..),
    mkGetInsightSelectorsResponse,

    -- ** Response lenses
    gisrrsInsightSelectors,
    gisrrsTrailARN,
    gisrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInsightSelectors' smart constructor.
newtype GetInsightSelectors = GetInsightSelectors'
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
    trailName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInsightSelectors' value with any optional fields omitted.
mkGetInsightSelectors ::
  -- | 'trailName'
  Types.String ->
  GetInsightSelectors
mkGetInsightSelectors trailName = GetInsightSelectors' {trailName}

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
gisTrailName :: Lens.Lens' GetInsightSelectors Types.String
gisTrailName = Lens.field @"trailName"
{-# DEPRECATED gisTrailName "Use generic-lens or generic-optics with 'trailName' instead." #-}

instance Core.FromJSON GetInsightSelectors where
  toJSON GetInsightSelectors {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TrailName" Core..= trailName)])

instance Core.AWSRequest GetInsightSelectors where
  type Rs GetInsightSelectors = GetInsightSelectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetInsightSelectors"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSelectorsResponse'
            Core.<$> (x Core..:? "InsightSelectors")
            Core.<*> (x Core..:? "TrailARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInsightSelectorsResponse' smart constructor.
data GetInsightSelectorsResponse = GetInsightSelectorsResponse'
  { -- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
    insightSelectors :: Core.Maybe [Types.InsightSelector],
    -- | The Amazon Resource Name (ARN) of a trail for which you want to get Insights selectors.
    trailARN :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInsightSelectorsResponse' value with any optional fields omitted.
mkGetInsightSelectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInsightSelectorsResponse
mkGetInsightSelectorsResponse responseStatus =
  GetInsightSelectorsResponse'
    { insightSelectors = Core.Nothing,
      trailARN = Core.Nothing,
      responseStatus
    }

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightSelectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsInsightSelectors :: Lens.Lens' GetInsightSelectorsResponse (Core.Maybe [Types.InsightSelector])
gisrrsInsightSelectors = Lens.field @"insightSelectors"
{-# DEPRECATED gisrrsInsightSelectors "Use generic-lens or generic-optics with 'insightSelectors' instead." #-}

-- | The Amazon Resource Name (ARN) of a trail for which you want to get Insights selectors.
--
-- /Note:/ Consider using 'trailARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsTrailARN :: Lens.Lens' GetInsightSelectorsResponse (Core.Maybe Types.String)
gisrrsTrailARN = Lens.field @"trailARN"
{-# DEPRECATED gisrrsTrailARN "Use generic-lens or generic-optics with 'trailARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetInsightSelectorsResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
