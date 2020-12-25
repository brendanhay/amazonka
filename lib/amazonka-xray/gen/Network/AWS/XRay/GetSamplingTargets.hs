{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a sampling quota for rules that the service is using to sample requests.
module Network.AWS.XRay.GetSamplingTargets
  ( -- * Creating a request
    GetSamplingTargets (..),
    mkGetSamplingTargets,

    -- ** Request lenses
    gstSamplingStatisticsDocuments,

    -- * Destructuring the response
    GetSamplingTargetsResponse (..),
    mkGetSamplingTargetsResponse,

    -- ** Response lenses
    gstrrsLastRuleModification,
    gstrrsSamplingTargetDocuments,
    gstrrsUnprocessedStatistics,
    gstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetSamplingTargets' smart constructor.
newtype GetSamplingTargets = GetSamplingTargets'
  { -- | Information about rules that the service is using to sample requests.
    samplingStatisticsDocuments :: [Types.SamplingStatisticsDocument]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'GetSamplingTargets' value with any optional fields omitted.
mkGetSamplingTargets ::
  GetSamplingTargets
mkGetSamplingTargets =
  GetSamplingTargets' {samplingStatisticsDocuments = Core.mempty}

-- | Information about rules that the service is using to sample requests.
--
-- /Note:/ Consider using 'samplingStatisticsDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstSamplingStatisticsDocuments :: Lens.Lens' GetSamplingTargets [Types.SamplingStatisticsDocument]
gstSamplingStatisticsDocuments = Lens.field @"samplingStatisticsDocuments"
{-# DEPRECATED gstSamplingStatisticsDocuments "Use generic-lens or generic-optics with 'samplingStatisticsDocuments' instead." #-}

instance Core.FromJSON GetSamplingTargets where
  toJSON GetSamplingTargets {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SamplingStatisticsDocuments"
                  Core..= samplingStatisticsDocuments
              )
          ]
      )

instance Core.AWSRequest GetSamplingTargets where
  type Rs GetSamplingTargets = GetSamplingTargetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/SamplingTargets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSamplingTargetsResponse'
            Core.<$> (x Core..:? "LastRuleModification")
            Core.<*> (x Core..:? "SamplingTargetDocuments")
            Core.<*> (x Core..:? "UnprocessedStatistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSamplingTargetsResponse' smart constructor.
data GetSamplingTargetsResponse = GetSamplingTargetsResponse'
  { -- | The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
    lastRuleModification :: Core.Maybe Core.NominalDiffTime,
    -- | Updated rules that the service should use to sample requests.
    samplingTargetDocuments :: Core.Maybe [Types.SamplingTargetDocument],
    -- | Information about 'SamplingStatisticsDocument' that X-Ray could not process.
    unprocessedStatistics :: Core.Maybe [Types.UnprocessedStatistics],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSamplingTargetsResponse' value with any optional fields omitted.
mkGetSamplingTargetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSamplingTargetsResponse
mkGetSamplingTargetsResponse responseStatus =
  GetSamplingTargetsResponse'
    { lastRuleModification = Core.Nothing,
      samplingTargetDocuments = Core.Nothing,
      unprocessedStatistics = Core.Nothing,
      responseStatus
    }

-- | The last time a user changed the sampling rule configuration. If the sampling rule configuration changed since the service last retrieved it, the service should call 'GetSamplingRules' to get the latest version.
--
-- /Note:/ Consider using 'lastRuleModification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsLastRuleModification :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe Core.NominalDiffTime)
gstrrsLastRuleModification = Lens.field @"lastRuleModification"
{-# DEPRECATED gstrrsLastRuleModification "Use generic-lens or generic-optics with 'lastRuleModification' instead." #-}

-- | Updated rules that the service should use to sample requests.
--
-- /Note:/ Consider using 'samplingTargetDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsSamplingTargetDocuments :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe [Types.SamplingTargetDocument])
gstrrsSamplingTargetDocuments = Lens.field @"samplingTargetDocuments"
{-# DEPRECATED gstrrsSamplingTargetDocuments "Use generic-lens or generic-optics with 'samplingTargetDocuments' instead." #-}

-- | Information about 'SamplingStatisticsDocument' that X-Ray could not process.
--
-- /Note:/ Consider using 'unprocessedStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsUnprocessedStatistics :: Lens.Lens' GetSamplingTargetsResponse (Core.Maybe [Types.UnprocessedStatistics])
gstrrsUnprocessedStatistics = Lens.field @"unprocessedStatistics"
{-# DEPRECATED gstrrsUnprocessedStatistics "Use generic-lens or generic-optics with 'unprocessedStatistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gstrrsResponseStatus :: Lens.Lens' GetSamplingTargetsResponse Core.Int
gstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
