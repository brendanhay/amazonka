{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results for the AWS Trusted Advisor check summaries for the check IDs that you specified. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains an array of 'TrustedAdvisorCheckSummary' objects.
module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
  ( -- * Creating a request
    DescribeTrustedAdvisorCheckSummaries (..),
    mkDescribeTrustedAdvisorCheckSummaries,

    -- ** Request lenses
    dtacsCheckIds,

    -- * Destructuring the response
    DescribeTrustedAdvisorCheckSummariesResponse (..),
    mkDescribeTrustedAdvisorCheckSummariesResponse,

    -- ** Response lenses
    dtacsrrsSummaries,
    dtacsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeTrustedAdvisorCheckSummaries' smart constructor.
newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries'
  { -- | The IDs of the Trusted Advisor checks.
    checkIds :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorCheckSummaries' value with any optional fields omitted.
mkDescribeTrustedAdvisorCheckSummaries ::
  DescribeTrustedAdvisorCheckSummaries
mkDescribeTrustedAdvisorCheckSummaries =
  DescribeTrustedAdvisorCheckSummaries' {checkIds = Core.mempty}

-- | The IDs of the Trusted Advisor checks.
--
-- /Note:/ Consider using 'checkIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsCheckIds :: Lens.Lens' DescribeTrustedAdvisorCheckSummaries [Types.String]
dtacsCheckIds = Lens.field @"checkIds"
{-# DEPRECATED dtacsCheckIds "Use generic-lens or generic-optics with 'checkIds' instead." #-}

instance Core.FromJSON DescribeTrustedAdvisorCheckSummaries where
  toJSON DescribeTrustedAdvisorCheckSummaries {..} =
    Core.object
      (Core.catMaybes [Core.Just ("checkIds" Core..= checkIds)])

instance Core.AWSRequest DescribeTrustedAdvisorCheckSummaries where
  type
    Rs DescribeTrustedAdvisorCheckSummaries =
      DescribeTrustedAdvisorCheckSummariesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSSupport_20130415.DescribeTrustedAdvisorCheckSummaries"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckSummariesResponse'
            Core.<$> (x Core..:? "summaries" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The summaries of the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorCheckSummaries' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckSummariesResponse' smart constructor.
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
  { -- | The summary information for the requested Trusted Advisor checks.
    summaries :: [Types.TrustedAdvisorCheckSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTrustedAdvisorCheckSummariesResponse' value with any optional fields omitted.
mkDescribeTrustedAdvisorCheckSummariesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTrustedAdvisorCheckSummariesResponse
mkDescribeTrustedAdvisorCheckSummariesResponse responseStatus =
  DescribeTrustedAdvisorCheckSummariesResponse'
    { summaries =
        Core.mempty,
      responseStatus
    }

-- | The summary information for the requested Trusted Advisor checks.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsrrsSummaries :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse [Types.TrustedAdvisorCheckSummary]
dtacsrrsSummaries = Lens.field @"summaries"
{-# DEPRECATED dtacsrrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsrrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse Core.Int
dtacsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtacsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
