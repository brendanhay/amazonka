{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides sending statistics for the current AWS Region. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetSendStatistics
  ( -- * Creating a request
    GetSendStatistics (..),
    mkGetSendStatistics,

    -- * Destructuring the response
    GetSendStatisticsResponse (..),
    mkGetSendStatisticsResponse,

    -- ** Response lenses
    gssrrsSendDataPoints,
    gssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | /See:/ 'mkGetSendStatistics' smart constructor.
data GetSendStatistics = GetSendStatistics'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSendStatistics' value with any optional fields omitted.
mkGetSendStatistics ::
  GetSendStatistics
mkGetSendStatistics = GetSendStatistics'

instance Core.AWSRequest GetSendStatistics where
  type Rs GetSendStatistics = GetSendStatisticsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetSendStatistics")
                Core.<> (Core.pure ("Version", "2010-12-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetSendStatisticsResult"
      ( \s h x ->
          GetSendStatisticsResponse'
            Core.<$> (x Core..@? "SendDataPoints" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.
--
-- /See:/ 'mkGetSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
  { -- | A list of data points, each of which represents 15 minutes of activity.
    sendDataPoints :: Core.Maybe [Types.SendDataPoint],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetSendStatisticsResponse' value with any optional fields omitted.
mkGetSendStatisticsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSendStatisticsResponse
mkGetSendStatisticsResponse responseStatus =
  GetSendStatisticsResponse'
    { sendDataPoints = Core.Nothing,
      responseStatus
    }

-- | A list of data points, each of which represents 15 minutes of activity.
--
-- /Note:/ Consider using 'sendDataPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsSendDataPoints :: Lens.Lens' GetSendStatisticsResponse (Core.Maybe [Types.SendDataPoint])
gssrrsSendDataPoints = Lens.field @"sendDataPoints"
{-# DEPRECATED gssrrsSendDataPoints "Use generic-lens or generic-optics with 'sendDataPoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsResponseStatus :: Lens.Lens' GetSendStatisticsResponse Core.Int
gssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
