{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetSendStatistics (..)
    , mkGetSendStatistics

    -- * Destructuring the response
    , GetSendStatisticsResponse (..)
    , mkGetSendStatisticsResponse
    -- ** Response lenses
    , gssrrsSendDataPoints
    , gssrrsResponseStatus
    ) where

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
mkGetSendStatistics
    :: GetSendStatistics
mkGetSendStatistics = GetSendStatistics'

instance Core.ToQuery GetSendStatistics where
        toQuery GetSendStatistics{..}
          = Core.toQueryPair "Action" ("GetSendStatistics" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders GetSendStatistics where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetSendStatistics where
        type Rs GetSendStatistics = GetSendStatisticsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetSendStatisticsResult"
              (\ s h x ->
                 GetSendStatisticsResponse' Core.<$>
                   (x Core..@? "SendDataPoints" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.
--
-- /See:/ 'mkGetSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
  { sendDataPoints :: Core.Maybe [Types.SendDataPoint]
    -- ^ A list of data points, each of which represents 15 minutes of activity.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSendStatisticsResponse' value with any optional fields omitted.
mkGetSendStatisticsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSendStatisticsResponse
mkGetSendStatisticsResponse responseStatus
  = GetSendStatisticsResponse'{sendDataPoints = Core.Nothing,
                               responseStatus}

-- | A list of data points, each of which represents 15 minutes of activity.
--
-- /Note:/ Consider using 'sendDataPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsSendDataPoints :: Lens.Lens' GetSendStatisticsResponse (Core.Maybe [Types.SendDataPoint])
gssrrsSendDataPoints = Lens.field @"sendDataPoints"
{-# INLINEABLE gssrrsSendDataPoints #-}
{-# DEPRECATED sendDataPoints "Use generic-lens or generic-optics with 'sendDataPoints' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrrsResponseStatus :: Lens.Lens' GetSendStatisticsResponse Core.Int
gssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
