{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
  ( ApplicationMetrics (..)
  -- * Smart constructor
  , mkApplicationMetrics
  -- * Lenses
  , amDuration
  , amLatency
  , amRequestCount
  , amStatusCodes
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.Latency as Types
import qualified Network.AWS.ElasticBeanstalk.Types.StatusCodes as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkApplicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { duration :: Core.Maybe Core.Int
    -- ^ The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
  , latency :: Core.Maybe Types.Latency
    -- ^ Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
  , requestCount :: Core.Maybe Core.Int
    -- ^ Average number of requests handled by the web server per second over the last 10 seconds.
  , statusCodes :: Core.Maybe Types.StatusCodes
    -- ^ Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationMetrics' value with any optional fields omitted.
mkApplicationMetrics
    :: ApplicationMetrics
mkApplicationMetrics
  = ApplicationMetrics'{duration = Core.Nothing,
                        latency = Core.Nothing, requestCount = Core.Nothing,
                        statusCodes = Core.Nothing}

-- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDuration :: Lens.Lens' ApplicationMetrics (Core.Maybe Core.Int)
amDuration = Lens.field @"duration"
{-# INLINEABLE amDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amLatency :: Lens.Lens' ApplicationMetrics (Core.Maybe Types.Latency)
amLatency = Lens.field @"latency"
{-# INLINEABLE amLatency #-}
{-# DEPRECATED latency "Use generic-lens or generic-optics with 'latency' instead"  #-}

-- | Average number of requests handled by the web server per second over the last 10 seconds.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amRequestCount :: Lens.Lens' ApplicationMetrics (Core.Maybe Core.Int)
amRequestCount = Lens.field @"requestCount"
{-# INLINEABLE amRequestCount #-}
{-# DEPRECATED requestCount "Use generic-lens or generic-optics with 'requestCount' instead"  #-}

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amStatusCodes :: Lens.Lens' ApplicationMetrics (Core.Maybe Types.StatusCodes)
amStatusCodes = Lens.field @"statusCodes"
{-# INLINEABLE amStatusCodes #-}
{-# DEPRECATED statusCodes "Use generic-lens or generic-optics with 'statusCodes' instead"  #-}

instance Core.FromXML ApplicationMetrics where
        parseXML x
          = ApplicationMetrics' Core.<$>
              (x Core..@? "Duration") Core.<*> x Core..@? "Latency" Core.<*>
                x Core..@? "RequestCount"
                Core.<*> x Core..@? "StatusCodes"
