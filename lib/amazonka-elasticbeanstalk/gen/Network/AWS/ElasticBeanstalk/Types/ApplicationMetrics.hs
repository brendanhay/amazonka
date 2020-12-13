{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
  ( ApplicationMetrics (..),

    -- * Smart constructor
    mkApplicationMetrics,

    -- * Lenses
    amRequestCount,
    amLatency,
    amStatusCodes,
    amDuration,
  )
where

import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'mkApplicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { -- | Average number of requests handled by the web server per second over the last 10 seconds.
    requestCount :: Lude.Maybe Lude.Int,
    -- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
    latency :: Lude.Maybe Latency,
    -- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
    statusCodes :: Lude.Maybe StatusCodes,
    -- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationMetrics' with the minimum fields required to make a request.
--
-- * 'requestCount' - Average number of requests handled by the web server per second over the last 10 seconds.
-- * 'latency' - Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
-- * 'statusCodes' - Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
-- * 'duration' - The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
mkApplicationMetrics ::
  ApplicationMetrics
mkApplicationMetrics =
  ApplicationMetrics'
    { requestCount = Lude.Nothing,
      latency = Lude.Nothing,
      statusCodes = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | Average number of requests handled by the web server per second over the last 10 seconds.
--
-- /Note:/ Consider using 'requestCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amRequestCount :: Lens.Lens' ApplicationMetrics (Lude.Maybe Lude.Int)
amRequestCount = Lens.lens (requestCount :: ApplicationMetrics -> Lude.Maybe Lude.Int) (\s a -> s {requestCount = a} :: ApplicationMetrics)
{-# DEPRECATED amRequestCount "Use generic-lens or generic-optics with 'requestCount' instead." #-}

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
--
-- /Note:/ Consider using 'latency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amLatency :: Lens.Lens' ApplicationMetrics (Lude.Maybe Latency)
amLatency = Lens.lens (latency :: ApplicationMetrics -> Lude.Maybe Latency) (\s a -> s {latency = a} :: ApplicationMetrics)
{-# DEPRECATED amLatency "Use generic-lens or generic-optics with 'latency' instead." #-}

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
--
-- /Note:/ Consider using 'statusCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amStatusCodes :: Lens.Lens' ApplicationMetrics (Lude.Maybe StatusCodes)
amStatusCodes = Lens.lens (statusCodes :: ApplicationMetrics -> Lude.Maybe StatusCodes) (\s a -> s {statusCodes = a} :: ApplicationMetrics)
{-# DEPRECATED amStatusCodes "Use generic-lens or generic-optics with 'statusCodes' instead." #-}

-- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDuration :: Lens.Lens' ApplicationMetrics (Lude.Maybe Lude.Int)
amDuration = Lens.lens (duration :: ApplicationMetrics -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ApplicationMetrics)
{-# DEPRECATED amDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ApplicationMetrics where
  parseXML x =
    ApplicationMetrics'
      Lude.<$> (x Lude..@? "RequestCount")
      Lude.<*> (x Lude..@? "Latency")
      Lude.<*> (x Lude..@? "StatusCodes")
      Lude.<*> (x Lude..@? "Duration")
