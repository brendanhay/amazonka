{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import qualified Network.AWS.Lens as Lens

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'newApplicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { -- | The amount of time that the metrics cover (usually 10 seconds). For
    -- example, you might have 5 requests (@request_count@) within the most
    -- recent time slice of 10 seconds (@duration@).
    duration :: Core.Maybe Core.Int,
    -- | Represents the percentage of requests over the last 10 seconds that
    -- resulted in each type of status code response.
    statusCodes :: Core.Maybe StatusCodes,
    -- | Average number of requests handled by the web server per second over the
    -- last 10 seconds.
    requestCount :: Core.Maybe Core.Int,
    -- | Represents the average latency for the slowest X percent of requests
    -- over the last 10 seconds. Latencies are in seconds with one millisecond
    -- resolution.
    latency :: Core.Maybe Latency
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'applicationMetrics_duration' - The amount of time that the metrics cover (usually 10 seconds). For
-- example, you might have 5 requests (@request_count@) within the most
-- recent time slice of 10 seconds (@duration@).
--
-- 'statusCodes', 'applicationMetrics_statusCodes' - Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response.
--
-- 'requestCount', 'applicationMetrics_requestCount' - Average number of requests handled by the web server per second over the
-- last 10 seconds.
--
-- 'latency', 'applicationMetrics_latency' - Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds. Latencies are in seconds with one millisecond
-- resolution.
newApplicationMetrics ::
  ApplicationMetrics
newApplicationMetrics =
  ApplicationMetrics'
    { duration = Core.Nothing,
      statusCodes = Core.Nothing,
      requestCount = Core.Nothing,
      latency = Core.Nothing
    }

-- | The amount of time that the metrics cover (usually 10 seconds). For
-- example, you might have 5 requests (@request_count@) within the most
-- recent time slice of 10 seconds (@duration@).
applicationMetrics_duration :: Lens.Lens' ApplicationMetrics (Core.Maybe Core.Int)
applicationMetrics_duration = Lens.lens (\ApplicationMetrics' {duration} -> duration) (\s@ApplicationMetrics' {} a -> s {duration = a} :: ApplicationMetrics)

-- | Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response.
applicationMetrics_statusCodes :: Lens.Lens' ApplicationMetrics (Core.Maybe StatusCodes)
applicationMetrics_statusCodes = Lens.lens (\ApplicationMetrics' {statusCodes} -> statusCodes) (\s@ApplicationMetrics' {} a -> s {statusCodes = a} :: ApplicationMetrics)

-- | Average number of requests handled by the web server per second over the
-- last 10 seconds.
applicationMetrics_requestCount :: Lens.Lens' ApplicationMetrics (Core.Maybe Core.Int)
applicationMetrics_requestCount = Lens.lens (\ApplicationMetrics' {requestCount} -> requestCount) (\s@ApplicationMetrics' {} a -> s {requestCount = a} :: ApplicationMetrics)

-- | Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds. Latencies are in seconds with one millisecond
-- resolution.
applicationMetrics_latency :: Lens.Lens' ApplicationMetrics (Core.Maybe Latency)
applicationMetrics_latency = Lens.lens (\ApplicationMetrics' {latency} -> latency) (\s@ApplicationMetrics' {} a -> s {latency = a} :: ApplicationMetrics)

instance Core.FromXML ApplicationMetrics where
  parseXML x =
    ApplicationMetrics'
      Core.<$> (x Core..@? "Duration")
      Core.<*> (x Core..@? "StatusCodes")
      Core.<*> (x Core..@? "RequestCount")
      Core.<*> (x Core..@? "Latency")

instance Core.Hashable ApplicationMetrics

instance Core.NFData ApplicationMetrics
