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
-- Module      : Amazonka.ElasticBeanstalk.Types.ApplicationMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ApplicationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types.Latency
import Amazonka.ElasticBeanstalk.Types.StatusCodes
import qualified Amazonka.Prelude as Prelude

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'newApplicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { -- | Represents the average latency for the slowest X percent of requests
    -- over the last 10 seconds. Latencies are in seconds with one millisecond
    -- resolution.
    latency :: Prelude.Maybe Latency,
    -- | The amount of time that the metrics cover (usually 10 seconds). For
    -- example, you might have 5 requests (@request_count@) within the most
    -- recent time slice of 10 seconds (@duration@).
    duration :: Prelude.Maybe Prelude.Int,
    -- | Average number of requests handled by the web server per second over the
    -- last 10 seconds.
    requestCount :: Prelude.Maybe Prelude.Int,
    -- | Represents the percentage of requests over the last 10 seconds that
    -- resulted in each type of status code response.
    statusCodes :: Prelude.Maybe StatusCodes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latency', 'applicationMetrics_latency' - Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds. Latencies are in seconds with one millisecond
-- resolution.
--
-- 'duration', 'applicationMetrics_duration' - The amount of time that the metrics cover (usually 10 seconds). For
-- example, you might have 5 requests (@request_count@) within the most
-- recent time slice of 10 seconds (@duration@).
--
-- 'requestCount', 'applicationMetrics_requestCount' - Average number of requests handled by the web server per second over the
-- last 10 seconds.
--
-- 'statusCodes', 'applicationMetrics_statusCodes' - Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response.
newApplicationMetrics ::
  ApplicationMetrics
newApplicationMetrics =
  ApplicationMetrics'
    { latency = Prelude.Nothing,
      duration = Prelude.Nothing,
      requestCount = Prelude.Nothing,
      statusCodes = Prelude.Nothing
    }

-- | Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds. Latencies are in seconds with one millisecond
-- resolution.
applicationMetrics_latency :: Lens.Lens' ApplicationMetrics (Prelude.Maybe Latency)
applicationMetrics_latency = Lens.lens (\ApplicationMetrics' {latency} -> latency) (\s@ApplicationMetrics' {} a -> s {latency = a} :: ApplicationMetrics)

-- | The amount of time that the metrics cover (usually 10 seconds). For
-- example, you might have 5 requests (@request_count@) within the most
-- recent time slice of 10 seconds (@duration@).
applicationMetrics_duration :: Lens.Lens' ApplicationMetrics (Prelude.Maybe Prelude.Int)
applicationMetrics_duration = Lens.lens (\ApplicationMetrics' {duration} -> duration) (\s@ApplicationMetrics' {} a -> s {duration = a} :: ApplicationMetrics)

-- | Average number of requests handled by the web server per second over the
-- last 10 seconds.
applicationMetrics_requestCount :: Lens.Lens' ApplicationMetrics (Prelude.Maybe Prelude.Int)
applicationMetrics_requestCount = Lens.lens (\ApplicationMetrics' {requestCount} -> requestCount) (\s@ApplicationMetrics' {} a -> s {requestCount = a} :: ApplicationMetrics)

-- | Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response.
applicationMetrics_statusCodes :: Lens.Lens' ApplicationMetrics (Prelude.Maybe StatusCodes)
applicationMetrics_statusCodes = Lens.lens (\ApplicationMetrics' {statusCodes} -> statusCodes) (\s@ApplicationMetrics' {} a -> s {statusCodes = a} :: ApplicationMetrics)

instance Data.FromXML ApplicationMetrics where
  parseXML x =
    ApplicationMetrics'
      Prelude.<$> (x Data..@? "Latency")
      Prelude.<*> (x Data..@? "Duration")
      Prelude.<*> (x Data..@? "RequestCount")
      Prelude.<*> (x Data..@? "StatusCodes")

instance Prelude.Hashable ApplicationMetrics where
  hashWithSalt _salt ApplicationMetrics' {..} =
    _salt `Prelude.hashWithSalt` latency
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` requestCount
      `Prelude.hashWithSalt` statusCodes

instance Prelude.NFData ApplicationMetrics where
  rnf ApplicationMetrics' {..} =
    Prelude.rnf latency
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf requestCount
      `Prelude.seq` Prelude.rnf statusCodes
