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
-- Module      : Amazonka.InternetMonitor.Types.PerformanceMeasurement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.PerformanceMeasurement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.RoundTripTime
import qualified Amazonka.Prelude as Prelude

-- | Measurements about the performance for your application on the internet
-- calculated by Amazon CloudWatch Internet Monitor. Amazon Web Services
-- has substantial historical data about internet performance and
-- availability between Amazon Web Services services and different network
-- providers and geographies. By applying statistical analysis to the data,
-- Internet Monitor can detect when the performance and availability for
-- your application has dropped, compared to an estimated baseline that\'s
-- already calculated. To make it easier to see those drops, we report that
-- information to you in the form of health scores: a performance score and
-- an availability score.
--
-- Performance in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing a performance drop. For example, a
-- performance score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing a performance drop for that
-- pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- /See:/ 'newPerformanceMeasurement' smart constructor.
data PerformanceMeasurement = PerformanceMeasurement'
  { -- | Experience scores, or health scores, are calculated for different
    -- geographic and network provider combinations (that is, different
    -- granularities) and also totaled into global scores. If you view
    -- performance or availability scores without filtering for any specific
    -- geography or service provider, Amazon CloudWatch Internet Monitor
    -- provides global health scores.
    --
    -- The Amazon CloudWatch Internet Monitor chapter in the CloudWatch User
    -- Guide includes detailed information about how Internet Monitor
    -- calculates health scores, including performance and availability scores,
    -- and when it creates and resolves health events. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    experienceScore :: Prelude.Maybe Prelude.Double,
    -- | How much performance impact was caused by a health event at a client
    -- location. For performance, this is the percentage of how much latency
    -- increased during the event compared to typical performance for traffic,
    -- from this client location to an Amazon Web Services location, using a
    -- specific client network.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    percentOfClientLocationImpacted :: Prelude.Maybe Prelude.Double,
    -- | How much performance impact was caused by a health event for total
    -- traffic globally. For performance, this is the percentage of how much
    -- latency increased during the event compared to typical performance for
    -- your application traffic globally.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    percentOfTotalTrafficImpacted :: Prelude.Maybe Prelude.Double,
    -- | This is the percentage of how much round-trip time increased during the
    -- event compared to typical round-trip time for your application for
    -- traffic.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    roundTripTime :: Prelude.Maybe RoundTripTime
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceMeasurement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experienceScore', 'performanceMeasurement_experienceScore' - Experience scores, or health scores, are calculated for different
-- geographic and network provider combinations (that is, different
-- granularities) and also totaled into global scores. If you view
-- performance or availability scores without filtering for any specific
-- geography or service provider, Amazon CloudWatch Internet Monitor
-- provides global health scores.
--
-- The Amazon CloudWatch Internet Monitor chapter in the CloudWatch User
-- Guide includes detailed information about how Internet Monitor
-- calculates health scores, including performance and availability scores,
-- and when it creates and resolves health events. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'percentOfClientLocationImpacted', 'performanceMeasurement_percentOfClientLocationImpacted' - How much performance impact was caused by a health event at a client
-- location. For performance, this is the percentage of how much latency
-- increased during the event compared to typical performance for traffic,
-- from this client location to an Amazon Web Services location, using a
-- specific client network.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'percentOfTotalTrafficImpacted', 'performanceMeasurement_percentOfTotalTrafficImpacted' - How much performance impact was caused by a health event for total
-- traffic globally. For performance, this is the percentage of how much
-- latency increased during the event compared to typical performance for
-- your application traffic globally.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'roundTripTime', 'performanceMeasurement_roundTripTime' - This is the percentage of how much round-trip time increased during the
-- event compared to typical round-trip time for your application for
-- traffic.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
newPerformanceMeasurement ::
  PerformanceMeasurement
newPerformanceMeasurement =
  PerformanceMeasurement'
    { experienceScore =
        Prelude.Nothing,
      percentOfClientLocationImpacted = Prelude.Nothing,
      percentOfTotalTrafficImpacted = Prelude.Nothing,
      roundTripTime = Prelude.Nothing
    }

-- | Experience scores, or health scores, are calculated for different
-- geographic and network provider combinations (that is, different
-- granularities) and also totaled into global scores. If you view
-- performance or availability scores without filtering for any specific
-- geography or service provider, Amazon CloudWatch Internet Monitor
-- provides global health scores.
--
-- The Amazon CloudWatch Internet Monitor chapter in the CloudWatch User
-- Guide includes detailed information about how Internet Monitor
-- calculates health scores, including performance and availability scores,
-- and when it creates and resolves health events. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
performanceMeasurement_experienceScore :: Lens.Lens' PerformanceMeasurement (Prelude.Maybe Prelude.Double)
performanceMeasurement_experienceScore = Lens.lens (\PerformanceMeasurement' {experienceScore} -> experienceScore) (\s@PerformanceMeasurement' {} a -> s {experienceScore = a} :: PerformanceMeasurement)

-- | How much performance impact was caused by a health event at a client
-- location. For performance, this is the percentage of how much latency
-- increased during the event compared to typical performance for traffic,
-- from this client location to an Amazon Web Services location, using a
-- specific client network.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
performanceMeasurement_percentOfClientLocationImpacted :: Lens.Lens' PerformanceMeasurement (Prelude.Maybe Prelude.Double)
performanceMeasurement_percentOfClientLocationImpacted = Lens.lens (\PerformanceMeasurement' {percentOfClientLocationImpacted} -> percentOfClientLocationImpacted) (\s@PerformanceMeasurement' {} a -> s {percentOfClientLocationImpacted = a} :: PerformanceMeasurement)

-- | How much performance impact was caused by a health event for total
-- traffic globally. For performance, this is the percentage of how much
-- latency increased during the event compared to typical performance for
-- your application traffic globally.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
performanceMeasurement_percentOfTotalTrafficImpacted :: Lens.Lens' PerformanceMeasurement (Prelude.Maybe Prelude.Double)
performanceMeasurement_percentOfTotalTrafficImpacted = Lens.lens (\PerformanceMeasurement' {percentOfTotalTrafficImpacted} -> percentOfTotalTrafficImpacted) (\s@PerformanceMeasurement' {} a -> s {percentOfTotalTrafficImpacted = a} :: PerformanceMeasurement)

-- | This is the percentage of how much round-trip time increased during the
-- event compared to typical round-trip time for your application for
-- traffic.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMHealthEventStartStop When Amazon Web Services creates and resolves health events>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
performanceMeasurement_roundTripTime :: Lens.Lens' PerformanceMeasurement (Prelude.Maybe RoundTripTime)
performanceMeasurement_roundTripTime = Lens.lens (\PerformanceMeasurement' {roundTripTime} -> roundTripTime) (\s@PerformanceMeasurement' {} a -> s {roundTripTime = a} :: PerformanceMeasurement)

instance Data.FromJSON PerformanceMeasurement where
  parseJSON =
    Data.withObject
      "PerformanceMeasurement"
      ( \x ->
          PerformanceMeasurement'
            Prelude.<$> (x Data..:? "ExperienceScore")
            Prelude.<*> (x Data..:? "PercentOfClientLocationImpacted")
            Prelude.<*> (x Data..:? "PercentOfTotalTrafficImpacted")
            Prelude.<*> (x Data..:? "RoundTripTime")
      )

instance Prelude.Hashable PerformanceMeasurement where
  hashWithSalt _salt PerformanceMeasurement' {..} =
    _salt
      `Prelude.hashWithSalt` experienceScore
      `Prelude.hashWithSalt` percentOfClientLocationImpacted
      `Prelude.hashWithSalt` percentOfTotalTrafficImpacted
      `Prelude.hashWithSalt` roundTripTime

instance Prelude.NFData PerformanceMeasurement where
  rnf PerformanceMeasurement' {..} =
    Prelude.rnf experienceScore
      `Prelude.seq` Prelude.rnf percentOfClientLocationImpacted
      `Prelude.seq` Prelude.rnf percentOfTotalTrafficImpacted
      `Prelude.seq` Prelude.rnf roundTripTime
