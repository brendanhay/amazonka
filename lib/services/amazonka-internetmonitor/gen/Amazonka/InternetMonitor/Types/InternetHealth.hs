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
-- Module      : Amazonka.InternetMonitor.Types.InternetHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.InternetHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.AvailabilityMeasurement
import Amazonka.InternetMonitor.Types.PerformanceMeasurement
import qualified Amazonka.Prelude as Prelude

-- | Internet health includes measurements calculated by Amazon CloudWatch
-- Internet Monitor about the performance and availability for your
-- application on the internet. Amazon Web Services has substantial
-- historical data about internet performance and availability between
-- Amazon Web Services services and different network providers and
-- geographies. By applying statistical analysis to the data, Internet
-- Monitor can detect when the performance and availability for your
-- application has dropped, compared to an estimated baseline that\'s
-- already calculated. To make it easier to see those drops, we report that
-- information to you in the form of health scores: a performance score and
-- an availability score.
--
-- /See:/ 'newInternetHealth' smart constructor.
data InternetHealth = InternetHealth'
  { -- | Availability in Internet Monitor represents the estimated percentage of
    -- traffic that is not seeing an availability drop. For example, an
    -- availability score of 99% for an end user and service location pair is
    -- equivalent to 1% of the traffic experiencing an availability drop for
    -- that pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    availability :: Prelude.Maybe AvailabilityMeasurement,
    -- | Performance in Internet Monitor represents the estimated percentage of
    -- traffic that is not seeing a performance drop. For example, a
    -- performance score of 99% for an end user and service location pair is
    -- equivalent to 1% of the traffic experiencing a performance drop for that
    -- pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    performance :: Prelude.Maybe PerformanceMeasurement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InternetHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availability', 'internetHealth_availability' - Availability in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing an availability drop. For example, an
-- availability score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing an availability drop for
-- that pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'performance', 'internetHealth_performance' - Performance in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing a performance drop. For example, a
-- performance score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing a performance drop for that
-- pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
newInternetHealth ::
  InternetHealth
newInternetHealth =
  InternetHealth'
    { availability = Prelude.Nothing,
      performance = Prelude.Nothing
    }

-- | Availability in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing an availability drop. For example, an
-- availability score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing an availability drop for
-- that pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
internetHealth_availability :: Lens.Lens' InternetHealth (Prelude.Maybe AvailabilityMeasurement)
internetHealth_availability = Lens.lens (\InternetHealth' {availability} -> availability) (\s@InternetHealth' {} a -> s {availability = a} :: InternetHealth)

-- | Performance in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing a performance drop. For example, a
-- performance score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing a performance drop for that
-- pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
internetHealth_performance :: Lens.Lens' InternetHealth (Prelude.Maybe PerformanceMeasurement)
internetHealth_performance = Lens.lens (\InternetHealth' {performance} -> performance) (\s@InternetHealth' {} a -> s {performance = a} :: InternetHealth)

instance Data.FromJSON InternetHealth where
  parseJSON =
    Data.withObject
      "InternetHealth"
      ( \x ->
          InternetHealth'
            Prelude.<$> (x Data..:? "Availability")
            Prelude.<*> (x Data..:? "Performance")
      )

instance Prelude.Hashable InternetHealth where
  hashWithSalt _salt InternetHealth' {..} =
    _salt
      `Prelude.hashWithSalt` availability
      `Prelude.hashWithSalt` performance

instance Prelude.NFData InternetHealth where
  rnf InternetHealth' {..} =
    Prelude.rnf availability
      `Prelude.seq` Prelude.rnf performance
