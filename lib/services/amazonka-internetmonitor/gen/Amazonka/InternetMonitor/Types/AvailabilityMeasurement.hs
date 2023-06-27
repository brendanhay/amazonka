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
-- Module      : Amazonka.InternetMonitor.Types.AvailabilityMeasurement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.AvailabilityMeasurement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Measurements about the availability for your application on the
-- internet, calculated by Amazon CloudWatch Internet Monitor. Amazon Web
-- Services has substantial historical data about internet performance and
-- availability between Amazon Web Services services and different network
-- providers and geographies. By applying statistical analysis to the data,
-- Internet Monitor can detect when the performance and availability for
-- your application has dropped, compared to an estimated baseline that\'s
-- already calculated. To make it easier to see those drops, we report that
-- information to you in the form of health scores: a performance score and
-- an availability score.
--
-- Availability in Internet Monitor represents the estimated percentage of
-- traffic that is not seeing an availability drop. For example, an
-- availability score of 99% for an end user and service location pair is
-- equivalent to 1% of the traffic experiencing an availability drop for
-- that pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Internet Monitor calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /Amazon
-- CloudWatch User Guide/.
--
-- /See:/ 'newAvailabilityMeasurement' smart constructor.
data AvailabilityMeasurement = AvailabilityMeasurement'
  { -- | Experience scores, or health scores are calculated for different
    -- geographic and network provider combinations (that is, different
    -- granularities) and also summed into global scores. If you view
    -- performance or availability scores without filtering for any specific
    -- geography or service provider, Amazon CloudWatch Internet Monitor
    -- provides global health scores.
    --
    -- The Amazon CloudWatch Internet Monitor chapter in the /CloudWatch User
    -- Guide/ includes detailed information about how Internet Monitor
    -- calculates health scores, including performance and availability scores,
    -- and when it creates and resolves health events. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
    -- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
    -- User Guide/.
    experienceScore :: Prelude.Maybe Prelude.Double,
    -- | The percentage of impact caused by a health event for client location
    -- traffic globally.
    --
    -- For information about how Internet Monitor calculates impact, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
    -- in the Amazon CloudWatch Internet Monitor section of the Amazon
    -- CloudWatch User Guide.
    percentOfClientLocationImpacted :: Prelude.Maybe Prelude.Double,
    -- | The percentage of impact caused by a health event for total traffic
    -- globally.
    --
    -- For information about how Internet Monitor calculates impact, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
    -- in the Amazon CloudWatch Internet Monitor section of the Amazon
    -- CloudWatch User Guide.
    percentOfTotalTrafficImpacted :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityMeasurement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experienceScore', 'availabilityMeasurement_experienceScore' - Experience scores, or health scores are calculated for different
-- geographic and network provider combinations (that is, different
-- granularities) and also summed into global scores. If you view
-- performance or availability scores without filtering for any specific
-- geography or service provider, Amazon CloudWatch Internet Monitor
-- provides global health scores.
--
-- The Amazon CloudWatch Internet Monitor chapter in the /CloudWatch User
-- Guide/ includes detailed information about how Internet Monitor
-- calculates health scores, including performance and availability scores,
-- and when it creates and resolves health events. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
--
-- 'percentOfClientLocationImpacted', 'availabilityMeasurement_percentOfClientLocationImpacted' - The percentage of impact caused by a health event for client location
-- traffic globally.
--
-- For information about how Internet Monitor calculates impact, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
-- in the Amazon CloudWatch Internet Monitor section of the Amazon
-- CloudWatch User Guide.
--
-- 'percentOfTotalTrafficImpacted', 'availabilityMeasurement_percentOfTotalTrafficImpacted' - The percentage of impact caused by a health event for total traffic
-- globally.
--
-- For information about how Internet Monitor calculates impact, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
-- in the Amazon CloudWatch Internet Monitor section of the Amazon
-- CloudWatch User Guide.
newAvailabilityMeasurement ::
  AvailabilityMeasurement
newAvailabilityMeasurement =
  AvailabilityMeasurement'
    { experienceScore =
        Prelude.Nothing,
      percentOfClientLocationImpacted = Prelude.Nothing,
      percentOfTotalTrafficImpacted = Prelude.Nothing
    }

-- | Experience scores, or health scores are calculated for different
-- geographic and network provider combinations (that is, different
-- granularities) and also summed into global scores. If you view
-- performance or availability scores without filtering for any specific
-- geography or service provider, Amazon CloudWatch Internet Monitor
-- provides global health scores.
--
-- The Amazon CloudWatch Internet Monitor chapter in the /CloudWatch User
-- Guide/ includes detailed information about how Internet Monitor
-- calculates health scores, including performance and availability scores,
-- and when it creates and resolves health events. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html#IMExperienceScores How Amazon Web Services calculates performance and availability scores>
-- in the Amazon CloudWatch Internet Monitor section of the /CloudWatch
-- User Guide/.
availabilityMeasurement_experienceScore :: Lens.Lens' AvailabilityMeasurement (Prelude.Maybe Prelude.Double)
availabilityMeasurement_experienceScore = Lens.lens (\AvailabilityMeasurement' {experienceScore} -> experienceScore) (\s@AvailabilityMeasurement' {} a -> s {experienceScore = a} :: AvailabilityMeasurement)

-- | The percentage of impact caused by a health event for client location
-- traffic globally.
--
-- For information about how Internet Monitor calculates impact, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
-- in the Amazon CloudWatch Internet Monitor section of the Amazon
-- CloudWatch User Guide.
availabilityMeasurement_percentOfClientLocationImpacted :: Lens.Lens' AvailabilityMeasurement (Prelude.Maybe Prelude.Double)
availabilityMeasurement_percentOfClientLocationImpacted = Lens.lens (\AvailabilityMeasurement' {percentOfClientLocationImpacted} -> percentOfClientLocationImpacted) (\s@AvailabilityMeasurement' {} a -> s {percentOfClientLocationImpacted = a} :: AvailabilityMeasurement)

-- | The percentage of impact caused by a health event for total traffic
-- globally.
--
-- For information about how Internet Monitor calculates impact, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-IM-inside-internet-monitor.html Inside Internet Monitor>
-- in the Amazon CloudWatch Internet Monitor section of the Amazon
-- CloudWatch User Guide.
availabilityMeasurement_percentOfTotalTrafficImpacted :: Lens.Lens' AvailabilityMeasurement (Prelude.Maybe Prelude.Double)
availabilityMeasurement_percentOfTotalTrafficImpacted = Lens.lens (\AvailabilityMeasurement' {percentOfTotalTrafficImpacted} -> percentOfTotalTrafficImpacted) (\s@AvailabilityMeasurement' {} a -> s {percentOfTotalTrafficImpacted = a} :: AvailabilityMeasurement)

instance Data.FromJSON AvailabilityMeasurement where
  parseJSON =
    Data.withObject
      "AvailabilityMeasurement"
      ( \x ->
          AvailabilityMeasurement'
            Prelude.<$> (x Data..:? "ExperienceScore")
            Prelude.<*> (x Data..:? "PercentOfClientLocationImpacted")
            Prelude.<*> (x Data..:? "PercentOfTotalTrafficImpacted")
      )

instance Prelude.Hashable AvailabilityMeasurement where
  hashWithSalt _salt AvailabilityMeasurement' {..} =
    _salt
      `Prelude.hashWithSalt` experienceScore
      `Prelude.hashWithSalt` percentOfClientLocationImpacted
      `Prelude.hashWithSalt` percentOfTotalTrafficImpacted

instance Prelude.NFData AvailabilityMeasurement where
  rnf AvailabilityMeasurement' {..} =
    Prelude.rnf experienceScore
      `Prelude.seq` Prelude.rnf percentOfClientLocationImpacted
      `Prelude.seq` Prelude.rnf percentOfTotalTrafficImpacted
