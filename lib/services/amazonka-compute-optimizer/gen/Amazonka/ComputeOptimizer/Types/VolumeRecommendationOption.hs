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
-- Module      : Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption where

import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation option for an Amazon Elastic Block Store
-- (Amazon EBS) instance.
--
-- /See:/ 'newVolumeRecommendationOption' smart constructor.
data VolumeRecommendationOption = VolumeRecommendationOption'
  { -- | The performance risk of the volume recommendation option.
    --
    -- Performance risk is the likelihood of the recommended volume type
    -- meeting the performance requirement of your workload.
    --
    -- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
    -- resource is predicted to always provide enough hardware capability. The
    -- higher the performance risk is, the more likely you should validate
    -- whether the recommendation will meet the performance requirements of
    -- your workload before migrating your resource.
    performanceRisk :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe a volume configuration.
    configuration :: Prelude.Maybe VolumeConfiguration,
    -- | An object that describes the savings opportunity for the EBS volume
    -- recommendation option. Savings opportunity includes the estimated
    -- monthly savings amount and percentage.
    savingsOpportunity :: Prelude.Maybe SavingsOpportunity,
    -- | The rank of the volume recommendation option.
    --
    -- The top recommendation option is ranked as @1@.
    rank :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeRecommendationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'performanceRisk', 'volumeRecommendationOption_performanceRisk' - The performance risk of the volume recommendation option.
--
-- Performance risk is the likelihood of the recommended volume type
-- meeting the performance requirement of your workload.
--
-- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
-- resource is predicted to always provide enough hardware capability. The
-- higher the performance risk is, the more likely you should validate
-- whether the recommendation will meet the performance requirements of
-- your workload before migrating your resource.
--
-- 'configuration', 'volumeRecommendationOption_configuration' - An array of objects that describe a volume configuration.
--
-- 'savingsOpportunity', 'volumeRecommendationOption_savingsOpportunity' - An object that describes the savings opportunity for the EBS volume
-- recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
--
-- 'rank', 'volumeRecommendationOption_rank' - The rank of the volume recommendation option.
--
-- The top recommendation option is ranked as @1@.
newVolumeRecommendationOption ::
  VolumeRecommendationOption
newVolumeRecommendationOption =
  VolumeRecommendationOption'
    { performanceRisk =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      savingsOpportunity = Prelude.Nothing,
      rank = Prelude.Nothing
    }

-- | The performance risk of the volume recommendation option.
--
-- Performance risk is the likelihood of the recommended volume type
-- meeting the performance requirement of your workload.
--
-- The value ranges from @0@ - @4@, with @0@ meaning that the recommended
-- resource is predicted to always provide enough hardware capability. The
-- higher the performance risk is, the more likely you should validate
-- whether the recommendation will meet the performance requirements of
-- your workload before migrating your resource.
volumeRecommendationOption_performanceRisk :: Lens.Lens' VolumeRecommendationOption (Prelude.Maybe Prelude.Double)
volumeRecommendationOption_performanceRisk = Lens.lens (\VolumeRecommendationOption' {performanceRisk} -> performanceRisk) (\s@VolumeRecommendationOption' {} a -> s {performanceRisk = a} :: VolumeRecommendationOption)

-- | An array of objects that describe a volume configuration.
volumeRecommendationOption_configuration :: Lens.Lens' VolumeRecommendationOption (Prelude.Maybe VolumeConfiguration)
volumeRecommendationOption_configuration = Lens.lens (\VolumeRecommendationOption' {configuration} -> configuration) (\s@VolumeRecommendationOption' {} a -> s {configuration = a} :: VolumeRecommendationOption)

-- | An object that describes the savings opportunity for the EBS volume
-- recommendation option. Savings opportunity includes the estimated
-- monthly savings amount and percentage.
volumeRecommendationOption_savingsOpportunity :: Lens.Lens' VolumeRecommendationOption (Prelude.Maybe SavingsOpportunity)
volumeRecommendationOption_savingsOpportunity = Lens.lens (\VolumeRecommendationOption' {savingsOpportunity} -> savingsOpportunity) (\s@VolumeRecommendationOption' {} a -> s {savingsOpportunity = a} :: VolumeRecommendationOption)

-- | The rank of the volume recommendation option.
--
-- The top recommendation option is ranked as @1@.
volumeRecommendationOption_rank :: Lens.Lens' VolumeRecommendationOption (Prelude.Maybe Prelude.Int)
volumeRecommendationOption_rank = Lens.lens (\VolumeRecommendationOption' {rank} -> rank) (\s@VolumeRecommendationOption' {} a -> s {rank = a} :: VolumeRecommendationOption)

instance Core.FromJSON VolumeRecommendationOption where
  parseJSON =
    Core.withObject
      "VolumeRecommendationOption"
      ( \x ->
          VolumeRecommendationOption'
            Prelude.<$> (x Core..:? "performanceRisk")
            Prelude.<*> (x Core..:? "configuration")
            Prelude.<*> (x Core..:? "savingsOpportunity")
            Prelude.<*> (x Core..:? "rank")
      )

instance Prelude.Hashable VolumeRecommendationOption where
  hashWithSalt _salt VolumeRecommendationOption' {..} =
    _salt `Prelude.hashWithSalt` performanceRisk
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` savingsOpportunity
      `Prelude.hashWithSalt` rank

instance Prelude.NFData VolumeRecommendationOption where
  rnf VolumeRecommendationOption' {..} =
    Prelude.rnf performanceRisk
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf savingsOpportunity
      `Prelude.seq` Prelude.rnf rank
