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
-- Module      : Amazonka.ComputeOptimizer.Types.VolumeRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.VolumeRecommendation where

import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
import Amazonka.ComputeOptimizer.Types.EBSFinding
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Elastic Block Store (Amazon EBS) volume
-- recommendation.
--
-- /See:/ 'newVolumeRecommendation' smart constructor.
data VolumeRecommendation = VolumeRecommendation'
  { -- | The Amazon Web Services account ID of the volume.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the current configuration of the
    -- volume.
    currentConfiguration :: Prelude.Maybe VolumeConfiguration,
    -- | The risk of the current EBS volume not meeting the performance needs of
    -- its workloads. The higher the risk, the more likely the current EBS
    -- volume doesn\'t have sufficient capacity.
    currentPerformanceRisk :: Prelude.Maybe CurrentPerformanceRisk,
    -- | The finding classification of the volume.
    --
    -- Findings for volumes include:
    --
    -- -   __@NotOptimized@__ —A volume is considered not optimized when
    --     Compute Optimizer identifies a recommendation that can provide
    --     better performance for your workload.
    --
    -- -   __@Optimized@__ —An volume is considered optimized when Compute
    --     Optimizer determines that the volume is correctly provisioned to run
    --     your workload based on the chosen volume type. For optimized
    --     resources, Compute Optimizer might recommend a new generation volume
    --     type.
    finding :: Prelude.Maybe EBSFinding,
    -- | The timestamp of when the volume recommendation was last generated.
    lastRefreshTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of days for which utilization metrics were analyzed for the
    -- volume.
    lookBackPeriodInDays :: Prelude.Maybe Prelude.Double,
    -- | An array of objects that describe the utilization metrics of the volume.
    utilizationMetrics :: Prelude.Maybe [EBSUtilizationMetric],
    -- | The Amazon Resource Name (ARN) of the current volume.
    volumeArn :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the recommendation options for the
    -- volume.
    volumeRecommendationOptions :: Prelude.Maybe [VolumeRecommendationOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'volumeRecommendation_accountId' - The Amazon Web Services account ID of the volume.
--
-- 'currentConfiguration', 'volumeRecommendation_currentConfiguration' - An array of objects that describe the current configuration of the
-- volume.
--
-- 'currentPerformanceRisk', 'volumeRecommendation_currentPerformanceRisk' - The risk of the current EBS volume not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current EBS
-- volume doesn\'t have sufficient capacity.
--
-- 'finding', 'volumeRecommendation_finding' - The finding classification of the volume.
--
-- Findings for volumes include:
--
-- -   __@NotOptimized@__ —A volume is considered not optimized when
--     Compute Optimizer identifies a recommendation that can provide
--     better performance for your workload.
--
-- -   __@Optimized@__ —An volume is considered optimized when Compute
--     Optimizer determines that the volume is correctly provisioned to run
--     your workload based on the chosen volume type. For optimized
--     resources, Compute Optimizer might recommend a new generation volume
--     type.
--
-- 'lastRefreshTimestamp', 'volumeRecommendation_lastRefreshTimestamp' - The timestamp of when the volume recommendation was last generated.
--
-- 'lookBackPeriodInDays', 'volumeRecommendation_lookBackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- volume.
--
-- 'utilizationMetrics', 'volumeRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the volume.
--
-- 'volumeArn', 'volumeRecommendation_volumeArn' - The Amazon Resource Name (ARN) of the current volume.
--
-- 'volumeRecommendationOptions', 'volumeRecommendation_volumeRecommendationOptions' - An array of objects that describe the recommendation options for the
-- volume.
newVolumeRecommendation ::
  VolumeRecommendation
newVolumeRecommendation =
  VolumeRecommendation'
    { accountId = Prelude.Nothing,
      currentConfiguration = Prelude.Nothing,
      currentPerformanceRisk = Prelude.Nothing,
      finding = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      lookBackPeriodInDays = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing,
      volumeArn = Prelude.Nothing,
      volumeRecommendationOptions = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the volume.
volumeRecommendation_accountId :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Text)
volumeRecommendation_accountId = Lens.lens (\VolumeRecommendation' {accountId} -> accountId) (\s@VolumeRecommendation' {} a -> s {accountId = a} :: VolumeRecommendation)

-- | An array of objects that describe the current configuration of the
-- volume.
volumeRecommendation_currentConfiguration :: Lens.Lens' VolumeRecommendation (Prelude.Maybe VolumeConfiguration)
volumeRecommendation_currentConfiguration = Lens.lens (\VolumeRecommendation' {currentConfiguration} -> currentConfiguration) (\s@VolumeRecommendation' {} a -> s {currentConfiguration = a} :: VolumeRecommendation)

-- | The risk of the current EBS volume not meeting the performance needs of
-- its workloads. The higher the risk, the more likely the current EBS
-- volume doesn\'t have sufficient capacity.
volumeRecommendation_currentPerformanceRisk :: Lens.Lens' VolumeRecommendation (Prelude.Maybe CurrentPerformanceRisk)
volumeRecommendation_currentPerformanceRisk = Lens.lens (\VolumeRecommendation' {currentPerformanceRisk} -> currentPerformanceRisk) (\s@VolumeRecommendation' {} a -> s {currentPerformanceRisk = a} :: VolumeRecommendation)

-- | The finding classification of the volume.
--
-- Findings for volumes include:
--
-- -   __@NotOptimized@__ —A volume is considered not optimized when
--     Compute Optimizer identifies a recommendation that can provide
--     better performance for your workload.
--
-- -   __@Optimized@__ —An volume is considered optimized when Compute
--     Optimizer determines that the volume is correctly provisioned to run
--     your workload based on the chosen volume type. For optimized
--     resources, Compute Optimizer might recommend a new generation volume
--     type.
volumeRecommendation_finding :: Lens.Lens' VolumeRecommendation (Prelude.Maybe EBSFinding)
volumeRecommendation_finding = Lens.lens (\VolumeRecommendation' {finding} -> finding) (\s@VolumeRecommendation' {} a -> s {finding = a} :: VolumeRecommendation)

-- | The timestamp of when the volume recommendation was last generated.
volumeRecommendation_lastRefreshTimestamp :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.UTCTime)
volumeRecommendation_lastRefreshTimestamp = Lens.lens (\VolumeRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@VolumeRecommendation' {} a -> s {lastRefreshTimestamp = a} :: VolumeRecommendation) Prelude.. Lens.mapping Data._Time

-- | The number of days for which utilization metrics were analyzed for the
-- volume.
volumeRecommendation_lookBackPeriodInDays :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Double)
volumeRecommendation_lookBackPeriodInDays = Lens.lens (\VolumeRecommendation' {lookBackPeriodInDays} -> lookBackPeriodInDays) (\s@VolumeRecommendation' {} a -> s {lookBackPeriodInDays = a} :: VolumeRecommendation)

-- | An array of objects that describe the utilization metrics of the volume.
volumeRecommendation_utilizationMetrics :: Lens.Lens' VolumeRecommendation (Prelude.Maybe [EBSUtilizationMetric])
volumeRecommendation_utilizationMetrics = Lens.lens (\VolumeRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@VolumeRecommendation' {} a -> s {utilizationMetrics = a} :: VolumeRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the current volume.
volumeRecommendation_volumeArn :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Text)
volumeRecommendation_volumeArn = Lens.lens (\VolumeRecommendation' {volumeArn} -> volumeArn) (\s@VolumeRecommendation' {} a -> s {volumeArn = a} :: VolumeRecommendation)

-- | An array of objects that describe the recommendation options for the
-- volume.
volumeRecommendation_volumeRecommendationOptions :: Lens.Lens' VolumeRecommendation (Prelude.Maybe [VolumeRecommendationOption])
volumeRecommendation_volumeRecommendationOptions = Lens.lens (\VolumeRecommendation' {volumeRecommendationOptions} -> volumeRecommendationOptions) (\s@VolumeRecommendation' {} a -> s {volumeRecommendationOptions = a} :: VolumeRecommendation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VolumeRecommendation where
  parseJSON =
    Data.withObject
      "VolumeRecommendation"
      ( \x ->
          VolumeRecommendation'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "currentConfiguration")
            Prelude.<*> (x Data..:? "currentPerformanceRisk")
            Prelude.<*> (x Data..:? "finding")
            Prelude.<*> (x Data..:? "lastRefreshTimestamp")
            Prelude.<*> (x Data..:? "lookBackPeriodInDays")
            Prelude.<*> ( x
                            Data..:? "utilizationMetrics"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "volumeArn")
            Prelude.<*> ( x
                            Data..:? "volumeRecommendationOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VolumeRecommendation where
  hashWithSalt _salt VolumeRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentConfiguration
      `Prelude.hashWithSalt` currentPerformanceRisk
      `Prelude.hashWithSalt` finding
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` lookBackPeriodInDays
      `Prelude.hashWithSalt` utilizationMetrics
      `Prelude.hashWithSalt` volumeArn
      `Prelude.hashWithSalt` volumeRecommendationOptions

instance Prelude.NFData VolumeRecommendation where
  rnf VolumeRecommendation' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf currentConfiguration `Prelude.seq`
        Prelude.rnf currentPerformanceRisk `Prelude.seq`
          Prelude.rnf finding `Prelude.seq`
            Prelude.rnf lastRefreshTimestamp `Prelude.seq`
              Prelude.rnf lookBackPeriodInDays `Prelude.seq`
                Prelude.rnf utilizationMetrics `Prelude.seq`
                  Prelude.rnf volumeArn `Prelude.seq`
                    Prelude.rnf volumeRecommendationOptions
