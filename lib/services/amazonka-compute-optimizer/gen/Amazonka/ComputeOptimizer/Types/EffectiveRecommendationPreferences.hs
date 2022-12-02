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
-- Module      : Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EffectiveRecommendationPreferences where

import Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
import Amazonka.ComputeOptimizer.Types.EnhancedInfrastructureMetrics
import Amazonka.ComputeOptimizer.Types.InferredWorkloadTypesPreference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the effective recommendation preferences for a resource.
--
-- /See:/ 'newEffectiveRecommendationPreferences' smart constructor.
data EffectiveRecommendationPreferences = EffectiveRecommendationPreferences'
  { -- | Describes the activation status of the inferred workload types
    -- preference.
    --
    -- A status of @Active@ confirms that the preference is applied in the
    -- latest recommendation refresh. A status of @Inactive@ confirms that
    -- it\'s not yet applied to recommendations.
    inferredWorkloadTypes :: Prelude.Maybe InferredWorkloadTypesPreference,
    -- | Describes the activation status of the enhanced infrastructure metrics
    -- preference.
    --
    -- A status of @Active@ confirms that the preference is applied in the
    -- latest recommendation refresh, and a status of @Inactive@ confirms that
    -- it\'s not yet applied to recommendations.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    enhancedInfrastructureMetrics :: Prelude.Maybe EnhancedInfrastructureMetrics,
    -- | Describes the CPU vendor and architecture for an instance or Auto
    -- Scaling group recommendations.
    --
    -- For example, when you specify @AWS_ARM64@ with:
    --
    -- -   A GetEC2InstanceRecommendations or
    --     GetAutoScalingGroupRecommendations request, Compute Optimizer
    --     returns recommendations that consist of Graviton2 instance types
    --     only.
    --
    -- -   A GetEC2RecommendationProjectedMetrics request, Compute Optimizer
    --     returns projected utilization metrics for Graviton2 instance type
    --     recommendations only.
    --
    -- -   A ExportEC2InstanceRecommendations or
    --     ExportAutoScalingGroupRecommendations request, Compute Optimizer
    --     exports recommendations that consist of Graviton2 instance types
    --     only.
    cpuVendorArchitectures :: Prelude.Maybe [CpuVendorArchitecture]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EffectiveRecommendationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferredWorkloadTypes', 'effectiveRecommendationPreferences_inferredWorkloadTypes' - Describes the activation status of the inferred workload types
-- preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh. A status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
--
-- 'enhancedInfrastructureMetrics', 'effectiveRecommendationPreferences_enhancedInfrastructureMetrics' - Describes the activation status of the enhanced infrastructure metrics
-- preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh, and a status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
--
-- 'cpuVendorArchitectures', 'effectiveRecommendationPreferences_cpuVendorArchitectures' - Describes the CPU vendor and architecture for an instance or Auto
-- Scaling group recommendations.
--
-- For example, when you specify @AWS_ARM64@ with:
--
-- -   A GetEC2InstanceRecommendations or
--     GetAutoScalingGroupRecommendations request, Compute Optimizer
--     returns recommendations that consist of Graviton2 instance types
--     only.
--
-- -   A GetEC2RecommendationProjectedMetrics request, Compute Optimizer
--     returns projected utilization metrics for Graviton2 instance type
--     recommendations only.
--
-- -   A ExportEC2InstanceRecommendations or
--     ExportAutoScalingGroupRecommendations request, Compute Optimizer
--     exports recommendations that consist of Graviton2 instance types
--     only.
newEffectiveRecommendationPreferences ::
  EffectiveRecommendationPreferences
newEffectiveRecommendationPreferences =
  EffectiveRecommendationPreferences'
    { inferredWorkloadTypes =
        Prelude.Nothing,
      enhancedInfrastructureMetrics =
        Prelude.Nothing,
      cpuVendorArchitectures =
        Prelude.Nothing
    }

-- | Describes the activation status of the inferred workload types
-- preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh. A status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
effectiveRecommendationPreferences_inferredWorkloadTypes :: Lens.Lens' EffectiveRecommendationPreferences (Prelude.Maybe InferredWorkloadTypesPreference)
effectiveRecommendationPreferences_inferredWorkloadTypes = Lens.lens (\EffectiveRecommendationPreferences' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@EffectiveRecommendationPreferences' {} a -> s {inferredWorkloadTypes = a} :: EffectiveRecommendationPreferences)

-- | Describes the activation status of the enhanced infrastructure metrics
-- preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh, and a status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
effectiveRecommendationPreferences_enhancedInfrastructureMetrics :: Lens.Lens' EffectiveRecommendationPreferences (Prelude.Maybe EnhancedInfrastructureMetrics)
effectiveRecommendationPreferences_enhancedInfrastructureMetrics = Lens.lens (\EffectiveRecommendationPreferences' {enhancedInfrastructureMetrics} -> enhancedInfrastructureMetrics) (\s@EffectiveRecommendationPreferences' {} a -> s {enhancedInfrastructureMetrics = a} :: EffectiveRecommendationPreferences)

-- | Describes the CPU vendor and architecture for an instance or Auto
-- Scaling group recommendations.
--
-- For example, when you specify @AWS_ARM64@ with:
--
-- -   A GetEC2InstanceRecommendations or
--     GetAutoScalingGroupRecommendations request, Compute Optimizer
--     returns recommendations that consist of Graviton2 instance types
--     only.
--
-- -   A GetEC2RecommendationProjectedMetrics request, Compute Optimizer
--     returns projected utilization metrics for Graviton2 instance type
--     recommendations only.
--
-- -   A ExportEC2InstanceRecommendations or
--     ExportAutoScalingGroupRecommendations request, Compute Optimizer
--     exports recommendations that consist of Graviton2 instance types
--     only.
effectiveRecommendationPreferences_cpuVendorArchitectures :: Lens.Lens' EffectiveRecommendationPreferences (Prelude.Maybe [CpuVendorArchitecture])
effectiveRecommendationPreferences_cpuVendorArchitectures = Lens.lens (\EffectiveRecommendationPreferences' {cpuVendorArchitectures} -> cpuVendorArchitectures) (\s@EffectiveRecommendationPreferences' {} a -> s {cpuVendorArchitectures = a} :: EffectiveRecommendationPreferences) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    EffectiveRecommendationPreferences
  where
  parseJSON =
    Data.withObject
      "EffectiveRecommendationPreferences"
      ( \x ->
          EffectiveRecommendationPreferences'
            Prelude.<$> (x Data..:? "inferredWorkloadTypes")
            Prelude.<*> (x Data..:? "enhancedInfrastructureMetrics")
            Prelude.<*> ( x Data..:? "cpuVendorArchitectures"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    EffectiveRecommendationPreferences
  where
  hashWithSalt
    _salt
    EffectiveRecommendationPreferences' {..} =
      _salt `Prelude.hashWithSalt` inferredWorkloadTypes
        `Prelude.hashWithSalt` enhancedInfrastructureMetrics
        `Prelude.hashWithSalt` cpuVendorArchitectures

instance
  Prelude.NFData
    EffectiveRecommendationPreferences
  where
  rnf EffectiveRecommendationPreferences' {..} =
    Prelude.rnf inferredWorkloadTypes
      `Prelude.seq` Prelude.rnf enhancedInfrastructureMetrics
      `Prelude.seq` Prelude.rnf cpuVendorArchitectures
