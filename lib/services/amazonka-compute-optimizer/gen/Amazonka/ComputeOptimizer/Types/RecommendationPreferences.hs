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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationPreferences where

import Amazonka.ComputeOptimizer.Types.CpuVendorArchitecture
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the recommendation preferences to return in the response of a
-- GetAutoScalingGroupRecommendations, GetEC2InstanceRecommendations, and
-- GetEC2RecommendationProjectedMetrics request.
--
-- /See:/ 'newRecommendationPreferences' smart constructor.
data RecommendationPreferences = RecommendationPreferences'
  { -- | Specifies the CPU vendor and architecture for Amazon EC2 instance and
    -- Auto Scaling group recommendations.
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
-- Create a value of 'RecommendationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuVendorArchitectures', 'recommendationPreferences_cpuVendorArchitectures' - Specifies the CPU vendor and architecture for Amazon EC2 instance and
-- Auto Scaling group recommendations.
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
newRecommendationPreferences ::
  RecommendationPreferences
newRecommendationPreferences =
  RecommendationPreferences'
    { cpuVendorArchitectures =
        Prelude.Nothing
    }

-- | Specifies the CPU vendor and architecture for Amazon EC2 instance and
-- Auto Scaling group recommendations.
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
recommendationPreferences_cpuVendorArchitectures :: Lens.Lens' RecommendationPreferences (Prelude.Maybe [CpuVendorArchitecture])
recommendationPreferences_cpuVendorArchitectures = Lens.lens (\RecommendationPreferences' {cpuVendorArchitectures} -> cpuVendorArchitectures) (\s@RecommendationPreferences' {} a -> s {cpuVendorArchitectures = a} :: RecommendationPreferences) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable RecommendationPreferences where
  hashWithSalt _salt RecommendationPreferences' {..} =
    _salt `Prelude.hashWithSalt` cpuVendorArchitectures

instance Prelude.NFData RecommendationPreferences where
  rnf RecommendationPreferences' {..} =
    Prelude.rnf cpuVendorArchitectures

instance Data.ToJSON RecommendationPreferences where
  toJSON RecommendationPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpuVendorArchitectures" Data..=)
              Prelude.<$> cpuVendorArchitectures
          ]
      )
