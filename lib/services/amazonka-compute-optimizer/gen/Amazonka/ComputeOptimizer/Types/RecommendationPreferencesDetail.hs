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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationPreferencesDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationPreferencesDetail where

import Amazonka.ComputeOptimizer.Types.EnhancedInfrastructureMetrics
import Amazonka.ComputeOptimizer.Types.InferredWorkloadTypesPreference
import Amazonka.ComputeOptimizer.Types.ResourceType
import Amazonka.ComputeOptimizer.Types.Scope
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a recommendation preference.
--
-- /See:/ 'newRecommendationPreferencesDetail' smart constructor.
data RecommendationPreferencesDetail = RecommendationPreferencesDetail'
  { -- | The target resource type of the recommendation preference to create.
    --
    -- The @Ec2Instance@ option encompasses standalone instances and instances
    -- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
    -- encompasses only instances that are part of an Auto Scaling group.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The status of the inferred workload types recommendation preference.
    --
    -- A status of @Active@ confirms that the preference is applied in the
    -- latest recommendation refresh. A status of @Inactive@ confirms that
    -- it\'s not yet applied to recommendations.
    inferredWorkloadTypes :: Prelude.Maybe InferredWorkloadTypesPreference,
    -- | The status of the enhanced infrastructure metrics recommendation
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
    -- | An object that describes the scope of the recommendation preference.
    --
    -- Recommendation preferences can be created at the organization level (for
    -- management accounts of an organization only), account level, and
    -- resource level. For more information, see
    -- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
    -- in the /Compute Optimizer User Guide/.
    scope :: Prelude.Maybe Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationPreferencesDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'recommendationPreferencesDetail_resourceType' - The target resource type of the recommendation preference to create.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
--
-- 'inferredWorkloadTypes', 'recommendationPreferencesDetail_inferredWorkloadTypes' - The status of the inferred workload types recommendation preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh. A status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
--
-- 'enhancedInfrastructureMetrics', 'recommendationPreferencesDetail_enhancedInfrastructureMetrics' - The status of the enhanced infrastructure metrics recommendation
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
-- 'scope', 'recommendationPreferencesDetail_scope' - An object that describes the scope of the recommendation preference.
--
-- Recommendation preferences can be created at the organization level (for
-- management accounts of an organization only), account level, and
-- resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
newRecommendationPreferencesDetail ::
  RecommendationPreferencesDetail
newRecommendationPreferencesDetail =
  RecommendationPreferencesDetail'
    { resourceType =
        Prelude.Nothing,
      inferredWorkloadTypes = Prelude.Nothing,
      enhancedInfrastructureMetrics =
        Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The target resource type of the recommendation preference to create.
--
-- The @Ec2Instance@ option encompasses standalone instances and instances
-- that are part of Auto Scaling groups. The @AutoScalingGroup@ option
-- encompasses only instances that are part of an Auto Scaling group.
recommendationPreferencesDetail_resourceType :: Lens.Lens' RecommendationPreferencesDetail (Prelude.Maybe ResourceType)
recommendationPreferencesDetail_resourceType = Lens.lens (\RecommendationPreferencesDetail' {resourceType} -> resourceType) (\s@RecommendationPreferencesDetail' {} a -> s {resourceType = a} :: RecommendationPreferencesDetail)

-- | The status of the inferred workload types recommendation preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh. A status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
recommendationPreferencesDetail_inferredWorkloadTypes :: Lens.Lens' RecommendationPreferencesDetail (Prelude.Maybe InferredWorkloadTypesPreference)
recommendationPreferencesDetail_inferredWorkloadTypes = Lens.lens (\RecommendationPreferencesDetail' {inferredWorkloadTypes} -> inferredWorkloadTypes) (\s@RecommendationPreferencesDetail' {} a -> s {inferredWorkloadTypes = a} :: RecommendationPreferencesDetail)

-- | The status of the enhanced infrastructure metrics recommendation
-- preference.
--
-- A status of @Active@ confirms that the preference is applied in the
-- latest recommendation refresh, and a status of @Inactive@ confirms that
-- it\'s not yet applied to recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
recommendationPreferencesDetail_enhancedInfrastructureMetrics :: Lens.Lens' RecommendationPreferencesDetail (Prelude.Maybe EnhancedInfrastructureMetrics)
recommendationPreferencesDetail_enhancedInfrastructureMetrics = Lens.lens (\RecommendationPreferencesDetail' {enhancedInfrastructureMetrics} -> enhancedInfrastructureMetrics) (\s@RecommendationPreferencesDetail' {} a -> s {enhancedInfrastructureMetrics = a} :: RecommendationPreferencesDetail)

-- | An object that describes the scope of the recommendation preference.
--
-- Recommendation preferences can be created at the organization level (for
-- management accounts of an organization only), account level, and
-- resource level. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/enhanced-infrastructure-metrics.html Activating enhanced infrastructure metrics>
-- in the /Compute Optimizer User Guide/.
recommendationPreferencesDetail_scope :: Lens.Lens' RecommendationPreferencesDetail (Prelude.Maybe Scope)
recommendationPreferencesDetail_scope = Lens.lens (\RecommendationPreferencesDetail' {scope} -> scope) (\s@RecommendationPreferencesDetail' {} a -> s {scope = a} :: RecommendationPreferencesDetail)

instance
  Data.FromJSON
    RecommendationPreferencesDetail
  where
  parseJSON =
    Data.withObject
      "RecommendationPreferencesDetail"
      ( \x ->
          RecommendationPreferencesDetail'
            Prelude.<$> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "inferredWorkloadTypes")
            Prelude.<*> (x Data..:? "enhancedInfrastructureMetrics")
            Prelude.<*> (x Data..:? "scope")
      )

instance
  Prelude.Hashable
    RecommendationPreferencesDetail
  where
  hashWithSalt
    _salt
    RecommendationPreferencesDetail' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` inferredWorkloadTypes
        `Prelude.hashWithSalt` enhancedInfrastructureMetrics
        `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    RecommendationPreferencesDetail
  where
  rnf RecommendationPreferencesDetail' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf inferredWorkloadTypes
      `Prelude.seq` Prelude.rnf enhancedInfrastructureMetrics
      `Prelude.seq` Prelude.rnf scope
