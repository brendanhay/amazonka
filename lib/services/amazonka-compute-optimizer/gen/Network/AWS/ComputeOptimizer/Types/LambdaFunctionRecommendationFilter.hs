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
-- Module      : Network.AWS.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter where

import Network.AWS.ComputeOptimizer.Types.LambdaFunctionRecommendationFilterName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a filter that returns a more specific list of Lambda function
-- recommendations. Use this filter with the
-- GetLambdaFunctionRecommendations action.
--
-- You can use @EBSFilter@ with the GetEBSVolumeRecommendations action,
-- @JobFilter@ with the DescribeRecommendationExportJobs action, and
-- @Filter@ with the GetAutoScalingGroupRecommendations and
-- GetEC2InstanceRecommendations actions.
--
-- /See:/ 'newLambdaFunctionRecommendationFilter' smart constructor.
data LambdaFunctionRecommendationFilter = LambdaFunctionRecommendationFilter'
  { -- | The value of the filter.
    --
    -- The valid values for this parameter are as follows, depending on what
    -- you specify for the @name@ parameter:
    --
    -- -   Specify @Optimized@, @NotOptimized@, or @Unavailable@ if you specify
    --     the @name@ parameter as @Finding@.
    --
    -- -   Specify @MemoryOverprovisioned@, @MemoryUnderprovisioned@,
    --     @InsufficientData@, or @Inconclusive@ if you specify the @name@
    --     parameter as @FindingReasonCode@.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification (for example, @NotOptimized@).
    --
    -- Specify @FindingReasonCode@ to return recommendations with a specific
    -- finding reason code (for example, @MemoryUnderprovisioned@).
    name :: Prelude.Maybe LambdaFunctionRecommendationFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionRecommendationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'lambdaFunctionRecommendationFilter_values' - The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter:
--
-- -   Specify @Optimized@, @NotOptimized@, or @Unavailable@ if you specify
--     the @name@ parameter as @Finding@.
--
-- -   Specify @MemoryOverprovisioned@, @MemoryUnderprovisioned@,
--     @InsufficientData@, or @Inconclusive@ if you specify the @name@
--     parameter as @FindingReasonCode@.
--
-- 'name', 'lambdaFunctionRecommendationFilter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code (for example, @MemoryUnderprovisioned@).
newLambdaFunctionRecommendationFilter ::
  LambdaFunctionRecommendationFilter
newLambdaFunctionRecommendationFilter =
  LambdaFunctionRecommendationFilter'
    { values =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value of the filter.
--
-- The valid values for this parameter are as follows, depending on what
-- you specify for the @name@ parameter:
--
-- -   Specify @Optimized@, @NotOptimized@, or @Unavailable@ if you specify
--     the @name@ parameter as @Finding@.
--
-- -   Specify @MemoryOverprovisioned@, @MemoryUnderprovisioned@,
--     @InsufficientData@, or @Inconclusive@ if you specify the @name@
--     parameter as @FindingReasonCode@.
lambdaFunctionRecommendationFilter_values :: Lens.Lens' LambdaFunctionRecommendationFilter (Prelude.Maybe [Prelude.Text])
lambdaFunctionRecommendationFilter_values = Lens.lens (\LambdaFunctionRecommendationFilter' {values} -> values) (\s@LambdaFunctionRecommendationFilter' {} a -> s {values = a} :: LambdaFunctionRecommendationFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code (for example, @MemoryUnderprovisioned@).
lambdaFunctionRecommendationFilter_name :: Lens.Lens' LambdaFunctionRecommendationFilter (Prelude.Maybe LambdaFunctionRecommendationFilterName)
lambdaFunctionRecommendationFilter_name = Lens.lens (\LambdaFunctionRecommendationFilter' {name} -> name) (\s@LambdaFunctionRecommendationFilter' {} a -> s {name = a} :: LambdaFunctionRecommendationFilter)

instance
  Prelude.Hashable
    LambdaFunctionRecommendationFilter

instance
  Prelude.NFData
    LambdaFunctionRecommendationFilter

instance
  Core.ToJSON
    LambdaFunctionRecommendationFilter
  where
  toJSON LambdaFunctionRecommendationFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
