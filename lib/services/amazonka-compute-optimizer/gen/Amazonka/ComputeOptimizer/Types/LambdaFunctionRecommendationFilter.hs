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
-- Module      : Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilter where

import Amazonka.ComputeOptimizer.Types.LambdaFunctionRecommendationFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification (for example, @NotOptimized@).
    --
    -- Specify @FindingReasonCode@ to return recommendations with a specific
    -- finding reason code (for example, @MemoryUnderprovisioned@).
    name :: Prelude.Maybe LambdaFunctionRecommendationFilterName,
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
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'name', 'lambdaFunctionRecommendationFilter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code (for example, @MemoryUnderprovisioned@).
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
newLambdaFunctionRecommendationFilter ::
  LambdaFunctionRecommendationFilter
newLambdaFunctionRecommendationFilter =
  LambdaFunctionRecommendationFilter'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code (for example, @MemoryUnderprovisioned@).
lambdaFunctionRecommendationFilter_name :: Lens.Lens' LambdaFunctionRecommendationFilter (Prelude.Maybe LambdaFunctionRecommendationFilterName)
lambdaFunctionRecommendationFilter_name = Lens.lens (\LambdaFunctionRecommendationFilter' {name} -> name) (\s@LambdaFunctionRecommendationFilter' {} a -> s {name = a} :: LambdaFunctionRecommendationFilter)

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

instance
  Prelude.Hashable
    LambdaFunctionRecommendationFilter
  where
  hashWithSalt
    _salt
    LambdaFunctionRecommendationFilter' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    LambdaFunctionRecommendationFilter
  where
  rnf LambdaFunctionRecommendationFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance
  Data.ToJSON
    LambdaFunctionRecommendationFilter
  where
  toJSON LambdaFunctionRecommendationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
