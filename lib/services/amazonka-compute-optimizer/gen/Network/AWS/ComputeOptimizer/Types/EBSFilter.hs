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
-- Module      : Network.AWS.ComputeOptimizer.Types.EBSFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.EBSFilter where

import Network.AWS.ComputeOptimizer.Types.EBSFilterName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a filter that returns a more specific list of Amazon Elastic
-- Block Store (Amazon EBS) volume recommendations. Use this filter with
-- the GetEBSVolumeRecommendations action.
--
-- You can use @LambdaFunctionRecommendationFilter@ with the
-- GetLambdaFunctionRecommendations action, @JobFilter@ with the
-- DescribeRecommendationExportJobs action, and @Filter@ with the
-- GetAutoScalingGroupRecommendations and GetEC2InstanceRecommendations
-- actions.
--
-- /See:/ 'newEBSFilter' smart constructor.
data EBSFilter = EBSFilter'
  { -- | The value of the filter.
    --
    -- The valid values are @Optimized@, or @NotOptimized@.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification (for example, @NotOptimized@).
    name :: Prelude.Maybe EBSFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'eBSFilter_values' - The value of the filter.
--
-- The valid values are @Optimized@, or @NotOptimized@.
--
-- 'name', 'eBSFilter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
newEBSFilter ::
  EBSFilter
newEBSFilter =
  EBSFilter'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The value of the filter.
--
-- The valid values are @Optimized@, or @NotOptimized@.
eBSFilter_values :: Lens.Lens' EBSFilter (Prelude.Maybe [Prelude.Text])
eBSFilter_values = Lens.lens (\EBSFilter' {values} -> values) (\s@EBSFilter' {} a -> s {values = a} :: EBSFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
eBSFilter_name :: Lens.Lens' EBSFilter (Prelude.Maybe EBSFilterName)
eBSFilter_name = Lens.lens (\EBSFilter' {name} -> name) (\s@EBSFilter' {} a -> s {name = a} :: EBSFilter)

instance Prelude.Hashable EBSFilter

instance Prelude.NFData EBSFilter

instance Core.ToJSON EBSFilter where
  toJSON EBSFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
