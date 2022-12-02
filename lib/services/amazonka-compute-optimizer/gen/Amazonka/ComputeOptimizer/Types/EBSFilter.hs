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
-- Module      : Amazonka.ComputeOptimizer.Types.EBSFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EBSFilter where

import Amazonka.ComputeOptimizer.Types.EBSFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification (for example, @NotOptimized@).
    name :: Prelude.Maybe EBSFilterName,
    -- | The value of the filter.
    --
    -- The valid values are @Optimized@, or @NotOptimized@.
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'name', 'eBSFilter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
--
-- 'values', 'eBSFilter_values' - The value of the filter.
--
-- The valid values are @Optimized@, or @NotOptimized@.
newEBSFilter ::
  EBSFilter
newEBSFilter =
  EBSFilter'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification (for example, @NotOptimized@).
eBSFilter_name :: Lens.Lens' EBSFilter (Prelude.Maybe EBSFilterName)
eBSFilter_name = Lens.lens (\EBSFilter' {name} -> name) (\s@EBSFilter' {} a -> s {name = a} :: EBSFilter)

-- | The value of the filter.
--
-- The valid values are @Optimized@, or @NotOptimized@.
eBSFilter_values :: Lens.Lens' EBSFilter (Prelude.Maybe [Prelude.Text])
eBSFilter_values = Lens.lens (\EBSFilter' {values} -> values) (\s@EBSFilter' {} a -> s {values = a} :: EBSFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EBSFilter where
  hashWithSalt _salt EBSFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData EBSFilter where
  rnf EBSFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON EBSFilter where
  toJSON EBSFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
