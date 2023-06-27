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
-- Module      : Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilter where

import Amazonka.ComputeOptimizer.Types.ECSServiceRecommendationFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter that returns a more specific list of Amazon ECS
-- service recommendations. Use this filter with the
-- GetECSServiceRecommendations action.
--
-- /See:/ 'newECSServiceRecommendationFilter' smart constructor.
data ECSServiceRecommendationFilter = ECSServiceRecommendationFilter'
  { -- | The name of the filter.
    --
    -- Specify @Finding@ to return recommendations with a specific finding
    -- classification.
    --
    -- Specify @FindingReasonCode@ to return recommendations with a specific
    -- finding reason code.
    --
    -- You can filter your Amazon ECS service recommendations by @tag:key@ and
    -- @tag-key@ tags.
    --
    -- A @tag:key@ is a key and value combination of a tag assigned to your
    -- Amazon ECS service recommendations. Use the tag key in the filter name
    -- and the tag value as the filter value. For example, to find all Amazon
    -- ECS service recommendations that have a tag with the key of @Owner@ and
    -- the value of @TeamA@, specify @tag:Owner@ for the filter name and
    -- @TeamA@ for the filter value.
    --
    -- A @tag-key@ is the key of a tag assigned to your Amazon ECS service
    -- recommendations. Use this filter to find all of your Amazon ECS service
    -- recommendations that have a tag with a specific key. This doesn’t
    -- consider the tag value. For example, you can find your Amazon ECS
    -- service recommendations with a tag key value of @Owner@ or without any
    -- tag keys assigned.
    name :: Prelude.Maybe ECSServiceRecommendationFilterName,
    -- | The value of the filter.
    --
    -- The valid values for this parameter are as follows:
    --
    -- -   If you specify the @name@ parameter as @Finding@, specify
    --     @Optimized@, @NotOptimized@, or @Unavailable@.
    --
    -- -   If you specify the @name@ parameter as @FindingReasonCode@, specify
    --     @CPUUnderprovisioned@, @CPUOverprovisioned@,
    --     @MemoryUnderprovisioned@, or @MemoryOverprovisioned@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ECSServiceRecommendationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eCSServiceRecommendationFilter_name' - The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification.
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code.
--
-- You can filter your Amazon ECS service recommendations by @tag:key@ and
-- @tag-key@ tags.
--
-- A @tag:key@ is a key and value combination of a tag assigned to your
-- Amazon ECS service recommendations. Use the tag key in the filter name
-- and the tag value as the filter value. For example, to find all Amazon
-- ECS service recommendations that have a tag with the key of @Owner@ and
-- the value of @TeamA@, specify @tag:Owner@ for the filter name and
-- @TeamA@ for the filter value.
--
-- A @tag-key@ is the key of a tag assigned to your Amazon ECS service
-- recommendations. Use this filter to find all of your Amazon ECS service
-- recommendations that have a tag with a specific key. This doesn’t
-- consider the tag value. For example, you can find your Amazon ECS
-- service recommendations with a tag key value of @Owner@ or without any
-- tag keys assigned.
--
-- 'values', 'eCSServiceRecommendationFilter_values' - The value of the filter.
--
-- The valid values for this parameter are as follows:
--
-- -   If you specify the @name@ parameter as @Finding@, specify
--     @Optimized@, @NotOptimized@, or @Unavailable@.
--
-- -   If you specify the @name@ parameter as @FindingReasonCode@, specify
--     @CPUUnderprovisioned@, @CPUOverprovisioned@,
--     @MemoryUnderprovisioned@, or @MemoryOverprovisioned@.
newECSServiceRecommendationFilter ::
  ECSServiceRecommendationFilter
newECSServiceRecommendationFilter =
  ECSServiceRecommendationFilter'
    { name =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter.
--
-- Specify @Finding@ to return recommendations with a specific finding
-- classification.
--
-- Specify @FindingReasonCode@ to return recommendations with a specific
-- finding reason code.
--
-- You can filter your Amazon ECS service recommendations by @tag:key@ and
-- @tag-key@ tags.
--
-- A @tag:key@ is a key and value combination of a tag assigned to your
-- Amazon ECS service recommendations. Use the tag key in the filter name
-- and the tag value as the filter value. For example, to find all Amazon
-- ECS service recommendations that have a tag with the key of @Owner@ and
-- the value of @TeamA@, specify @tag:Owner@ for the filter name and
-- @TeamA@ for the filter value.
--
-- A @tag-key@ is the key of a tag assigned to your Amazon ECS service
-- recommendations. Use this filter to find all of your Amazon ECS service
-- recommendations that have a tag with a specific key. This doesn’t
-- consider the tag value. For example, you can find your Amazon ECS
-- service recommendations with a tag key value of @Owner@ or without any
-- tag keys assigned.
eCSServiceRecommendationFilter_name :: Lens.Lens' ECSServiceRecommendationFilter (Prelude.Maybe ECSServiceRecommendationFilterName)
eCSServiceRecommendationFilter_name = Lens.lens (\ECSServiceRecommendationFilter' {name} -> name) (\s@ECSServiceRecommendationFilter' {} a -> s {name = a} :: ECSServiceRecommendationFilter)

-- | The value of the filter.
--
-- The valid values for this parameter are as follows:
--
-- -   If you specify the @name@ parameter as @Finding@, specify
--     @Optimized@, @NotOptimized@, or @Unavailable@.
--
-- -   If you specify the @name@ parameter as @FindingReasonCode@, specify
--     @CPUUnderprovisioned@, @CPUOverprovisioned@,
--     @MemoryUnderprovisioned@, or @MemoryOverprovisioned@.
eCSServiceRecommendationFilter_values :: Lens.Lens' ECSServiceRecommendationFilter (Prelude.Maybe [Prelude.Text])
eCSServiceRecommendationFilter_values = Lens.lens (\ECSServiceRecommendationFilter' {values} -> values) (\s@ECSServiceRecommendationFilter' {} a -> s {values = a} :: ECSServiceRecommendationFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ECSServiceRecommendationFilter
  where
  hashWithSalt
    _salt
    ECSServiceRecommendationFilter' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    ECSServiceRecommendationFilter
  where
  rnf ECSServiceRecommendationFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON ECSServiceRecommendationFilter where
  toJSON ECSServiceRecommendationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
