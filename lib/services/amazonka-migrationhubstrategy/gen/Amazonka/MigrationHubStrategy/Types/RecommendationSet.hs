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
-- Module      : Amazonka.MigrationHubStrategy.Types.RecommendationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RecommendationSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types.Strategy
import Amazonka.MigrationHubStrategy.Types.TargetDestination
import Amazonka.MigrationHubStrategy.Types.TransformationTool
import qualified Amazonka.Prelude as Prelude

-- | Contains a recommendation set.
--
-- /See:/ 'newRecommendationSet' smart constructor.
data RecommendationSet = RecommendationSet'
  { -- | The target destination for the recommendation set.
    transformationTool :: Prelude.Maybe TransformationTool,
    -- | The recommended target destination.
    targetDestination :: Prelude.Maybe TargetDestination,
    -- | The recommended strategy.
    strategy :: Prelude.Maybe Strategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformationTool', 'recommendationSet_transformationTool' - The target destination for the recommendation set.
--
-- 'targetDestination', 'recommendationSet_targetDestination' - The recommended target destination.
--
-- 'strategy', 'recommendationSet_strategy' - The recommended strategy.
newRecommendationSet ::
  RecommendationSet
newRecommendationSet =
  RecommendationSet'
    { transformationTool =
        Prelude.Nothing,
      targetDestination = Prelude.Nothing,
      strategy = Prelude.Nothing
    }

-- | The target destination for the recommendation set.
recommendationSet_transformationTool :: Lens.Lens' RecommendationSet (Prelude.Maybe TransformationTool)
recommendationSet_transformationTool = Lens.lens (\RecommendationSet' {transformationTool} -> transformationTool) (\s@RecommendationSet' {} a -> s {transformationTool = a} :: RecommendationSet)

-- | The recommended target destination.
recommendationSet_targetDestination :: Lens.Lens' RecommendationSet (Prelude.Maybe TargetDestination)
recommendationSet_targetDestination = Lens.lens (\RecommendationSet' {targetDestination} -> targetDestination) (\s@RecommendationSet' {} a -> s {targetDestination = a} :: RecommendationSet)

-- | The recommended strategy.
recommendationSet_strategy :: Lens.Lens' RecommendationSet (Prelude.Maybe Strategy)
recommendationSet_strategy = Lens.lens (\RecommendationSet' {strategy} -> strategy) (\s@RecommendationSet' {} a -> s {strategy = a} :: RecommendationSet)

instance Core.FromJSON RecommendationSet where
  parseJSON =
    Core.withObject
      "RecommendationSet"
      ( \x ->
          RecommendationSet'
            Prelude.<$> (x Core..:? "transformationTool")
            Prelude.<*> (x Core..:? "targetDestination")
            Prelude.<*> (x Core..:? "strategy")
      )

instance Prelude.Hashable RecommendationSet where
  hashWithSalt _salt RecommendationSet' {..} =
    _salt `Prelude.hashWithSalt` transformationTool
      `Prelude.hashWithSalt` targetDestination
      `Prelude.hashWithSalt` strategy

instance Prelude.NFData RecommendationSet where
  rnf RecommendationSet' {..} =
    Prelude.rnf transformationTool
      `Prelude.seq` Prelude.rnf targetDestination
      `Prelude.seq` Prelude.rnf strategy
