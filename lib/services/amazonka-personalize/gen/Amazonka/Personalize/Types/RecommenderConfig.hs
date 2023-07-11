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
-- Module      : Amazonka.Personalize.Types.RecommenderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.RecommenderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of the recommender.
--
-- /See:/ 'newRecommenderConfig' smart constructor.
data RecommenderConfig = RecommenderConfig'
  { -- | Specifies the exploration configuration hyperparameters, including
    -- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
    -- configure the amount of item exploration Amazon Personalize uses when
    -- recommending items. Provide @itemExplorationConfig@ data only if your
    -- recommenders generate personalized recommendations for a user (not
    -- popular items or similar items).
    itemExplorationConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the requested minimum provisioned recommendation requests per
    -- second that Amazon Personalize will support.
    minRecommendationRequestsPerSecond :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommenderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemExplorationConfig', 'recommenderConfig_itemExplorationConfig' - Specifies the exploration configuration hyperparameters, including
-- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
-- configure the amount of item exploration Amazon Personalize uses when
-- recommending items. Provide @itemExplorationConfig@ data only if your
-- recommenders generate personalized recommendations for a user (not
-- popular items or similar items).
--
-- 'minRecommendationRequestsPerSecond', 'recommenderConfig_minRecommendationRequestsPerSecond' - Specifies the requested minimum provisioned recommendation requests per
-- second that Amazon Personalize will support.
newRecommenderConfig ::
  RecommenderConfig
newRecommenderConfig =
  RecommenderConfig'
    { itemExplorationConfig =
        Prelude.Nothing,
      minRecommendationRequestsPerSecond = Prelude.Nothing
    }

-- | Specifies the exploration configuration hyperparameters, including
-- @explorationWeight@ and @explorationItemAgeCutOff@, you want to use to
-- configure the amount of item exploration Amazon Personalize uses when
-- recommending items. Provide @itemExplorationConfig@ data only if your
-- recommenders generate personalized recommendations for a user (not
-- popular items or similar items).
recommenderConfig_itemExplorationConfig :: Lens.Lens' RecommenderConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recommenderConfig_itemExplorationConfig = Lens.lens (\RecommenderConfig' {itemExplorationConfig} -> itemExplorationConfig) (\s@RecommenderConfig' {} a -> s {itemExplorationConfig = a} :: RecommenderConfig) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the requested minimum provisioned recommendation requests per
-- second that Amazon Personalize will support.
recommenderConfig_minRecommendationRequestsPerSecond :: Lens.Lens' RecommenderConfig (Prelude.Maybe Prelude.Natural)
recommenderConfig_minRecommendationRequestsPerSecond = Lens.lens (\RecommenderConfig' {minRecommendationRequestsPerSecond} -> minRecommendationRequestsPerSecond) (\s@RecommenderConfig' {} a -> s {minRecommendationRequestsPerSecond = a} :: RecommenderConfig)

instance Data.FromJSON RecommenderConfig where
  parseJSON =
    Data.withObject
      "RecommenderConfig"
      ( \x ->
          RecommenderConfig'
            Prelude.<$> ( x
                            Data..:? "itemExplorationConfig"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "minRecommendationRequestsPerSecond")
      )

instance Prelude.Hashable RecommenderConfig where
  hashWithSalt _salt RecommenderConfig' {..} =
    _salt
      `Prelude.hashWithSalt` itemExplorationConfig
      `Prelude.hashWithSalt` minRecommendationRequestsPerSecond

instance Prelude.NFData RecommenderConfig where
  rnf RecommenderConfig' {..} =
    Prelude.rnf itemExplorationConfig
      `Prelude.seq` Prelude.rnf minRecommendationRequestsPerSecond

instance Data.ToJSON RecommenderConfig where
  toJSON RecommenderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("itemExplorationConfig" Data..=)
              Prelude.<$> itemExplorationConfig,
            ("minRecommendationRequestsPerSecond" Data..=)
              Prelude.<$> minRecommendationRequestsPerSecond
          ]
      )
