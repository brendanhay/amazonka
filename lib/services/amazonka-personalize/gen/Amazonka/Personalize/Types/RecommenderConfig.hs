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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.RecommenderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types.TrainingDataConfig
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
    -- second that Amazon Personalize will support. A high
    -- @minRecommendationRequestsPerSecond@ will increase your bill. We
    -- recommend starting with 1 for @minRecommendationRequestsPerSecond@ (the
    -- default). Track your usage using Amazon CloudWatch metrics, and increase
    -- the @minRecommendationRequestsPerSecond@ as necessary.
    minRecommendationRequestsPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the training data configuration to use when creating a domain
    -- recommender.
    trainingDataConfig :: Prelude.Maybe TrainingDataConfig
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
-- second that Amazon Personalize will support. A high
-- @minRecommendationRequestsPerSecond@ will increase your bill. We
-- recommend starting with 1 for @minRecommendationRequestsPerSecond@ (the
-- default). Track your usage using Amazon CloudWatch metrics, and increase
-- the @minRecommendationRequestsPerSecond@ as necessary.
--
-- 'trainingDataConfig', 'recommenderConfig_trainingDataConfig' - Specifies the training data configuration to use when creating a domain
-- recommender.
newRecommenderConfig ::
  RecommenderConfig
newRecommenderConfig =
  RecommenderConfig'
    { itemExplorationConfig =
        Prelude.Nothing,
      minRecommendationRequestsPerSecond = Prelude.Nothing,
      trainingDataConfig = Prelude.Nothing
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
-- second that Amazon Personalize will support. A high
-- @minRecommendationRequestsPerSecond@ will increase your bill. We
-- recommend starting with 1 for @minRecommendationRequestsPerSecond@ (the
-- default). Track your usage using Amazon CloudWatch metrics, and increase
-- the @minRecommendationRequestsPerSecond@ as necessary.
recommenderConfig_minRecommendationRequestsPerSecond :: Lens.Lens' RecommenderConfig (Prelude.Maybe Prelude.Natural)
recommenderConfig_minRecommendationRequestsPerSecond = Lens.lens (\RecommenderConfig' {minRecommendationRequestsPerSecond} -> minRecommendationRequestsPerSecond) (\s@RecommenderConfig' {} a -> s {minRecommendationRequestsPerSecond = a} :: RecommenderConfig)

-- | Specifies the training data configuration to use when creating a domain
-- recommender.
recommenderConfig_trainingDataConfig :: Lens.Lens' RecommenderConfig (Prelude.Maybe TrainingDataConfig)
recommenderConfig_trainingDataConfig = Lens.lens (\RecommenderConfig' {trainingDataConfig} -> trainingDataConfig) (\s@RecommenderConfig' {} a -> s {trainingDataConfig = a} :: RecommenderConfig)

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
            Prelude.<*> (x Data..:? "trainingDataConfig")
      )

instance Prelude.Hashable RecommenderConfig where
  hashWithSalt _salt RecommenderConfig' {..} =
    _salt
      `Prelude.hashWithSalt` itemExplorationConfig
      `Prelude.hashWithSalt` minRecommendationRequestsPerSecond
      `Prelude.hashWithSalt` trainingDataConfig

instance Prelude.NFData RecommenderConfig where
  rnf RecommenderConfig' {..} =
    Prelude.rnf itemExplorationConfig
      `Prelude.seq` Prelude.rnf minRecommendationRequestsPerSecond
      `Prelude.seq` Prelude.rnf trainingDataConfig

instance Data.ToJSON RecommenderConfig where
  toJSON RecommenderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("itemExplorationConfig" Data..=)
              Prelude.<$> itemExplorationConfig,
            ("minRecommendationRequestsPerSecond" Data..=)
              Prelude.<$> minRecommendationRequestsPerSecond,
            ("trainingDataConfig" Data..=)
              Prelude.<$> trainingDataConfig
          ]
      )
