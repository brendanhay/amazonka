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
-- Module      : Amazonka.Personalize.Types.BatchInferenceJobConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.BatchInferenceJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of a batch inference job.
--
-- /See:/ 'newBatchInferenceJobConfig' smart constructor.
data BatchInferenceJobConfig = BatchInferenceJobConfig'
  { -- | A string to string map specifying the exploration configuration
    -- hyperparameters, including @explorationWeight@ and
    -- @explorationItemAgeCutOff@, you want to use to configure the amount of
    -- item exploration Amazon Personalize uses when recommending items. See
    -- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>.
    itemExplorationConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchInferenceJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemExplorationConfig', 'batchInferenceJobConfig_itemExplorationConfig' - A string to string map specifying the exploration configuration
-- hyperparameters, including @explorationWeight@ and
-- @explorationItemAgeCutOff@, you want to use to configure the amount of
-- item exploration Amazon Personalize uses when recommending items. See
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>.
newBatchInferenceJobConfig ::
  BatchInferenceJobConfig
newBatchInferenceJobConfig =
  BatchInferenceJobConfig'
    { itemExplorationConfig =
        Prelude.Nothing
    }

-- | A string to string map specifying the exploration configuration
-- hyperparameters, including @explorationWeight@ and
-- @explorationItemAgeCutOff@, you want to use to configure the amount of
-- item exploration Amazon Personalize uses when recommending items. See
-- <https://docs.aws.amazon.com/personalize/latest/dg/native-recipe-new-item-USER_PERSONALIZATION.html User-Personalization>.
batchInferenceJobConfig_itemExplorationConfig :: Lens.Lens' BatchInferenceJobConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
batchInferenceJobConfig_itemExplorationConfig = Lens.lens (\BatchInferenceJobConfig' {itemExplorationConfig} -> itemExplorationConfig) (\s@BatchInferenceJobConfig' {} a -> s {itemExplorationConfig = a} :: BatchInferenceJobConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BatchInferenceJobConfig where
  parseJSON =
    Data.withObject
      "BatchInferenceJobConfig"
      ( \x ->
          BatchInferenceJobConfig'
            Prelude.<$> ( x Data..:? "itemExplorationConfig"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchInferenceJobConfig where
  hashWithSalt _salt BatchInferenceJobConfig' {..} =
    _salt `Prelude.hashWithSalt` itemExplorationConfig

instance Prelude.NFData BatchInferenceJobConfig where
  rnf BatchInferenceJobConfig' {..} =
    Prelude.rnf itemExplorationConfig

instance Data.ToJSON BatchInferenceJobConfig where
  toJSON BatchInferenceJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("itemExplorationConfig" Data..=)
              Prelude.<$> itemExplorationConfig
          ]
      )
