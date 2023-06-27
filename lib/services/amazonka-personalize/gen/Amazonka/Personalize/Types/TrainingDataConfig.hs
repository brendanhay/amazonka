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
-- Module      : Amazonka.Personalize.Types.TrainingDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.TrainingDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The training data configuration to use when creating a domain
-- recommender or custom solution version (trained model).
--
-- /See:/ 'newTrainingDataConfig' smart constructor.
data TrainingDataConfig = TrainingDataConfig'
  { -- | Specifies the columns to exclude from training. Each key is a dataset
    -- type, and each value is a list of columns. Exclude columns to control
    -- what data Amazon Personalize uses to generate recommendations. For
    -- example, you might have a column that you want to use only to filter
    -- recommendations. You can exclude this column from training and Amazon
    -- Personalize considers it only when filtering.
    excludedDatasetColumns :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludedDatasetColumns', 'trainingDataConfig_excludedDatasetColumns' - Specifies the columns to exclude from training. Each key is a dataset
-- type, and each value is a list of columns. Exclude columns to control
-- what data Amazon Personalize uses to generate recommendations. For
-- example, you might have a column that you want to use only to filter
-- recommendations. You can exclude this column from training and Amazon
-- Personalize considers it only when filtering.
newTrainingDataConfig ::
  TrainingDataConfig
newTrainingDataConfig =
  TrainingDataConfig'
    { excludedDatasetColumns =
        Prelude.Nothing
    }

-- | Specifies the columns to exclude from training. Each key is a dataset
-- type, and each value is a list of columns. Exclude columns to control
-- what data Amazon Personalize uses to generate recommendations. For
-- example, you might have a column that you want to use only to filter
-- recommendations. You can exclude this column from training and Amazon
-- Personalize considers it only when filtering.
trainingDataConfig_excludedDatasetColumns :: Lens.Lens' TrainingDataConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
trainingDataConfig_excludedDatasetColumns = Lens.lens (\TrainingDataConfig' {excludedDatasetColumns} -> excludedDatasetColumns) (\s@TrainingDataConfig' {} a -> s {excludedDatasetColumns = a} :: TrainingDataConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TrainingDataConfig where
  parseJSON =
    Data.withObject
      "TrainingDataConfig"
      ( \x ->
          TrainingDataConfig'
            Prelude.<$> ( x
                            Data..:? "excludedDatasetColumns"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TrainingDataConfig where
  hashWithSalt _salt TrainingDataConfig' {..} =
    _salt `Prelude.hashWithSalt` excludedDatasetColumns

instance Prelude.NFData TrainingDataConfig where
  rnf TrainingDataConfig' {..} =
    Prelude.rnf excludedDatasetColumns

instance Data.ToJSON TrainingDataConfig where
  toJSON TrainingDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludedDatasetColumns" Data..=)
              Prelude.<$> excludedDatasetColumns
          ]
      )
