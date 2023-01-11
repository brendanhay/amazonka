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
-- Module      : Amazonka.MachineLearning.Types.Prediction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.Prediction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MachineLearning.Types.DetailsAttributes
import qualified Amazonka.Prelude as Prelude

-- | The output from a @Predict@ operation:
--
-- -   @Details@ - Contains the following attributes:
--     @DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS@
--     @DetailsAttributes.ALGORITHM - SGD@
--
-- -   @PredictedLabel@ - Present for either a @BINARY@ or @MULTICLASS@
--     @MLModel@ request.
--
-- -   @PredictedScores@ - Contains the raw classification score
--     corresponding to each label.
--
-- -   @PredictedValue@ - Present for a @REGRESSION@ @MLModel@ request.
--
-- /See:/ 'newPrediction' smart constructor.
data Prediction = Prediction'
  { details :: Prelude.Maybe (Prelude.HashMap DetailsAttributes Prelude.Text),
    -- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
    predictedLabel :: Prelude.Maybe Prelude.Text,
    predictedScores :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The prediction value for @REGRESSION@ @MLModel@.
    predictedValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Prediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'prediction_details' - Undocumented member.
--
-- 'predictedLabel', 'prediction_predictedLabel' - The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
--
-- 'predictedScores', 'prediction_predictedScores' - Undocumented member.
--
-- 'predictedValue', 'prediction_predictedValue' - The prediction value for @REGRESSION@ @MLModel@.
newPrediction ::
  Prediction
newPrediction =
  Prediction'
    { details = Prelude.Nothing,
      predictedLabel = Prelude.Nothing,
      predictedScores = Prelude.Nothing,
      predictedValue = Prelude.Nothing
    }

-- | Undocumented member.
prediction_details :: Lens.Lens' Prediction (Prelude.Maybe (Prelude.HashMap DetailsAttributes Prelude.Text))
prediction_details = Lens.lens (\Prediction' {details} -> details) (\s@Prediction' {} a -> s {details = a} :: Prediction) Prelude.. Lens.mapping Lens.coerced

-- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
prediction_predictedLabel :: Lens.Lens' Prediction (Prelude.Maybe Prelude.Text)
prediction_predictedLabel = Lens.lens (\Prediction' {predictedLabel} -> predictedLabel) (\s@Prediction' {} a -> s {predictedLabel = a} :: Prediction)

-- | Undocumented member.
prediction_predictedScores :: Lens.Lens' Prediction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
prediction_predictedScores = Lens.lens (\Prediction' {predictedScores} -> predictedScores) (\s@Prediction' {} a -> s {predictedScores = a} :: Prediction) Prelude.. Lens.mapping Lens.coerced

-- | The prediction value for @REGRESSION@ @MLModel@.
prediction_predictedValue :: Lens.Lens' Prediction (Prelude.Maybe Prelude.Double)
prediction_predictedValue = Lens.lens (\Prediction' {predictedValue} -> predictedValue) (\s@Prediction' {} a -> s {predictedValue = a} :: Prediction)

instance Data.FromJSON Prediction where
  parseJSON =
    Data.withObject
      "Prediction"
      ( \x ->
          Prediction'
            Prelude.<$> (x Data..:? "details" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "predictedLabel")
            Prelude.<*> ( x Data..:? "predictedScores"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "predictedValue")
      )

instance Prelude.Hashable Prediction where
  hashWithSalt _salt Prediction' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` predictedLabel
      `Prelude.hashWithSalt` predictedScores
      `Prelude.hashWithSalt` predictedValue

instance Prelude.NFData Prediction where
  rnf Prediction' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf predictedLabel
      `Prelude.seq` Prelude.rnf predictedScores
      `Prelude.seq` Prelude.rnf predictedValue
