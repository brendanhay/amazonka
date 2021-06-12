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
-- Module      : Network.AWS.MachineLearning.Types.Prediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Prediction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.DetailsAttributes

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
  { -- | The prediction value for @REGRESSION@ @MLModel@.
    predictedValue :: Core.Maybe Core.Double,
    predictedScores :: Core.Maybe (Core.HashMap Core.Text Core.Double),
    -- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
    predictedLabel :: Core.Maybe Core.Text,
    details :: Core.Maybe (Core.HashMap DetailsAttributes Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Prediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictedValue', 'prediction_predictedValue' - The prediction value for @REGRESSION@ @MLModel@.
--
-- 'predictedScores', 'prediction_predictedScores' - Undocumented member.
--
-- 'predictedLabel', 'prediction_predictedLabel' - The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
--
-- 'details', 'prediction_details' - Undocumented member.
newPrediction ::
  Prediction
newPrediction =
  Prediction'
    { predictedValue = Core.Nothing,
      predictedScores = Core.Nothing,
      predictedLabel = Core.Nothing,
      details = Core.Nothing
    }

-- | The prediction value for @REGRESSION@ @MLModel@.
prediction_predictedValue :: Lens.Lens' Prediction (Core.Maybe Core.Double)
prediction_predictedValue = Lens.lens (\Prediction' {predictedValue} -> predictedValue) (\s@Prediction' {} a -> s {predictedValue = a} :: Prediction)

-- | Undocumented member.
prediction_predictedScores :: Lens.Lens' Prediction (Core.Maybe (Core.HashMap Core.Text Core.Double))
prediction_predictedScores = Lens.lens (\Prediction' {predictedScores} -> predictedScores) (\s@Prediction' {} a -> s {predictedScores = a} :: Prediction) Core.. Lens.mapping Lens._Coerce

-- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@.
prediction_predictedLabel :: Lens.Lens' Prediction (Core.Maybe Core.Text)
prediction_predictedLabel = Lens.lens (\Prediction' {predictedLabel} -> predictedLabel) (\s@Prediction' {} a -> s {predictedLabel = a} :: Prediction)

-- | Undocumented member.
prediction_details :: Lens.Lens' Prediction (Core.Maybe (Core.HashMap DetailsAttributes Core.Text))
prediction_details = Lens.lens (\Prediction' {details} -> details) (\s@Prediction' {} a -> s {details = a} :: Prediction) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Prediction where
  parseJSON =
    Core.withObject
      "Prediction"
      ( \x ->
          Prediction'
            Core.<$> (x Core..:? "predictedValue")
            Core.<*> (x Core..:? "predictedScores" Core..!= Core.mempty)
            Core.<*> (x Core..:? "predictedLabel")
            Core.<*> (x Core..:? "details" Core..!= Core.mempty)
      )

instance Core.Hashable Prediction

instance Core.NFData Prediction
