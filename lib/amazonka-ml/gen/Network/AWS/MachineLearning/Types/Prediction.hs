{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Prediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Prediction
  ( Prediction (..),

    -- * Smart constructor
    mkPrediction,

    -- * Lenses
    pPredictedValue,
    pPredictedLabel,
    pPredictedScores,
    pDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.DetailsAttributes
import qualified Network.AWS.Prelude as Lude

-- | The output from a @Predict@ operation:
--
--
--     * @Details@ - Contains the following attributes: @DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS@ @DetailsAttributes.ALGORITHM - SGD@
--
--
--     * @PredictedLabel@ - Present for either a @BINARY@ or @MULTICLASS@ @MLModel@ request.
--
--
--     * @PredictedScores@ - Contains the raw classification score corresponding to each label.
--
--
--     * @PredictedValue@ - Present for a @REGRESSION@ @MLModel@ request.
--
--
--
-- /See:/ 'mkPrediction' smart constructor.
data Prediction = Prediction'
  { -- | The prediction value for @REGRESSION@ @MLModel@ .
    predictedValue :: Lude.Maybe Lude.Double,
    -- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
    predictedLabel :: Lude.Maybe Lude.Text,
    predictedScores :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)),
    details :: Lude.Maybe (Lude.HashMap DetailsAttributes (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Prediction' with the minimum fields required to make a request.
--
-- * 'predictedValue' - The prediction value for @REGRESSION@ @MLModel@ .
-- * 'predictedLabel' - The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
-- * 'predictedScores' -
-- * 'details' -
mkPrediction ::
  Prediction
mkPrediction =
  Prediction'
    { predictedValue = Lude.Nothing,
      predictedLabel = Lude.Nothing,
      predictedScores = Lude.Nothing,
      details = Lude.Nothing
    }

-- | The prediction value for @REGRESSION@ @MLModel@ .
--
-- /Note:/ Consider using 'predictedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedValue :: Lens.Lens' Prediction (Lude.Maybe Lude.Double)
pPredictedValue = Lens.lens (predictedValue :: Prediction -> Lude.Maybe Lude.Double) (\s a -> s {predictedValue = a} :: Prediction)
{-# DEPRECATED pPredictedValue "Use generic-lens or generic-optics with 'predictedValue' instead." #-}

-- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
--
-- /Note:/ Consider using 'predictedLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedLabel :: Lens.Lens' Prediction (Lude.Maybe Lude.Text)
pPredictedLabel = Lens.lens (predictedLabel :: Prediction -> Lude.Maybe Lude.Text) (\s a -> s {predictedLabel = a} :: Prediction)
{-# DEPRECATED pPredictedLabel "Use generic-lens or generic-optics with 'predictedLabel' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'predictedScores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedScores :: Lens.Lens' Prediction (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
pPredictedScores = Lens.lens (predictedScores :: Prediction -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {predictedScores = a} :: Prediction)
{-# DEPRECATED pPredictedScores "Use generic-lens or generic-optics with 'predictedScores' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDetails :: Lens.Lens' Prediction (Lude.Maybe (Lude.HashMap DetailsAttributes (Lude.Text)))
pDetails = Lens.lens (details :: Prediction -> Lude.Maybe (Lude.HashMap DetailsAttributes (Lude.Text))) (\s a -> s {details = a} :: Prediction)
{-# DEPRECATED pDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromJSON Prediction where
  parseJSON =
    Lude.withObject
      "Prediction"
      ( \x ->
          Prediction'
            Lude.<$> (x Lude..:? "predictedValue")
            Lude.<*> (x Lude..:? "predictedLabel")
            Lude.<*> (x Lude..:? "predictedScores" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "details" Lude..!= Lude.mempty)
      )
