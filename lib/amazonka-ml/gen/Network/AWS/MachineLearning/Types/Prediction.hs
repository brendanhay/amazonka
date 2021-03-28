{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Prediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Prediction
  ( Prediction (..)
  -- * Smart constructor
  , mkPrediction
  -- * Lenses
  , pDetails
  , pPredictedLabel
  , pPredictedScores
  , pPredictedValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.DetailsAttributes as Types
import qualified Network.AWS.MachineLearning.Types.DetailsValue as Types
import qualified Network.AWS.MachineLearning.Types.Label as Types
import qualified Network.AWS.MachineLearning.Types.PredictedLabel as Types
import qualified Network.AWS.Prelude as Core

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
  { details :: Core.Maybe (Core.HashMap Types.DetailsAttributes Types.DetailsValue)
  , predictedLabel :: Core.Maybe Types.PredictedLabel
    -- ^ The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
  , predictedScores :: Core.Maybe (Core.HashMap Types.Label Core.Double)
  , predictedValue :: Core.Maybe Core.Double
    -- ^ The prediction value for @REGRESSION@ @MLModel@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Prediction' value with any optional fields omitted.
mkPrediction
    :: Prediction
mkPrediction
  = Prediction'{details = Core.Nothing,
                predictedLabel = Core.Nothing, predictedScores = Core.Nothing,
                predictedValue = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDetails :: Lens.Lens' Prediction (Core.Maybe (Core.HashMap Types.DetailsAttributes Types.DetailsValue))
pDetails = Lens.field @"details"
{-# INLINEABLE pDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
--
-- /Note:/ Consider using 'predictedLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedLabel :: Lens.Lens' Prediction (Core.Maybe Types.PredictedLabel)
pPredictedLabel = Lens.field @"predictedLabel"
{-# INLINEABLE pPredictedLabel #-}
{-# DEPRECATED predictedLabel "Use generic-lens or generic-optics with 'predictedLabel' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'predictedScores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedScores :: Lens.Lens' Prediction (Core.Maybe (Core.HashMap Types.Label Core.Double))
pPredictedScores = Lens.field @"predictedScores"
{-# INLINEABLE pPredictedScores #-}
{-# DEPRECATED predictedScores "Use generic-lens or generic-optics with 'predictedScores' instead"  #-}

-- | The prediction value for @REGRESSION@ @MLModel@ .
--
-- /Note:/ Consider using 'predictedValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictedValue :: Lens.Lens' Prediction (Core.Maybe Core.Double)
pPredictedValue = Lens.field @"predictedValue"
{-# INLINEABLE pPredictedValue #-}
{-# DEPRECATED predictedValue "Use generic-lens or generic-optics with 'predictedValue' instead"  #-}

instance Core.FromJSON Prediction where
        parseJSON
          = Core.withObject "Prediction" Core.$
              \ x ->
                Prediction' Core.<$>
                  (x Core..:? "details") Core.<*> x Core..:? "predictedLabel"
                    Core.<*> x Core..:? "predictedScores"
                    Core.<*> x Core..:? "predictedValue"
