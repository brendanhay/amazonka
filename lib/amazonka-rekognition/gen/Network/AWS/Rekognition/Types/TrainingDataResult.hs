{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.TrainingDataResult
  ( TrainingDataResult (..)
  -- * Smart constructor
  , mkTrainingDataResult
  -- * Lenses
  , tInput
  , tOutput
  , tValidation
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.TrainingData as Types
import qualified Network.AWS.Rekognition.Types.ValidationData as Types

-- | Sagemaker Groundtruth format manifest files for the input, output and validation datasets that are used and created during testing.
--
-- /See:/ 'mkTrainingDataResult' smart constructor.
data TrainingDataResult = TrainingDataResult'
  { input :: Core.Maybe Types.TrainingData
    -- ^ The training assets that you supplied for training.
  , output :: Core.Maybe Types.TrainingData
    -- ^ The images (assets) that were actually trained by Amazon Rekognition Custom Labels. 
  , validation :: Core.Maybe Types.ValidationData
    -- ^ The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrainingDataResult' value with any optional fields omitted.
mkTrainingDataResult
    :: TrainingDataResult
mkTrainingDataResult
  = TrainingDataResult'{input = Core.Nothing, output = Core.Nothing,
                        validation = Core.Nothing}

-- | The training assets that you supplied for training.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInput :: Lens.Lens' TrainingDataResult (Core.Maybe Types.TrainingData)
tInput = Lens.field @"input"
{-# INLINEABLE tInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | The images (assets) that were actually trained by Amazon Rekognition Custom Labels. 
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOutput :: Lens.Lens' TrainingDataResult (Core.Maybe Types.TrainingData)
tOutput = Lens.field @"output"
{-# INLINEABLE tOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
--
-- /Note:/ Consider using 'validation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValidation :: Lens.Lens' TrainingDataResult (Core.Maybe Types.ValidationData)
tValidation = Lens.field @"validation"
{-# INLINEABLE tValidation #-}
{-# DEPRECATED validation "Use generic-lens or generic-optics with 'validation' instead"  #-}

instance Core.FromJSON TrainingDataResult where
        parseJSON
          = Core.withObject "TrainingDataResult" Core.$
              \ x ->
                TrainingDataResult' Core.<$>
                  (x Core..:? "Input") Core.<*> x Core..:? "Output" Core.<*>
                    x Core..:? "Validation"
