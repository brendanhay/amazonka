-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TrainingDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingDataResult
  ( TrainingDataResult (..),

    -- * Smart constructor
    mkTrainingDataResult,

    -- * Lenses
    tInput,
    tOutput,
    tValidation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and validation datasets that are used and created during testing.
--
-- /See:/ 'mkTrainingDataResult' smart constructor.
data TrainingDataResult = TrainingDataResult'
  { input ::
      Lude.Maybe TrainingData,
    output :: Lude.Maybe TrainingData,
    validation :: Lude.Maybe ValidationData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingDataResult' with the minimum fields required to make a request.
--
-- * 'input' - The training assets that you supplied for training.
-- * 'output' - The images (assets) that were actually trained by Amazon Rekognition Custom Labels.
-- * 'validation' - The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
mkTrainingDataResult ::
  TrainingDataResult
mkTrainingDataResult =
  TrainingDataResult'
    { input = Lude.Nothing,
      output = Lude.Nothing,
      validation = Lude.Nothing
    }

-- | The training assets that you supplied for training.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tInput :: Lens.Lens' TrainingDataResult (Lude.Maybe TrainingData)
tInput = Lens.lens (input :: TrainingDataResult -> Lude.Maybe TrainingData) (\s a -> s {input = a} :: TrainingDataResult)
{-# DEPRECATED tInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The images (assets) that were actually trained by Amazon Rekognition Custom Labels.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tOutput :: Lens.Lens' TrainingDataResult (Lude.Maybe TrainingData)
tOutput = Lens.lens (output :: TrainingDataResult -> Lude.Maybe TrainingData) (\s a -> s {output = a} :: TrainingDataResult)
{-# DEPRECATED tOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The location of the data validation manifest. The data validation manifest is created for the training dataset during model training.
--
-- /Note:/ Consider using 'validation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValidation :: Lens.Lens' TrainingDataResult (Lude.Maybe ValidationData)
tValidation = Lens.lens (validation :: TrainingDataResult -> Lude.Maybe ValidationData) (\s a -> s {validation = a} :: TrainingDataResult)
{-# DEPRECATED tValidation "Use generic-lens or generic-optics with 'validation' instead." #-}

instance Lude.FromJSON TrainingDataResult where
  parseJSON =
    Lude.withObject
      "TrainingDataResult"
      ( \x ->
          TrainingDataResult'
            Lude.<$> (x Lude..:? "Input")
            Lude.<*> (x Lude..:? "Output")
            Lude.<*> (x Lude..:? "Validation")
      )
