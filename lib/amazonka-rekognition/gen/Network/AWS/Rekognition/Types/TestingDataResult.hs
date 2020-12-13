{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.TestingDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TestingDataResult
  ( TestingDataResult (..),

    -- * Smart constructor
    mkTestingDataResult,

    -- * Lenses
    tdrInput,
    tdrOutput,
    tdrValidation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and validation datasets that are used and created during testing.
--
-- /See:/ 'mkTestingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'
  { -- | The testing dataset that was supplied for training.
    input :: Lude.Maybe TestingData,
    -- | The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues.
    output :: Lude.Maybe TestingData,
    -- | The location of the data validation manifest. The data validation manifest is created for the test dataset during model training.
    validation :: Lude.Maybe ValidationData
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestingDataResult' with the minimum fields required to make a request.
--
-- * 'input' - The testing dataset that was supplied for training.
-- * 'output' - The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues.
-- * 'validation' - The location of the data validation manifest. The data validation manifest is created for the test dataset during model training.
mkTestingDataResult ::
  TestingDataResult
mkTestingDataResult =
  TestingDataResult'
    { input = Lude.Nothing,
      output = Lude.Nothing,
      validation = Lude.Nothing
    }

-- | The testing dataset that was supplied for training.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrInput :: Lens.Lens' TestingDataResult (Lude.Maybe TestingData)
tdrInput = Lens.lens (input :: TestingDataResult -> Lude.Maybe TestingData) (\s a -> s {input = a} :: TestingDataResult)
{-# DEPRECATED tdrInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrOutput :: Lens.Lens' TestingDataResult (Lude.Maybe TestingData)
tdrOutput = Lens.lens (output :: TestingDataResult -> Lude.Maybe TestingData) (\s a -> s {output = a} :: TestingDataResult)
{-# DEPRECATED tdrOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The location of the data validation manifest. The data validation manifest is created for the test dataset during model training.
--
-- /Note:/ Consider using 'validation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrValidation :: Lens.Lens' TestingDataResult (Lude.Maybe ValidationData)
tdrValidation = Lens.lens (validation :: TestingDataResult -> Lude.Maybe ValidationData) (\s a -> s {validation = a} :: TestingDataResult)
{-# DEPRECATED tdrValidation "Use generic-lens or generic-optics with 'validation' instead." #-}

instance Lude.FromJSON TestingDataResult where
  parseJSON =
    Lude.withObject
      "TestingDataResult"
      ( \x ->
          TestingDataResult'
            Lude.<$> (x Lude..:? "Input")
            Lude.<*> (x Lude..:? "Output")
            Lude.<*> (x Lude..:? "Validation")
      )
