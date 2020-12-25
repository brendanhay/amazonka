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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.TestingData as Types
import qualified Network.AWS.Rekognition.Types.ValidationData as Types

-- | Sagemaker Groundtruth format manifest files for the input, output and validation datasets that are used and created during testing.
--
-- /See:/ 'mkTestingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'
  { -- | The testing dataset that was supplied for training.
    input :: Core.Maybe Types.TestingData,
    -- | The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues.
    output :: Core.Maybe Types.TestingData,
    -- | The location of the data validation manifest. The data validation manifest is created for the test dataset during model training.
    validation :: Core.Maybe Types.ValidationData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestingDataResult' value with any optional fields omitted.
mkTestingDataResult ::
  TestingDataResult
mkTestingDataResult =
  TestingDataResult'
    { input = Core.Nothing,
      output = Core.Nothing,
      validation = Core.Nothing
    }

-- | The testing dataset that was supplied for training.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrInput :: Lens.Lens' TestingDataResult (Core.Maybe Types.TestingData)
tdrInput = Lens.field @"input"
{-# DEPRECATED tdrInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The subset of the dataset that was actually tested. Some images (assets) might not be tested due to file formatting and other issues.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrOutput :: Lens.Lens' TestingDataResult (Core.Maybe Types.TestingData)
tdrOutput = Lens.field @"output"
{-# DEPRECATED tdrOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The location of the data validation manifest. The data validation manifest is created for the test dataset during model training.
--
-- /Note:/ Consider using 'validation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdrValidation :: Lens.Lens' TestingDataResult (Core.Maybe Types.ValidationData)
tdrValidation = Lens.field @"validation"
{-# DEPRECATED tdrValidation "Use generic-lens or generic-optics with 'validation' instead." #-}

instance Core.FromJSON TestingDataResult where
  parseJSON =
    Core.withObject "TestingDataResult" Core.$
      \x ->
        TestingDataResult'
          Core.<$> (x Core..:? "Input")
          Core.<*> (x Core..:? "Output")
          Core.<*> (x Core..:? "Validation")
