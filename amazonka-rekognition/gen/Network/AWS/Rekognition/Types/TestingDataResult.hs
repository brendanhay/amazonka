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
-- Module      : Network.AWS.Rekognition.Types.TestingDataResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TestingDataResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and
-- validation datasets that are used and created during testing.
--
-- /See:/ 'newTestingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'
  { -- | The testing dataset that was supplied for training.
    input :: Core.Maybe TestingData,
    -- | The subset of the dataset that was actually tested. Some images (assets)
    -- might not be tested due to file formatting and other issues.
    output :: Core.Maybe TestingData,
    -- | The location of the data validation manifest. The data validation
    -- manifest is created for the test dataset during model training.
    validation :: Core.Maybe ValidationData
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TestingDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'testingDataResult_input' - The testing dataset that was supplied for training.
--
-- 'output', 'testingDataResult_output' - The subset of the dataset that was actually tested. Some images (assets)
-- might not be tested due to file formatting and other issues.
--
-- 'validation', 'testingDataResult_validation' - The location of the data validation manifest. The data validation
-- manifest is created for the test dataset during model training.
newTestingDataResult ::
  TestingDataResult
newTestingDataResult =
  TestingDataResult'
    { input = Core.Nothing,
      output = Core.Nothing,
      validation = Core.Nothing
    }

-- | The testing dataset that was supplied for training.
testingDataResult_input :: Lens.Lens' TestingDataResult (Core.Maybe TestingData)
testingDataResult_input = Lens.lens (\TestingDataResult' {input} -> input) (\s@TestingDataResult' {} a -> s {input = a} :: TestingDataResult)

-- | The subset of the dataset that was actually tested. Some images (assets)
-- might not be tested due to file formatting and other issues.
testingDataResult_output :: Lens.Lens' TestingDataResult (Core.Maybe TestingData)
testingDataResult_output = Lens.lens (\TestingDataResult' {output} -> output) (\s@TestingDataResult' {} a -> s {output = a} :: TestingDataResult)

-- | The location of the data validation manifest. The data validation
-- manifest is created for the test dataset during model training.
testingDataResult_validation :: Lens.Lens' TestingDataResult (Core.Maybe ValidationData)
testingDataResult_validation = Lens.lens (\TestingDataResult' {validation} -> validation) (\s@TestingDataResult' {} a -> s {validation = a} :: TestingDataResult)

instance Core.FromJSON TestingDataResult where
  parseJSON =
    Core.withObject
      "TestingDataResult"
      ( \x ->
          TestingDataResult'
            Core.<$> (x Core..:? "Input")
            Core.<*> (x Core..:? "Output")
            Core.<*> (x Core..:? "Validation")
      )

instance Core.Hashable TestingDataResult

instance Core.NFData TestingDataResult
