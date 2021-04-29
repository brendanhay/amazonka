{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.TestingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and
-- validation datasets that are used and created during testing.
--
-- /See:/ 'newTestingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'
  { -- | The testing dataset that was supplied for training.
    input :: Prelude.Maybe TestingData,
    -- | The subset of the dataset that was actually tested. Some images (assets)
    -- might not be tested due to file formatting and other issues.
    output :: Prelude.Maybe TestingData,
    -- | The location of the data validation manifest. The data validation
    -- manifest is created for the test dataset during model training.
    validation :: Prelude.Maybe ValidationData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { input = Prelude.Nothing,
      output = Prelude.Nothing,
      validation = Prelude.Nothing
    }

-- | The testing dataset that was supplied for training.
testingDataResult_input :: Lens.Lens' TestingDataResult (Prelude.Maybe TestingData)
testingDataResult_input = Lens.lens (\TestingDataResult' {input} -> input) (\s@TestingDataResult' {} a -> s {input = a} :: TestingDataResult)

-- | The subset of the dataset that was actually tested. Some images (assets)
-- might not be tested due to file formatting and other issues.
testingDataResult_output :: Lens.Lens' TestingDataResult (Prelude.Maybe TestingData)
testingDataResult_output = Lens.lens (\TestingDataResult' {output} -> output) (\s@TestingDataResult' {} a -> s {output = a} :: TestingDataResult)

-- | The location of the data validation manifest. The data validation
-- manifest is created for the test dataset during model training.
testingDataResult_validation :: Lens.Lens' TestingDataResult (Prelude.Maybe ValidationData)
testingDataResult_validation = Lens.lens (\TestingDataResult' {validation} -> validation) (\s@TestingDataResult' {} a -> s {validation = a} :: TestingDataResult)

instance Prelude.FromJSON TestingDataResult where
  parseJSON =
    Prelude.withObject
      "TestingDataResult"
      ( \x ->
          TestingDataResult'
            Prelude.<$> (x Prelude..:? "Input")
            Prelude.<*> (x Prelude..:? "Output")
            Prelude.<*> (x Prelude..:? "Validation")
      )

instance Prelude.Hashable TestingDataResult

instance Prelude.NFData TestingDataResult
