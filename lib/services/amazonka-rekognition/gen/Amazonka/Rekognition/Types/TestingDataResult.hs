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
-- Module      : Amazonka.Rekognition.Types.TestingDataResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TestingDataResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.TestingData
import Amazonka.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and
-- validation datasets that are used and created during testing.
--
-- /See:/ 'newTestingDataResult' smart constructor.
data TestingDataResult = TestingDataResult'
  { -- | The location of the data validation manifest. The data validation
    -- manifest is created for the test dataset during model training.
    validation :: Prelude.Maybe ValidationData,
    -- | The testing dataset that was supplied for training.
    input :: Prelude.Maybe TestingData,
    -- | The subset of the dataset that was actually tested. Some images (assets)
    -- might not be tested due to file formatting and other issues.
    output :: Prelude.Maybe TestingData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestingDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validation', 'testingDataResult_validation' - The location of the data validation manifest. The data validation
-- manifest is created for the test dataset during model training.
--
-- 'input', 'testingDataResult_input' - The testing dataset that was supplied for training.
--
-- 'output', 'testingDataResult_output' - The subset of the dataset that was actually tested. Some images (assets)
-- might not be tested due to file formatting and other issues.
newTestingDataResult ::
  TestingDataResult
newTestingDataResult =
  TestingDataResult'
    { validation = Prelude.Nothing,
      input = Prelude.Nothing,
      output = Prelude.Nothing
    }

-- | The location of the data validation manifest. The data validation
-- manifest is created for the test dataset during model training.
testingDataResult_validation :: Lens.Lens' TestingDataResult (Prelude.Maybe ValidationData)
testingDataResult_validation = Lens.lens (\TestingDataResult' {validation} -> validation) (\s@TestingDataResult' {} a -> s {validation = a} :: TestingDataResult)

-- | The testing dataset that was supplied for training.
testingDataResult_input :: Lens.Lens' TestingDataResult (Prelude.Maybe TestingData)
testingDataResult_input = Lens.lens (\TestingDataResult' {input} -> input) (\s@TestingDataResult' {} a -> s {input = a} :: TestingDataResult)

-- | The subset of the dataset that was actually tested. Some images (assets)
-- might not be tested due to file formatting and other issues.
testingDataResult_output :: Lens.Lens' TestingDataResult (Prelude.Maybe TestingData)
testingDataResult_output = Lens.lens (\TestingDataResult' {output} -> output) (\s@TestingDataResult' {} a -> s {output = a} :: TestingDataResult)

instance Data.FromJSON TestingDataResult where
  parseJSON =
    Data.withObject
      "TestingDataResult"
      ( \x ->
          TestingDataResult'
            Prelude.<$> (x Data..:? "Validation")
            Prelude.<*> (x Data..:? "Input")
            Prelude.<*> (x Data..:? "Output")
      )

instance Prelude.Hashable TestingDataResult where
  hashWithSalt _salt TestingDataResult' {..} =
    _salt `Prelude.hashWithSalt` validation
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` output

instance Prelude.NFData TestingDataResult where
  rnf TestingDataResult' {..} =
    Prelude.rnf validation
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf output
