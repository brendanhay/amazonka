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
-- Module      : Network.AWS.Rekognition.Types.TrainingDataResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TrainingDataResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.TrainingData
import Network.AWS.Rekognition.Types.ValidationData

-- | Sagemaker Groundtruth format manifest files for the input, output and
-- validation datasets that are used and created during testing.
--
-- /See:/ 'newTrainingDataResult' smart constructor.
data TrainingDataResult = TrainingDataResult'
  { -- | The training assets that you supplied for training.
    input :: Prelude.Maybe TrainingData,
    -- | The images (assets) that were actually trained by Amazon Rekognition
    -- Custom Labels.
    output :: Prelude.Maybe TrainingData,
    -- | The location of the data validation manifest. The data validation
    -- manifest is created for the training dataset during model training.
    validation :: Prelude.Maybe ValidationData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrainingDataResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'trainingDataResult_input' - The training assets that you supplied for training.
--
-- 'output', 'trainingDataResult_output' - The images (assets) that were actually trained by Amazon Rekognition
-- Custom Labels.
--
-- 'validation', 'trainingDataResult_validation' - The location of the data validation manifest. The data validation
-- manifest is created for the training dataset during model training.
newTrainingDataResult ::
  TrainingDataResult
newTrainingDataResult =
  TrainingDataResult'
    { input = Prelude.Nothing,
      output = Prelude.Nothing,
      validation = Prelude.Nothing
    }

-- | The training assets that you supplied for training.
trainingDataResult_input :: Lens.Lens' TrainingDataResult (Prelude.Maybe TrainingData)
trainingDataResult_input = Lens.lens (\TrainingDataResult' {input} -> input) (\s@TrainingDataResult' {} a -> s {input = a} :: TrainingDataResult)

-- | The images (assets) that were actually trained by Amazon Rekognition
-- Custom Labels.
trainingDataResult_output :: Lens.Lens' TrainingDataResult (Prelude.Maybe TrainingData)
trainingDataResult_output = Lens.lens (\TrainingDataResult' {output} -> output) (\s@TrainingDataResult' {} a -> s {output = a} :: TrainingDataResult)

-- | The location of the data validation manifest. The data validation
-- manifest is created for the training dataset during model training.
trainingDataResult_validation :: Lens.Lens' TrainingDataResult (Prelude.Maybe ValidationData)
trainingDataResult_validation = Lens.lens (\TrainingDataResult' {validation} -> validation) (\s@TrainingDataResult' {} a -> s {validation = a} :: TrainingDataResult)

instance Prelude.FromJSON TrainingDataResult where
  parseJSON =
    Prelude.withObject
      "TrainingDataResult"
      ( \x ->
          TrainingDataResult'
            Prelude.<$> (x Prelude..:? "Input")
            Prelude.<*> (x Prelude..:? "Output")
            Prelude.<*> (x Prelude..:? "Validation")
      )

instance Prelude.Hashable TrainingDataResult

instance Prelude.NFData TrainingDataResult
