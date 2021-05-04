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
-- Module      : Network.AWS.SageMaker.Types.DataProcessing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataProcessing where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.JoinSource

-- | The data structure used to specify the data to be used for inference in
-- a batch transform job and to associate the data that is relevant to the
-- prediction results in the output. The input filter provided allows you
-- to exclude input data that is not needed for inference in a batch
-- transform job. The output filter provided allows you to include input
-- data relevant to interpreting the predictions in the output from the
-- job. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records>.
--
-- /See:/ 'newDataProcessing' smart constructor.
data DataProcessing = DataProcessing'
  { -- | A
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
    -- expression used to select a portion of the joined dataset to save in the
    -- output file for a batch transform job. If you want Amazon SageMaker to
    -- store the entire input dataset in the output file, leave the default
    -- value, @$@. If you specify indexes that aren\'t within the dimension
    -- size of the joined dataset, you get an error.
    --
    -- Examples: @\"$\"@, @\"$[0,5:]\"@, @\"$[\'id\',\'SageMakerOutput\']\"@
    outputFilter :: Prelude.Maybe Prelude.Text,
    -- | Specifies the source of the data to join with the transformed data. The
    -- valid values are @None@ and @Input@. The default value is @None@, which
    -- specifies not to join the input with the transformed data. If you want
    -- the batch transform job to join the original input data with the
    -- transformed data, set @JoinSource@ to @Input@.
    --
    -- For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker
    -- adds the transformed data to the input JSON object in an attribute
    -- called @SageMakerOutput@. The joined result for JSON must be a key-value
    -- pair object. If the input is not a key-value pair object, Amazon
    -- SageMaker creates a new JSON file. In the new JSON file, and the input
    -- data is stored under the @SageMakerInput@ key and the results are stored
    -- in @SageMakerOutput@.
    --
    -- For CSV files, Amazon SageMaker combines the transformed data with the
    -- input data at the end of the input data and stores it in the output
    -- file. The joined data has the joined input data followed by the
    -- transformed data and the output is a CSV file.
    joinSource :: Prelude.Maybe JoinSource,
    -- | A
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
    -- expression used to select a portion of the input data to pass to the
    -- algorithm. Use the @InputFilter@ parameter to exclude fields, such as an
    -- ID column, from the input. If you want Amazon SageMaker to pass the
    -- entire input dataset to the algorithm, accept the default value @$@.
    --
    -- Examples: @\"$\"@, @\"$[1:]\"@, @\"$.features\"@
    inputFilter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataProcessing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputFilter', 'dataProcessing_outputFilter' - A
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
-- expression used to select a portion of the joined dataset to save in the
-- output file for a batch transform job. If you want Amazon SageMaker to
-- store the entire input dataset in the output file, leave the default
-- value, @$@. If you specify indexes that aren\'t within the dimension
-- size of the joined dataset, you get an error.
--
-- Examples: @\"$\"@, @\"$[0,5:]\"@, @\"$[\'id\',\'SageMakerOutput\']\"@
--
-- 'joinSource', 'dataProcessing_joinSource' - Specifies the source of the data to join with the transformed data. The
-- valid values are @None@ and @Input@. The default value is @None@, which
-- specifies not to join the input with the transformed data. If you want
-- the batch transform job to join the original input data with the
-- transformed data, set @JoinSource@ to @Input@.
--
-- For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker
-- adds the transformed data to the input JSON object in an attribute
-- called @SageMakerOutput@. The joined result for JSON must be a key-value
-- pair object. If the input is not a key-value pair object, Amazon
-- SageMaker creates a new JSON file. In the new JSON file, and the input
-- data is stored under the @SageMakerInput@ key and the results are stored
-- in @SageMakerOutput@.
--
-- For CSV files, Amazon SageMaker combines the transformed data with the
-- input data at the end of the input data and stores it in the output
-- file. The joined data has the joined input data followed by the
-- transformed data and the output is a CSV file.
--
-- 'inputFilter', 'dataProcessing_inputFilter' - A
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
-- expression used to select a portion of the input data to pass to the
-- algorithm. Use the @InputFilter@ parameter to exclude fields, such as an
-- ID column, from the input. If you want Amazon SageMaker to pass the
-- entire input dataset to the algorithm, accept the default value @$@.
--
-- Examples: @\"$\"@, @\"$[1:]\"@, @\"$.features\"@
newDataProcessing ::
  DataProcessing
newDataProcessing =
  DataProcessing'
    { outputFilter = Prelude.Nothing,
      joinSource = Prelude.Nothing,
      inputFilter = Prelude.Nothing
    }

-- | A
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
-- expression used to select a portion of the joined dataset to save in the
-- output file for a batch transform job. If you want Amazon SageMaker to
-- store the entire input dataset in the output file, leave the default
-- value, @$@. If you specify indexes that aren\'t within the dimension
-- size of the joined dataset, you get an error.
--
-- Examples: @\"$\"@, @\"$[0,5:]\"@, @\"$[\'id\',\'SageMakerOutput\']\"@
dataProcessing_outputFilter :: Lens.Lens' DataProcessing (Prelude.Maybe Prelude.Text)
dataProcessing_outputFilter = Lens.lens (\DataProcessing' {outputFilter} -> outputFilter) (\s@DataProcessing' {} a -> s {outputFilter = a} :: DataProcessing)

-- | Specifies the source of the data to join with the transformed data. The
-- valid values are @None@ and @Input@. The default value is @None@, which
-- specifies not to join the input with the transformed data. If you want
-- the batch transform job to join the original input data with the
-- transformed data, set @JoinSource@ to @Input@.
--
-- For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker
-- adds the transformed data to the input JSON object in an attribute
-- called @SageMakerOutput@. The joined result for JSON must be a key-value
-- pair object. If the input is not a key-value pair object, Amazon
-- SageMaker creates a new JSON file. In the new JSON file, and the input
-- data is stored under the @SageMakerInput@ key and the results are stored
-- in @SageMakerOutput@.
--
-- For CSV files, Amazon SageMaker combines the transformed data with the
-- input data at the end of the input data and stores it in the output
-- file. The joined data has the joined input data followed by the
-- transformed data and the output is a CSV file.
dataProcessing_joinSource :: Lens.Lens' DataProcessing (Prelude.Maybe JoinSource)
dataProcessing_joinSource = Lens.lens (\DataProcessing' {joinSource} -> joinSource) (\s@DataProcessing' {} a -> s {joinSource = a} :: DataProcessing)

-- | A
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath>
-- expression used to select a portion of the input data to pass to the
-- algorithm. Use the @InputFilter@ parameter to exclude fields, such as an
-- ID column, from the input. If you want Amazon SageMaker to pass the
-- entire input dataset to the algorithm, accept the default value @$@.
--
-- Examples: @\"$\"@, @\"$[1:]\"@, @\"$.features\"@
dataProcessing_inputFilter :: Lens.Lens' DataProcessing (Prelude.Maybe Prelude.Text)
dataProcessing_inputFilter = Lens.lens (\DataProcessing' {inputFilter} -> inputFilter) (\s@DataProcessing' {} a -> s {inputFilter = a} :: DataProcessing)

instance Prelude.FromJSON DataProcessing where
  parseJSON =
    Prelude.withObject
      "DataProcessing"
      ( \x ->
          DataProcessing'
            Prelude.<$> (x Prelude..:? "OutputFilter")
            Prelude.<*> (x Prelude..:? "JoinSource")
            Prelude.<*> (x Prelude..:? "InputFilter")
      )

instance Prelude.Hashable DataProcessing

instance Prelude.NFData DataProcessing

instance Prelude.ToJSON DataProcessing where
  toJSON DataProcessing' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OutputFilter" Prelude..=)
              Prelude.<$> outputFilter,
            ("JoinSource" Prelude..=) Prelude.<$> joinSource,
            ("InputFilter" Prelude..=) Prelude.<$> inputFilter
          ]
      )
