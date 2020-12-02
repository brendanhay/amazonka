{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataProcessing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataProcessing where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.JoinSource

-- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
--
--
--
-- /See:/ 'dataProcessing' smart constructor.
data DataProcessing = DataProcessing'
  { _dpOutputFilter ::
      !(Maybe Text),
    _dpJoinSource :: !(Maybe JoinSource),
    _dpInputFilter :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataProcessing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpOutputFilter' - A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the joined dataset to save in the output file for a batch transform job. If you want Amazon SageMaker to store the entire input dataset in the output file, leave the default value, @> @ . If you specify indexes that aren't within the dimension size of the joined dataset, you get an error. Examples: @"$"@ , @"$[0,5:]"@ , @"$['id','SageMakerOutput']"@
--
-- * 'dpJoinSource' - Specifies the source of the data to join with the transformed data. The valid values are @None@ and @Input@ . The default value is @None@ , which specifies not to join the input with the transformed data. If you want the batch transform job to join the original input data with the transformed data, set @JoinSource@ to @Input@ .  For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker adds the transformed data to the input JSON object in an attribute called @SageMakerOutput@ . The joined result for JSON must be a key-value pair object. If the input is not a key-value pair object, Amazon SageMaker creates a new JSON file. In the new JSON file, and the input data is stored under the @SageMakerInput@ key and the results are stored in @SageMakerOutput@ . For CSV files, Amazon SageMaker combines the transformed data with the input data at the end of the input data and stores it in the output file. The joined data has the joined input data followed by the transformed data and the output is a CSV file.
--
-- * 'dpInputFilter' - A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the input data to pass to the algorithm. Use the @InputFilter@ parameter to exclude fields, such as an ID column, from the input. If you want Amazon SageMaker to pass the entire input dataset to the algorithm, accept the default value @> @ . Examples: @"$"@ , @"$[1:]"@ , @"$.features"@
dataProcessing ::
  DataProcessing
dataProcessing =
  DataProcessing'
    { _dpOutputFilter = Nothing,
      _dpJoinSource = Nothing,
      _dpInputFilter = Nothing
    }

-- | A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the joined dataset to save in the output file for a batch transform job. If you want Amazon SageMaker to store the entire input dataset in the output file, leave the default value, @> @ . If you specify indexes that aren't within the dimension size of the joined dataset, you get an error. Examples: @"$"@ , @"$[0,5:]"@ , @"$['id','SageMakerOutput']"@
dpOutputFilter :: Lens' DataProcessing (Maybe Text)
dpOutputFilter = lens _dpOutputFilter (\s a -> s {_dpOutputFilter = a})

-- | Specifies the source of the data to join with the transformed data. The valid values are @None@ and @Input@ . The default value is @None@ , which specifies not to join the input with the transformed data. If you want the batch transform job to join the original input data with the transformed data, set @JoinSource@ to @Input@ .  For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker adds the transformed data to the input JSON object in an attribute called @SageMakerOutput@ . The joined result for JSON must be a key-value pair object. If the input is not a key-value pair object, Amazon SageMaker creates a new JSON file. In the new JSON file, and the input data is stored under the @SageMakerInput@ key and the results are stored in @SageMakerOutput@ . For CSV files, Amazon SageMaker combines the transformed data with the input data at the end of the input data and stores it in the output file. The joined data has the joined input data followed by the transformed data and the output is a CSV file.
dpJoinSource :: Lens' DataProcessing (Maybe JoinSource)
dpJoinSource = lens _dpJoinSource (\s a -> s {_dpJoinSource = a})

-- | A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the input data to pass to the algorithm. Use the @InputFilter@ parameter to exclude fields, such as an ID column, from the input. If you want Amazon SageMaker to pass the entire input dataset to the algorithm, accept the default value @> @ . Examples: @"$"@ , @"$[1:]"@ , @"$.features"@
dpInputFilter :: Lens' DataProcessing (Maybe Text)
dpInputFilter = lens _dpInputFilter (\s a -> s {_dpInputFilter = a})

instance FromJSON DataProcessing where
  parseJSON =
    withObject
      "DataProcessing"
      ( \x ->
          DataProcessing'
            <$> (x .:? "OutputFilter")
            <*> (x .:? "JoinSource")
            <*> (x .:? "InputFilter")
      )

instance Hashable DataProcessing

instance NFData DataProcessing

instance ToJSON DataProcessing where
  toJSON DataProcessing' {..} =
    object
      ( catMaybes
          [ ("OutputFilter" .=) <$> _dpOutputFilter,
            ("JoinSource" .=) <$> _dpJoinSource,
            ("InputFilter" .=) <$> _dpInputFilter
          ]
      )
