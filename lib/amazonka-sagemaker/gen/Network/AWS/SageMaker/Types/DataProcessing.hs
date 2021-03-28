{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DataProcessing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.DataProcessing
  ( DataProcessing (..)
  -- * Smart constructor
  , mkDataProcessing
  -- * Lenses
  , dpInputFilter
  , dpJoinSource
  , dpOutputFilter
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.JoinSource as Types
import qualified Network.AWS.SageMaker.Types.JsonPath as Types

-- | The data structure used to specify the data to be used for inference in a batch transform job and to associate the data that is relevant to the prediction results in the output. The input filter provided allows you to exclude input data that is not needed for inference in a batch transform job. The output filter provided allows you to include input data relevant to interpreting the predictions in the output from the job. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html Associate Prediction Results with their Corresponding Input Records> .
--
-- /See:/ 'mkDataProcessing' smart constructor.
data DataProcessing = DataProcessing'
  { inputFilter :: Core.Maybe Types.JsonPath
    -- ^ A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the input data to pass to the algorithm. Use the @InputFilter@ parameter to exclude fields, such as an ID column, from the input. If you want Amazon SageMaker to pass the entire input dataset to the algorithm, accept the default value @> @ .
--
-- Examples: @"$"@ , @"$[1:]"@ , @"$.features"@ 
  , joinSource :: Core.Maybe Types.JoinSource
    -- ^ Specifies the source of the data to join with the transformed data. The valid values are @None@ and @Input@ . The default value is @None@ , which specifies not to join the input with the transformed data. If you want the batch transform job to join the original input data with the transformed data, set @JoinSource@ to @Input@ . 
--
-- For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker adds the transformed data to the input JSON object in an attribute called @SageMakerOutput@ . The joined result for JSON must be a key-value pair object. If the input is not a key-value pair object, Amazon SageMaker creates a new JSON file. In the new JSON file, and the input data is stored under the @SageMakerInput@ key and the results are stored in @SageMakerOutput@ .
-- For CSV files, Amazon SageMaker combines the transformed data with the input data at the end of the input data and stores it in the output file. The joined data has the joined input data followed by the transformed data and the output is a CSV file. 
  , outputFilter :: Core.Maybe Types.JsonPath
    -- ^ A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the joined dataset to save in the output file for a batch transform job. If you want Amazon SageMaker to store the entire input dataset in the output file, leave the default value, @> @ . If you specify indexes that aren't within the dimension size of the joined dataset, you get an error.
--
-- Examples: @"$"@ , @"$[0,5:]"@ , @"$['id','SageMakerOutput']"@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataProcessing' value with any optional fields omitted.
mkDataProcessing
    :: DataProcessing
mkDataProcessing
  = DataProcessing'{inputFilter = Core.Nothing,
                    joinSource = Core.Nothing, outputFilter = Core.Nothing}

-- | A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the input data to pass to the algorithm. Use the @InputFilter@ parameter to exclude fields, such as an ID column, from the input. If you want Amazon SageMaker to pass the entire input dataset to the algorithm, accept the default value @> @ .
--
-- Examples: @"$"@ , @"$[1:]"@ , @"$.features"@ 
--
-- /Note:/ Consider using 'inputFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpInputFilter :: Lens.Lens' DataProcessing (Core.Maybe Types.JsonPath)
dpInputFilter = Lens.field @"inputFilter"
{-# INLINEABLE dpInputFilter #-}
{-# DEPRECATED inputFilter "Use generic-lens or generic-optics with 'inputFilter' instead"  #-}

-- | Specifies the source of the data to join with the transformed data. The valid values are @None@ and @Input@ . The default value is @None@ , which specifies not to join the input with the transformed data. If you want the batch transform job to join the original input data with the transformed data, set @JoinSource@ to @Input@ . 
--
-- For JSON or JSONLines objects, such as a JSON array, Amazon SageMaker adds the transformed data to the input JSON object in an attribute called @SageMakerOutput@ . The joined result for JSON must be a key-value pair object. If the input is not a key-value pair object, Amazon SageMaker creates a new JSON file. In the new JSON file, and the input data is stored under the @SageMakerInput@ key and the results are stored in @SageMakerOutput@ .
-- For CSV files, Amazon SageMaker combines the transformed data with the input data at the end of the input data and stores it in the output file. The joined data has the joined input data followed by the transformed data and the output is a CSV file. 
--
-- /Note:/ Consider using 'joinSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpJoinSource :: Lens.Lens' DataProcessing (Core.Maybe Types.JoinSource)
dpJoinSource = Lens.field @"joinSource"
{-# INLINEABLE dpJoinSource #-}
{-# DEPRECATED joinSource "Use generic-lens or generic-optics with 'joinSource' instead"  #-}

-- | A <https://docs.aws.amazon.com/sagemaker/latest/dg/batch-transform-data-processing.html#data-processing-operators JSONPath> expression used to select a portion of the joined dataset to save in the output file for a batch transform job. If you want Amazon SageMaker to store the entire input dataset in the output file, leave the default value, @> @ . If you specify indexes that aren't within the dimension size of the joined dataset, you get an error.
--
-- Examples: @"$"@ , @"$[0,5:]"@ , @"$['id','SageMakerOutput']"@ 
--
-- /Note:/ Consider using 'outputFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpOutputFilter :: Lens.Lens' DataProcessing (Core.Maybe Types.JsonPath)
dpOutputFilter = Lens.field @"outputFilter"
{-# INLINEABLE dpOutputFilter #-}
{-# DEPRECATED outputFilter "Use generic-lens or generic-optics with 'outputFilter' instead"  #-}

instance Core.FromJSON DataProcessing where
        toJSON DataProcessing{..}
          = Core.object
              (Core.catMaybes
                 [("InputFilter" Core..=) Core.<$> inputFilter,
                  ("JoinSource" Core..=) Core.<$> joinSource,
                  ("OutputFilter" Core..=) Core.<$> outputFilter])

instance Core.FromJSON DataProcessing where
        parseJSON
          = Core.withObject "DataProcessing" Core.$
              \ x ->
                DataProcessing' Core.<$>
                  (x Core..:? "InputFilter") Core.<*> x Core..:? "JoinSource"
                    Core.<*> x Core..:? "OutputFilter"
