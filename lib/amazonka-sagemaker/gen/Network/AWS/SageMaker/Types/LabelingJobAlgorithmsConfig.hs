{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig

-- | Provides configuration information for auto-labeling of your data objects. A @LabelingJobAlgorithmsConfig@ object must be supplied in order to use auto-labeling.
--
--
--
-- /See:/ 'labelingJobAlgorithmsConfig' smart constructor.
data LabelingJobAlgorithmsConfig = LabelingJobAlgorithmsConfig'
  { _ljacLabelingJobResourceConfig ::
      !(Maybe LabelingJobResourceConfig),
    _ljacInitialActiveLearningModelARN ::
      !(Maybe Text),
    _ljacLabelingJobAlgorithmSpecificationARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobAlgorithmsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljacLabelingJobResourceConfig' - Provides configuration information for a labeling job.
--
-- * 'ljacInitialActiveLearningModelARN' - At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
--
-- * 'ljacLabelingJobAlgorithmSpecificationARN' - Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:     * /Image classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@      * /Text classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@      * /Object detection/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@      * /Semantic Segmentation/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/semantic-segmentation@
labelingJobAlgorithmsConfig ::
  -- | 'ljacLabelingJobAlgorithmSpecificationARN'
  Text ->
  LabelingJobAlgorithmsConfig
labelingJobAlgorithmsConfig pLabelingJobAlgorithmSpecificationARN_ =
  LabelingJobAlgorithmsConfig'
    { _ljacLabelingJobResourceConfig =
        Nothing,
      _ljacInitialActiveLearningModelARN = Nothing,
      _ljacLabelingJobAlgorithmSpecificationARN =
        pLabelingJobAlgorithmSpecificationARN_
    }

-- | Provides configuration information for a labeling job.
ljacLabelingJobResourceConfig :: Lens' LabelingJobAlgorithmsConfig (Maybe LabelingJobResourceConfig)
ljacLabelingJobResourceConfig = lens _ljacLabelingJobResourceConfig (\s a -> s {_ljacLabelingJobResourceConfig = a})

-- | At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
ljacInitialActiveLearningModelARN :: Lens' LabelingJobAlgorithmsConfig (Maybe Text)
ljacInitialActiveLearningModelARN = lens _ljacInitialActiveLearningModelARN (\s a -> s {_ljacInitialActiveLearningModelARN = a})

-- | Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:     * /Image classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@      * /Text classification/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@      * /Object detection/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@      * /Semantic Segmentation/  @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/semantic-segmentation@
ljacLabelingJobAlgorithmSpecificationARN :: Lens' LabelingJobAlgorithmsConfig Text
ljacLabelingJobAlgorithmSpecificationARN = lens _ljacLabelingJobAlgorithmSpecificationARN (\s a -> s {_ljacLabelingJobAlgorithmSpecificationARN = a})

instance FromJSON LabelingJobAlgorithmsConfig where
  parseJSON =
    withObject
      "LabelingJobAlgorithmsConfig"
      ( \x ->
          LabelingJobAlgorithmsConfig'
            <$> (x .:? "LabelingJobResourceConfig")
            <*> (x .:? "InitialActiveLearningModelArn")
            <*> (x .: "LabelingJobAlgorithmSpecificationArn")
      )

instance Hashable LabelingJobAlgorithmsConfig

instance NFData LabelingJobAlgorithmsConfig

instance ToJSON LabelingJobAlgorithmsConfig where
  toJSON LabelingJobAlgorithmsConfig' {..} =
    object
      ( catMaybes
          [ ("LabelingJobResourceConfig" .=)
              <$> _ljacLabelingJobResourceConfig,
            ("InitialActiveLearningModelArn" .=)
              <$> _ljacInitialActiveLearningModelARN,
            Just
              ( "LabelingJobAlgorithmSpecificationArn"
                  .= _ljacLabelingJobAlgorithmSpecificationARN
              )
          ]
      )
