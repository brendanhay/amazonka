{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the location of the output produced by the labeling job.
--
--
--
-- /See:/ 'labelingJobOutput' smart constructor.
data LabelingJobOutput = LabelingJobOutput'
  { _ljoFinalActiveLearningModelARN ::
      !(Maybe Text),
    _ljoOutputDatasetS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljoFinalActiveLearningModelARN' - The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
--
-- * 'ljoOutputDatasetS3URI' - The Amazon S3 bucket location of the manifest file for labeled data.
labelingJobOutput ::
  -- | 'ljoOutputDatasetS3URI'
  Text ->
  LabelingJobOutput
labelingJobOutput pOutputDatasetS3URI_ =
  LabelingJobOutput'
    { _ljoFinalActiveLearningModelARN = Nothing,
      _ljoOutputDatasetS3URI = pOutputDatasetS3URI_
    }

-- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
ljoFinalActiveLearningModelARN :: Lens' LabelingJobOutput (Maybe Text)
ljoFinalActiveLearningModelARN = lens _ljoFinalActiveLearningModelARN (\s a -> s {_ljoFinalActiveLearningModelARN = a})

-- | The Amazon S3 bucket location of the manifest file for labeled data.
ljoOutputDatasetS3URI :: Lens' LabelingJobOutput Text
ljoOutputDatasetS3URI = lens _ljoOutputDatasetS3URI (\s a -> s {_ljoOutputDatasetS3URI = a})

instance FromJSON LabelingJobOutput where
  parseJSON =
    withObject
      "LabelingJobOutput"
      ( \x ->
          LabelingJobOutput'
            <$> (x .:? "FinalActiveLearningModelArn")
            <*> (x .: "OutputDatasetS3Uri")
      )

instance Hashable LabelingJobOutput

instance NFData LabelingJobOutput
