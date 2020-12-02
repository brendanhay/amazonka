{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSNSDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSNSDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An Amazon SNS data source used for streaming labeling jobs.
--
--
--
-- /See:/ 'labelingJobSNSDataSource' smart constructor.
newtype LabelingJobSNSDataSource = LabelingJobSNSDataSource'
  { _ljsdsSNSTopicARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobSNSDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsdsSNSTopicARN' - The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job. If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
labelingJobSNSDataSource ::
  -- | 'ljsdsSNSTopicARN'
  Text ->
  LabelingJobSNSDataSource
labelingJobSNSDataSource pSNSTopicARN_ =
  LabelingJobSNSDataSource' {_ljsdsSNSTopicARN = pSNSTopicARN_}

-- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job. If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
ljsdsSNSTopicARN :: Lens' LabelingJobSNSDataSource Text
ljsdsSNSTopicARN = lens _ljsdsSNSTopicARN (\s a -> s {_ljsdsSNSTopicARN = a})

instance FromJSON LabelingJobSNSDataSource where
  parseJSON =
    withObject
      "LabelingJobSNSDataSource"
      (\x -> LabelingJobSNSDataSource' <$> (x .: "SnsTopicArn"))

instance Hashable LabelingJobSNSDataSource

instance NFData LabelingJobSNSDataSource

instance ToJSON LabelingJobSNSDataSource where
  toJSON LabelingJobSNSDataSource' {..} =
    object (catMaybes [Just ("SnsTopicArn" .= _ljsdsSNSTopicARN)])
