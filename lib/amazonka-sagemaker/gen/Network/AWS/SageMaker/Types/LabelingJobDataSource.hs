{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
import Network.AWS.SageMaker.Types.LabelingJobSNSDataSource

-- | Provides information about the location of input data.
--
--
-- You must specify at least one of the following: @S3DataSource@ or @SnsDataSource@ .
--
-- Use @SnsDataSource@ to specify an SNS input topic for a streaming labeling job. If you do not specify and SNS input topic ARN, Ground Truth will create a one-time labeling job.
--
-- Use @S3DataSource@ to specify an input manifest file for both streaming and one-time labeling jobs. Adding an @S3DataSource@ is optional if you use @SnsDataSource@ to create a streaming labeling job.
--
--
-- /See:/ 'labelingJobDataSource' smart constructor.
data LabelingJobDataSource = LabelingJobDataSource'
  { _ljdsS3DataSource ::
      !(Maybe LabelingJobS3DataSource),
    _ljdsSNSDataSource ::
      !(Maybe LabelingJobSNSDataSource)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljdsS3DataSource' - The Amazon S3 location of the input data objects.
--
-- * 'ljdsSNSDataSource' - An Amazon SNS data source used for streaming labeling jobs.
labelingJobDataSource ::
  LabelingJobDataSource
labelingJobDataSource =
  LabelingJobDataSource'
    { _ljdsS3DataSource = Nothing,
      _ljdsSNSDataSource = Nothing
    }

-- | The Amazon S3 location of the input data objects.
ljdsS3DataSource :: Lens' LabelingJobDataSource (Maybe LabelingJobS3DataSource)
ljdsS3DataSource = lens _ljdsS3DataSource (\s a -> s {_ljdsS3DataSource = a})

-- | An Amazon SNS data source used for streaming labeling jobs.
ljdsSNSDataSource :: Lens' LabelingJobDataSource (Maybe LabelingJobSNSDataSource)
ljdsSNSDataSource = lens _ljdsSNSDataSource (\s a -> s {_ljdsSNSDataSource = a})

instance FromJSON LabelingJobDataSource where
  parseJSON =
    withObject
      "LabelingJobDataSource"
      ( \x ->
          LabelingJobDataSource'
            <$> (x .:? "S3DataSource") <*> (x .:? "SnsDataSource")
      )

instance Hashable LabelingJobDataSource

instance NFData LabelingJobDataSource

instance ToJSON LabelingJobDataSource where
  toJSON LabelingJobDataSource' {..} =
    object
      ( catMaybes
          [ ("S3DataSource" .=) <$> _ljdsS3DataSource,
            ("SnsDataSource" .=) <$> _ljdsSNSDataSource
          ]
      )
