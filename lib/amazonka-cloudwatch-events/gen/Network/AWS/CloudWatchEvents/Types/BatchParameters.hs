{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchParameters where

import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The custom parameters to be used when the target is an AWS Batch job.
--
--
--
-- /See:/ 'batchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { _bpRetryStrategy ::
      !(Maybe BatchRetryStrategy),
    _bpArrayProperties :: !(Maybe BatchArrayProperties),
    _bpJobDefinition :: !Text,
    _bpJobName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpRetryStrategy' - The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- * 'bpArrayProperties' - The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- * 'bpJobDefinition' - The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
--
-- * 'bpJobName' - The name to use for this execution of the job, if the target is an AWS Batch job.
batchParameters ::
  -- | 'bpJobDefinition'
  Text ->
  -- | 'bpJobName'
  Text ->
  BatchParameters
batchParameters pJobDefinition_ pJobName_ =
  BatchParameters'
    { _bpRetryStrategy = Nothing,
      _bpArrayProperties = Nothing,
      _bpJobDefinition = pJobDefinition_,
      _bpJobName = pJobName_
    }

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
bpRetryStrategy :: Lens' BatchParameters (Maybe BatchRetryStrategy)
bpRetryStrategy = lens _bpRetryStrategy (\s a -> s {_bpRetryStrategy = a})

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
bpArrayProperties :: Lens' BatchParameters (Maybe BatchArrayProperties)
bpArrayProperties = lens _bpArrayProperties (\s a -> s {_bpArrayProperties = a})

-- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
bpJobDefinition :: Lens' BatchParameters Text
bpJobDefinition = lens _bpJobDefinition (\s a -> s {_bpJobDefinition = a})

-- | The name to use for this execution of the job, if the target is an AWS Batch job.
bpJobName :: Lens' BatchParameters Text
bpJobName = lens _bpJobName (\s a -> s {_bpJobName = a})

instance FromJSON BatchParameters where
  parseJSON =
    withObject
      "BatchParameters"
      ( \x ->
          BatchParameters'
            <$> (x .:? "RetryStrategy")
            <*> (x .:? "ArrayProperties")
            <*> (x .: "JobDefinition")
            <*> (x .: "JobName")
      )

instance Hashable BatchParameters

instance NFData BatchParameters

instance ToJSON BatchParameters where
  toJSON BatchParameters' {..} =
    object
      ( catMaybes
          [ ("RetryStrategy" .=) <$> _bpRetryStrategy,
            ("ArrayProperties" .=) <$> _bpArrayProperties,
            Just ("JobDefinition" .= _bpJobDefinition),
            Just ("JobName" .= _bpJobName)
          ]
      )
