{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.AttemptContainerDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.AttemptContainerDetail where

import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the details of a container that is part of a job attempt.
--
--
--
-- /See:/ 'attemptContainerDetail' smart constructor.
data AttemptContainerDetail = AttemptContainerDetail'
  { _acdNetworkInterfaces ::
      !(Maybe [NetworkInterface]),
    _acdTaskARN :: !(Maybe Text),
    _acdContainerInstanceARN :: !(Maybe Text),
    _acdReason :: !(Maybe Text),
    _acdLogStreamName :: !(Maybe Text),
    _acdExitCode :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttemptContainerDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acdNetworkInterfaces' - The network interfaces associated with the job attempt.
--
-- * 'acdTaskARN' - The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
--
-- * 'acdContainerInstanceARN' - The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
--
-- * 'acdReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'acdLogStreamName' - The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
--
-- * 'acdExitCode' - The exit code for the job attempt. A non-zero exit code is considered a failure.
attemptContainerDetail ::
  AttemptContainerDetail
attemptContainerDetail =
  AttemptContainerDetail'
    { _acdNetworkInterfaces = Nothing,
      _acdTaskARN = Nothing,
      _acdContainerInstanceARN = Nothing,
      _acdReason = Nothing,
      _acdLogStreamName = Nothing,
      _acdExitCode = Nothing
    }

-- | The network interfaces associated with the job attempt.
acdNetworkInterfaces :: Lens' AttemptContainerDetail [NetworkInterface]
acdNetworkInterfaces = lens _acdNetworkInterfaces (\s a -> s {_acdNetworkInterfaces = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Amazon ECS task that is associated with the job attempt. Each container attempt receives a task ARN when they reach the @STARTING@ status.
acdTaskARN :: Lens' AttemptContainerDetail (Maybe Text)
acdTaskARN = lens _acdTaskARN (\s a -> s {_acdTaskARN = a})

-- | The Amazon Resource Name (ARN) of the Amazon ECS container instance that hosts the job attempt.
acdContainerInstanceARN :: Lens' AttemptContainerDetail (Maybe Text)
acdContainerInstanceARN = lens _acdContainerInstanceARN (\s a -> s {_acdContainerInstanceARN = a})

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
acdReason :: Lens' AttemptContainerDetail (Maybe Text)
acdReason = lens _acdReason (\s a -> s {_acdReason = a})

-- | The name of the CloudWatch Logs log stream associated with the container. The log group for AWS Batch jobs is @/aws/batch/job@ . Each container attempt receives a log stream name when they reach the @RUNNING@ status.
acdLogStreamName :: Lens' AttemptContainerDetail (Maybe Text)
acdLogStreamName = lens _acdLogStreamName (\s a -> s {_acdLogStreamName = a})

-- | The exit code for the job attempt. A non-zero exit code is considered a failure.
acdExitCode :: Lens' AttemptContainerDetail (Maybe Int)
acdExitCode = lens _acdExitCode (\s a -> s {_acdExitCode = a})

instance FromJSON AttemptContainerDetail where
  parseJSON =
    withObject
      "AttemptContainerDetail"
      ( \x ->
          AttemptContainerDetail'
            <$> (x .:? "networkInterfaces" .!= mempty)
            <*> (x .:? "taskArn")
            <*> (x .:? "containerInstanceArn")
            <*> (x .:? "reason")
            <*> (x .:? "logStreamName")
            <*> (x .:? "exitCode")
      )

instance Hashable AttemptContainerDetail

instance NFData AttemptContainerDetail
