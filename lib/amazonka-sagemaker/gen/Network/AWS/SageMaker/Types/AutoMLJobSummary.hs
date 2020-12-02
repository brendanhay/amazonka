{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
import Network.AWS.SageMaker.Types.AutoMLJobStatus

-- | Provides a summary about a job.
--
--
--
-- /See:/ 'autoMLJobSummary' smart constructor.
data AutoMLJobSummary = AutoMLJobSummary'
  { _amljsFailureReason ::
      !(Maybe Text),
    _amljsEndTime :: !(Maybe POSIX),
    _amljsAutoMLJobName :: !Text,
    _amljsAutoMLJobARN :: !Text,
    _amljsAutoMLJobStatus :: !AutoMLJobStatus,
    _amljsAutoMLJobSecondaryStatus ::
      !AutoMLJobSecondaryStatus,
    _amljsCreationTime :: !POSIX,
    _amljsLastModifiedTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amljsFailureReason' - The failure reason of a job.
--
-- * 'amljsEndTime' - The end time of an AutoML job.
--
-- * 'amljsAutoMLJobName' - The name of the object you are requesting.
--
-- * 'amljsAutoMLJobARN' - The ARN of the job.
--
-- * 'amljsAutoMLJobStatus' - The job's status.
--
-- * 'amljsAutoMLJobSecondaryStatus' - The job's secondary status.
--
-- * 'amljsCreationTime' - When the job was created.
--
-- * 'amljsLastModifiedTime' - When the job was last modified.
autoMLJobSummary ::
  -- | 'amljsAutoMLJobName'
  Text ->
  -- | 'amljsAutoMLJobARN'
  Text ->
  -- | 'amljsAutoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'amljsAutoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  -- | 'amljsCreationTime'
  UTCTime ->
  -- | 'amljsLastModifiedTime'
  UTCTime ->
  AutoMLJobSummary
autoMLJobSummary
  pAutoMLJobName_
  pAutoMLJobARN_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLJobSummary'
      { _amljsFailureReason = Nothing,
        _amljsEndTime = Nothing,
        _amljsAutoMLJobName = pAutoMLJobName_,
        _amljsAutoMLJobARN = pAutoMLJobARN_,
        _amljsAutoMLJobStatus = pAutoMLJobStatus_,
        _amljsAutoMLJobSecondaryStatus = pAutoMLJobSecondaryStatus_,
        _amljsCreationTime = _Time # pCreationTime_,
        _amljsLastModifiedTime = _Time # pLastModifiedTime_
      }

-- | The failure reason of a job.
amljsFailureReason :: Lens' AutoMLJobSummary (Maybe Text)
amljsFailureReason = lens _amljsFailureReason (\s a -> s {_amljsFailureReason = a})

-- | The end time of an AutoML job.
amljsEndTime :: Lens' AutoMLJobSummary (Maybe UTCTime)
amljsEndTime = lens _amljsEndTime (\s a -> s {_amljsEndTime = a}) . mapping _Time

-- | The name of the object you are requesting.
amljsAutoMLJobName :: Lens' AutoMLJobSummary Text
amljsAutoMLJobName = lens _amljsAutoMLJobName (\s a -> s {_amljsAutoMLJobName = a})

-- | The ARN of the job.
amljsAutoMLJobARN :: Lens' AutoMLJobSummary Text
amljsAutoMLJobARN = lens _amljsAutoMLJobARN (\s a -> s {_amljsAutoMLJobARN = a})

-- | The job's status.
amljsAutoMLJobStatus :: Lens' AutoMLJobSummary AutoMLJobStatus
amljsAutoMLJobStatus = lens _amljsAutoMLJobStatus (\s a -> s {_amljsAutoMLJobStatus = a})

-- | The job's secondary status.
amljsAutoMLJobSecondaryStatus :: Lens' AutoMLJobSummary AutoMLJobSecondaryStatus
amljsAutoMLJobSecondaryStatus = lens _amljsAutoMLJobSecondaryStatus (\s a -> s {_amljsAutoMLJobSecondaryStatus = a})

-- | When the job was created.
amljsCreationTime :: Lens' AutoMLJobSummary UTCTime
amljsCreationTime = lens _amljsCreationTime (\s a -> s {_amljsCreationTime = a}) . _Time

-- | When the job was last modified.
amljsLastModifiedTime :: Lens' AutoMLJobSummary UTCTime
amljsLastModifiedTime = lens _amljsLastModifiedTime (\s a -> s {_amljsLastModifiedTime = a}) . _Time

instance FromJSON AutoMLJobSummary where
  parseJSON =
    withObject
      "AutoMLJobSummary"
      ( \x ->
          AutoMLJobSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "EndTime")
            <*> (x .: "AutoMLJobName")
            <*> (x .: "AutoMLJobArn")
            <*> (x .: "AutoMLJobStatus")
            <*> (x .: "AutoMLJobSecondaryStatus")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
      )

instance Hashable AutoMLJobSummary

instance NFData AutoMLJobSummary
