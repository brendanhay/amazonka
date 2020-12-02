{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Provides summary information about a training job.
--
--
--
-- /See:/ 'trainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { _tjsjTrainingEndTime ::
      !(Maybe POSIX),
    _tjsjLastModifiedTime :: !(Maybe POSIX),
    _tjsjTrainingJobName :: !Text,
    _tjsjTrainingJobARN :: !Text,
    _tjsjCreationTime :: !POSIX,
    _tjsjTrainingJobStatus :: !TrainingJobStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrainingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsjTrainingEndTime' - A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- * 'tjsjLastModifiedTime' - Timestamp when the training job was last modified.
--
-- * 'tjsjTrainingJobName' - The name of the training job that you want a summary for.
--
-- * 'tjsjTrainingJobARN' - The Amazon Resource Name (ARN) of the training job.
--
-- * 'tjsjCreationTime' - A timestamp that shows when the training job was created.
--
-- * 'tjsjTrainingJobStatus' - The status of the training job.
trainingJobSummary ::
  -- | 'tjsjTrainingJobName'
  Text ->
  -- | 'tjsjTrainingJobARN'
  Text ->
  -- | 'tjsjCreationTime'
  UTCTime ->
  -- | 'tjsjTrainingJobStatus'
  TrainingJobStatus ->
  TrainingJobSummary
trainingJobSummary
  pTrainingJobName_
  pTrainingJobARN_
  pCreationTime_
  pTrainingJobStatus_ =
    TrainingJobSummary'
      { _tjsjTrainingEndTime = Nothing,
        _tjsjLastModifiedTime = Nothing,
        _tjsjTrainingJobName = pTrainingJobName_,
        _tjsjTrainingJobARN = pTrainingJobARN_,
        _tjsjCreationTime = _Time # pCreationTime_,
        _tjsjTrainingJobStatus = pTrainingJobStatus_
      }

-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
tjsjTrainingEndTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tjsjTrainingEndTime = lens _tjsjTrainingEndTime (\s a -> s {_tjsjTrainingEndTime = a}) . mapping _Time

-- | Timestamp when the training job was last modified.
tjsjLastModifiedTime :: Lens' TrainingJobSummary (Maybe UTCTime)
tjsjLastModifiedTime = lens _tjsjLastModifiedTime (\s a -> s {_tjsjLastModifiedTime = a}) . mapping _Time

-- | The name of the training job that you want a summary for.
tjsjTrainingJobName :: Lens' TrainingJobSummary Text
tjsjTrainingJobName = lens _tjsjTrainingJobName (\s a -> s {_tjsjTrainingJobName = a})

-- | The Amazon Resource Name (ARN) of the training job.
tjsjTrainingJobARN :: Lens' TrainingJobSummary Text
tjsjTrainingJobARN = lens _tjsjTrainingJobARN (\s a -> s {_tjsjTrainingJobARN = a})

-- | A timestamp that shows when the training job was created.
tjsjCreationTime :: Lens' TrainingJobSummary UTCTime
tjsjCreationTime = lens _tjsjCreationTime (\s a -> s {_tjsjCreationTime = a}) . _Time

-- | The status of the training job.
tjsjTrainingJobStatus :: Lens' TrainingJobSummary TrainingJobStatus
tjsjTrainingJobStatus = lens _tjsjTrainingJobStatus (\s a -> s {_tjsjTrainingJobStatus = a})

instance FromJSON TrainingJobSummary where
  parseJSON =
    withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            <$> (x .:? "TrainingEndTime")
            <*> (x .:? "LastModifiedTime")
            <*> (x .: "TrainingJobName")
            <*> (x .: "TrainingJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "TrainingJobStatus")
      )

instance Hashable TrainingJobSummary

instance NFData TrainingJobSummary
