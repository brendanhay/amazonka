{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingJobStatus

-- | Summary of information about a processing job.
--
--
--
-- /See:/ 'processingJobSummary' smart constructor.
data ProcessingJobSummary = ProcessingJobSummary'
  { _pjsFailureReason ::
      !(Maybe Text),
    _pjsLastModifiedTime :: !(Maybe POSIX),
    _pjsExitMessage :: !(Maybe Text),
    _pjsProcessingEndTime :: !(Maybe POSIX),
    _pjsProcessingJobName :: !Text,
    _pjsProcessingJobARN :: !Text,
    _pjsCreationTime :: !POSIX,
    _pjsProcessingJobStatus :: !ProcessingJobStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjsFailureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- * 'pjsLastModifiedTime' - A timestamp that indicates the last time the processing job was modified.
--
-- * 'pjsExitMessage' - An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- * 'pjsProcessingEndTime' - The time at which the processing job completed.
--
-- * 'pjsProcessingJobName' - The name of the processing job.
--
-- * 'pjsProcessingJobARN' - The Amazon Resource Name (ARN) of the processing job..
--
-- * 'pjsCreationTime' - The time at which the processing job was created.
--
-- * 'pjsProcessingJobStatus' - The status of the processing job.
processingJobSummary ::
  -- | 'pjsProcessingJobName'
  Text ->
  -- | 'pjsProcessingJobARN'
  Text ->
  -- | 'pjsCreationTime'
  UTCTime ->
  -- | 'pjsProcessingJobStatus'
  ProcessingJobStatus ->
  ProcessingJobSummary
processingJobSummary
  pProcessingJobName_
  pProcessingJobARN_
  pCreationTime_
  pProcessingJobStatus_ =
    ProcessingJobSummary'
      { _pjsFailureReason = Nothing,
        _pjsLastModifiedTime = Nothing,
        _pjsExitMessage = Nothing,
        _pjsProcessingEndTime = Nothing,
        _pjsProcessingJobName = pProcessingJobName_,
        _pjsProcessingJobARN = pProcessingJobARN_,
        _pjsCreationTime = _Time # pCreationTime_,
        _pjsProcessingJobStatus = pProcessingJobStatus_
      }

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
pjsFailureReason :: Lens' ProcessingJobSummary (Maybe Text)
pjsFailureReason = lens _pjsFailureReason (\s a -> s {_pjsFailureReason = a})

-- | A timestamp that indicates the last time the processing job was modified.
pjsLastModifiedTime :: Lens' ProcessingJobSummary (Maybe UTCTime)
pjsLastModifiedTime = lens _pjsLastModifiedTime (\s a -> s {_pjsLastModifiedTime = a}) . mapping _Time

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
pjsExitMessage :: Lens' ProcessingJobSummary (Maybe Text)
pjsExitMessage = lens _pjsExitMessage (\s a -> s {_pjsExitMessage = a})

-- | The time at which the processing job completed.
pjsProcessingEndTime :: Lens' ProcessingJobSummary (Maybe UTCTime)
pjsProcessingEndTime = lens _pjsProcessingEndTime (\s a -> s {_pjsProcessingEndTime = a}) . mapping _Time

-- | The name of the processing job.
pjsProcessingJobName :: Lens' ProcessingJobSummary Text
pjsProcessingJobName = lens _pjsProcessingJobName (\s a -> s {_pjsProcessingJobName = a})

-- | The Amazon Resource Name (ARN) of the processing job..
pjsProcessingJobARN :: Lens' ProcessingJobSummary Text
pjsProcessingJobARN = lens _pjsProcessingJobARN (\s a -> s {_pjsProcessingJobARN = a})

-- | The time at which the processing job was created.
pjsCreationTime :: Lens' ProcessingJobSummary UTCTime
pjsCreationTime = lens _pjsCreationTime (\s a -> s {_pjsCreationTime = a}) . _Time

-- | The status of the processing job.
pjsProcessingJobStatus :: Lens' ProcessingJobSummary ProcessingJobStatus
pjsProcessingJobStatus = lens _pjsProcessingJobStatus (\s a -> s {_pjsProcessingJobStatus = a})

instance FromJSON ProcessingJobSummary where
  parseJSON =
    withObject
      "ProcessingJobSummary"
      ( \x ->
          ProcessingJobSummary'
            <$> (x .:? "FailureReason")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "ExitMessage")
            <*> (x .:? "ProcessingEndTime")
            <*> (x .: "ProcessingJobName")
            <*> (x .: "ProcessingJobArn")
            <*> (x .: "CreationTime")
            <*> (x .: "ProcessingJobStatus")
      )

instance Hashable ProcessingJobSummary

instance NFData ProcessingJobSummary
