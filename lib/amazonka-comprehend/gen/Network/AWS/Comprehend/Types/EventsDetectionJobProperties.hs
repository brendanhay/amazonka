{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EventsDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about an events detection job.
--
--
--
-- /See:/ 'eventsDetectionJobProperties' smart constructor.
data EventsDetectionJobProperties = EventsDetectionJobProperties'
  { _edjpLanguageCode ::
      !(Maybe LanguageCode),
    _edjpJobId :: !(Maybe Text),
    _edjpJobName :: !(Maybe Text),
    _edjpTargetEventTypes ::
      !(Maybe (List1 Text)),
    _edjpInputDataConfig ::
      !(Maybe InputDataConfig),
    _edjpEndTime :: !(Maybe POSIX),
    _edjpOutputDataConfig ::
      !(Maybe OutputDataConfig),
    _edjpDataAccessRoleARN ::
      !(Maybe Text),
    _edjpJobStatus ::
      !(Maybe JobStatus),
    _edjpMessage :: !(Maybe Text),
    _edjpSubmitTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventsDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edjpLanguageCode' - The language code of the input documents.
--
-- * 'edjpJobId' - The identifier assigned to the events detection job.
--
-- * 'edjpJobName' - The name you assigned the events detection job.
--
-- * 'edjpTargetEventTypes' - The types of events that are detected by the job.
--
-- * 'edjpInputDataConfig' - The input data configuration that you supplied when you created the events detection job.
--
-- * 'edjpEndTime' - The time that the events detection job completed.
--
-- * 'edjpOutputDataConfig' - The output data configuration that you supplied when you created the events detection job.
--
-- * 'edjpDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'edjpJobStatus' - The current status of the events detection job.
--
-- * 'edjpMessage' - A description of the status of the events detection job.
--
-- * 'edjpSubmitTime' - The time that the events detection job was submitted for processing.
eventsDetectionJobProperties ::
  EventsDetectionJobProperties
eventsDetectionJobProperties =
  EventsDetectionJobProperties'
    { _edjpLanguageCode = Nothing,
      _edjpJobId = Nothing,
      _edjpJobName = Nothing,
      _edjpTargetEventTypes = Nothing,
      _edjpInputDataConfig = Nothing,
      _edjpEndTime = Nothing,
      _edjpOutputDataConfig = Nothing,
      _edjpDataAccessRoleARN = Nothing,
      _edjpJobStatus = Nothing,
      _edjpMessage = Nothing,
      _edjpSubmitTime = Nothing
    }

-- | The language code of the input documents.
edjpLanguageCode :: Lens' EventsDetectionJobProperties (Maybe LanguageCode)
edjpLanguageCode = lens _edjpLanguageCode (\s a -> s {_edjpLanguageCode = a})

-- | The identifier assigned to the events detection job.
edjpJobId :: Lens' EventsDetectionJobProperties (Maybe Text)
edjpJobId = lens _edjpJobId (\s a -> s {_edjpJobId = a})

-- | The name you assigned the events detection job.
edjpJobName :: Lens' EventsDetectionJobProperties (Maybe Text)
edjpJobName = lens _edjpJobName (\s a -> s {_edjpJobName = a})

-- | The types of events that are detected by the job.
edjpTargetEventTypes :: Lens' EventsDetectionJobProperties (Maybe (NonEmpty Text))
edjpTargetEventTypes = lens _edjpTargetEventTypes (\s a -> s {_edjpTargetEventTypes = a}) . mapping _List1

-- | The input data configuration that you supplied when you created the events detection job.
edjpInputDataConfig :: Lens' EventsDetectionJobProperties (Maybe InputDataConfig)
edjpInputDataConfig = lens _edjpInputDataConfig (\s a -> s {_edjpInputDataConfig = a})

-- | The time that the events detection job completed.
edjpEndTime :: Lens' EventsDetectionJobProperties (Maybe UTCTime)
edjpEndTime = lens _edjpEndTime (\s a -> s {_edjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the events detection job.
edjpOutputDataConfig :: Lens' EventsDetectionJobProperties (Maybe OutputDataConfig)
edjpOutputDataConfig = lens _edjpOutputDataConfig (\s a -> s {_edjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identify and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
edjpDataAccessRoleARN :: Lens' EventsDetectionJobProperties (Maybe Text)
edjpDataAccessRoleARN = lens _edjpDataAccessRoleARN (\s a -> s {_edjpDataAccessRoleARN = a})

-- | The current status of the events detection job.
edjpJobStatus :: Lens' EventsDetectionJobProperties (Maybe JobStatus)
edjpJobStatus = lens _edjpJobStatus (\s a -> s {_edjpJobStatus = a})

-- | A description of the status of the events detection job.
edjpMessage :: Lens' EventsDetectionJobProperties (Maybe Text)
edjpMessage = lens _edjpMessage (\s a -> s {_edjpMessage = a})

-- | The time that the events detection job was submitted for processing.
edjpSubmitTime :: Lens' EventsDetectionJobProperties (Maybe UTCTime)
edjpSubmitTime = lens _edjpSubmitTime (\s a -> s {_edjpSubmitTime = a}) . mapping _Time

instance FromJSON EventsDetectionJobProperties where
  parseJSON =
    withObject
      "EventsDetectionJobProperties"
      ( \x ->
          EventsDetectionJobProperties'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "JobId")
            <*> (x .:? "JobName")
            <*> (x .:? "TargetEventTypes")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "EndTime")
            <*> (x .:? "OutputDataConfig")
            <*> (x .:? "DataAccessRoleArn")
            <*> (x .:? "JobStatus")
            <*> (x .:? "Message")
            <*> (x .:? "SubmitTime")
      )

instance Hashable EventsDetectionJobProperties

instance NFData EventsDetectionJobProperties
