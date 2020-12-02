{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties where

import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode
import Network.AWS.Comprehend.Types.PiiOutputDataConfig
import Network.AWS.Comprehend.Types.RedactionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a PII entities detection job.
--
--
--
-- /See:/ 'piiEntitiesDetectionJobProperties' smart constructor.
data PiiEntitiesDetectionJobProperties = PiiEntitiesDetectionJobProperties'
  { _pedjpLanguageCode ::
      !(Maybe LanguageCode),
    _pedjpJobId ::
      !(Maybe Text),
    _pedjpJobName ::
      !(Maybe Text),
    _pedjpMode ::
      !( Maybe
           PiiEntitiesDetectionMode
       ),
    _pedjpInputDataConfig ::
      !( Maybe
           InputDataConfig
       ),
    _pedjpRedactionConfig ::
      !( Maybe
           RedactionConfig
       ),
    _pedjpEndTime ::
      !(Maybe POSIX),
    _pedjpOutputDataConfig ::
      !( Maybe
           PiiOutputDataConfig
       ),
    _pedjpDataAccessRoleARN ::
      !(Maybe Text),
    _pedjpJobStatus ::
      !(Maybe JobStatus),
    _pedjpMessage ::
      !(Maybe Text),
    _pedjpSubmitTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PiiEntitiesDetectionJobProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pedjpLanguageCode' - The language code of the input documents
--
-- * 'pedjpJobId' - The identifier assigned to the PII entities detection job.
--
-- * 'pedjpJobName' - The name that you assigned the PII entities detection job.
--
-- * 'pedjpMode' - Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- * 'pedjpInputDataConfig' - The input properties for a PII entities detection job.
--
-- * 'pedjpRedactionConfig' - Provides configuration parameters for PII entity redaction. This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- * 'pedjpEndTime' - The time that the PII entities detection job completed.
--
-- * 'pedjpOutputDataConfig' - The output data configuration that you supplied when you created the PII entities detection job.
--
-- * 'pedjpDataAccessRoleARN' - The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
--
-- * 'pedjpJobStatus' - The current status of the PII entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
--
-- * 'pedjpMessage' - A description of the status of a job.
--
-- * 'pedjpSubmitTime' - The time that the PII entities detection job was submitted for processing.
piiEntitiesDetectionJobProperties ::
  PiiEntitiesDetectionJobProperties
piiEntitiesDetectionJobProperties =
  PiiEntitiesDetectionJobProperties'
    { _pedjpLanguageCode = Nothing,
      _pedjpJobId = Nothing,
      _pedjpJobName = Nothing,
      _pedjpMode = Nothing,
      _pedjpInputDataConfig = Nothing,
      _pedjpRedactionConfig = Nothing,
      _pedjpEndTime = Nothing,
      _pedjpOutputDataConfig = Nothing,
      _pedjpDataAccessRoleARN = Nothing,
      _pedjpJobStatus = Nothing,
      _pedjpMessage = Nothing,
      _pedjpSubmitTime = Nothing
    }

-- | The language code of the input documents
pedjpLanguageCode :: Lens' PiiEntitiesDetectionJobProperties (Maybe LanguageCode)
pedjpLanguageCode = lens _pedjpLanguageCode (\s a -> s {_pedjpLanguageCode = a})

-- | The identifier assigned to the PII entities detection job.
pedjpJobId :: Lens' PiiEntitiesDetectionJobProperties (Maybe Text)
pedjpJobId = lens _pedjpJobId (\s a -> s {_pedjpJobId = a})

-- | The name that you assigned the PII entities detection job.
pedjpJobName :: Lens' PiiEntitiesDetectionJobProperties (Maybe Text)
pedjpJobName = lens _pedjpJobName (\s a -> s {_pedjpJobName = a})

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
pedjpMode :: Lens' PiiEntitiesDetectionJobProperties (Maybe PiiEntitiesDetectionMode)
pedjpMode = lens _pedjpMode (\s a -> s {_pedjpMode = a})

-- | The input properties for a PII entities detection job.
pedjpInputDataConfig :: Lens' PiiEntitiesDetectionJobProperties (Maybe InputDataConfig)
pedjpInputDataConfig = lens _pedjpInputDataConfig (\s a -> s {_pedjpInputDataConfig = a})

-- | Provides configuration parameters for PII entity redaction. This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
pedjpRedactionConfig :: Lens' PiiEntitiesDetectionJobProperties (Maybe RedactionConfig)
pedjpRedactionConfig = lens _pedjpRedactionConfig (\s a -> s {_pedjpRedactionConfig = a})

-- | The time that the PII entities detection job completed.
pedjpEndTime :: Lens' PiiEntitiesDetectionJobProperties (Maybe UTCTime)
pedjpEndTime = lens _pedjpEndTime (\s a -> s {_pedjpEndTime = a}) . mapping _Time

-- | The output data configuration that you supplied when you created the PII entities detection job.
pedjpOutputDataConfig :: Lens' PiiEntitiesDetectionJobProperties (Maybe PiiOutputDataConfig)
pedjpOutputDataConfig = lens _pedjpOutputDataConfig (\s a -> s {_pedjpOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend read access to your input data.
pedjpDataAccessRoleARN :: Lens' PiiEntitiesDetectionJobProperties (Maybe Text)
pedjpDataAccessRoleARN = lens _pedjpDataAccessRoleARN (\s a -> s {_pedjpDataAccessRoleARN = a})

-- | The current status of the PII entities detection job. If the status is @FAILED@ , the @Message@ field shows the reason for the failure.
pedjpJobStatus :: Lens' PiiEntitiesDetectionJobProperties (Maybe JobStatus)
pedjpJobStatus = lens _pedjpJobStatus (\s a -> s {_pedjpJobStatus = a})

-- | A description of the status of a job.
pedjpMessage :: Lens' PiiEntitiesDetectionJobProperties (Maybe Text)
pedjpMessage = lens _pedjpMessage (\s a -> s {_pedjpMessage = a})

-- | The time that the PII entities detection job was submitted for processing.
pedjpSubmitTime :: Lens' PiiEntitiesDetectionJobProperties (Maybe UTCTime)
pedjpSubmitTime = lens _pedjpSubmitTime (\s a -> s {_pedjpSubmitTime = a}) . mapping _Time

instance FromJSON PiiEntitiesDetectionJobProperties where
  parseJSON =
    withObject
      "PiiEntitiesDetectionJobProperties"
      ( \x ->
          PiiEntitiesDetectionJobProperties'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "JobId")
            <*> (x .:? "JobName")
            <*> (x .:? "Mode")
            <*> (x .:? "InputDataConfig")
            <*> (x .:? "RedactionConfig")
            <*> (x .:? "EndTime")
            <*> (x .:? "OutputDataConfig")
            <*> (x .:? "DataAccessRoleArn")
            <*> (x .:? "JobStatus")
            <*> (x .:? "Message")
            <*> (x .:? "SubmitTime")
      )

instance Hashable PiiEntitiesDetectionJobProperties

instance NFData PiiEntitiesDetectionJobProperties
