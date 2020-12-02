{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous PII entity detection job for a collection of documents.
module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
  ( -- * Creating a Request
    startPiiEntitiesDetectionJob,
    StartPiiEntitiesDetectionJob,

    -- * Request Lenses
    spedjJobName,
    spedjRedactionConfig,
    spedjClientRequestToken,
    spedjInputDataConfig,
    spedjOutputDataConfig,
    spedjMode,
    spedjDataAccessRoleARN,
    spedjLanguageCode,

    -- * Destructuring the Response
    startPiiEntitiesDetectionJobResponse,
    StartPiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    spedjrsJobId,
    spedjrsJobStatus,
    spedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { _spedjJobName ::
      !(Maybe Text),
    _spedjRedactionConfig ::
      !(Maybe RedactionConfig),
    _spedjClientRequestToken ::
      !(Maybe Text),
    _spedjInputDataConfig ::
      !InputDataConfig,
    _spedjOutputDataConfig ::
      !OutputDataConfig,
    _spedjMode ::
      !PiiEntitiesDetectionMode,
    _spedjDataAccessRoleARN :: !Text,
    _spedjLanguageCode ::
      !LanguageCode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartPiiEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spedjJobName' - The identifier of the job.
--
-- * 'spedjRedactionConfig' - Provides configuration parameters for PII entity redaction. This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- * 'spedjClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'spedjInputDataConfig' - The input properties for a PII entities detection job.
--
-- * 'spedjOutputDataConfig' - Provides conﬁguration parameters for the output of PII entity detection jobs.
--
-- * 'spedjMode' - Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- * 'spedjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'spedjLanguageCode' - The language of the input documents.
startPiiEntitiesDetectionJob ::
  -- | 'spedjInputDataConfig'
  InputDataConfig ->
  -- | 'spedjOutputDataConfig'
  OutputDataConfig ->
  -- | 'spedjMode'
  PiiEntitiesDetectionMode ->
  -- | 'spedjDataAccessRoleARN'
  Text ->
  -- | 'spedjLanguageCode'
  LanguageCode ->
  StartPiiEntitiesDetectionJob
startPiiEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pMode_
  pDataAccessRoleARN_
  pLanguageCode_ =
    StartPiiEntitiesDetectionJob'
      { _spedjJobName = Nothing,
        _spedjRedactionConfig = Nothing,
        _spedjClientRequestToken = Nothing,
        _spedjInputDataConfig = pInputDataConfig_,
        _spedjOutputDataConfig = pOutputDataConfig_,
        _spedjMode = pMode_,
        _spedjDataAccessRoleARN = pDataAccessRoleARN_,
        _spedjLanguageCode = pLanguageCode_
      }

-- | The identifier of the job.
spedjJobName :: Lens' StartPiiEntitiesDetectionJob (Maybe Text)
spedjJobName = lens _spedjJobName (\s a -> s {_spedjJobName = a})

-- | Provides configuration parameters for PII entity redaction. This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
spedjRedactionConfig :: Lens' StartPiiEntitiesDetectionJob (Maybe RedactionConfig)
spedjRedactionConfig = lens _spedjRedactionConfig (\s a -> s {_spedjRedactionConfig = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
spedjClientRequestToken :: Lens' StartPiiEntitiesDetectionJob (Maybe Text)
spedjClientRequestToken = lens _spedjClientRequestToken (\s a -> s {_spedjClientRequestToken = a})

-- | The input properties for a PII entities detection job.
spedjInputDataConfig :: Lens' StartPiiEntitiesDetectionJob InputDataConfig
spedjInputDataConfig = lens _spedjInputDataConfig (\s a -> s {_spedjInputDataConfig = a})

-- | Provides conﬁguration parameters for the output of PII entity detection jobs.
spedjOutputDataConfig :: Lens' StartPiiEntitiesDetectionJob OutputDataConfig
spedjOutputDataConfig = lens _spedjOutputDataConfig (\s a -> s {_spedjOutputDataConfig = a})

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
spedjMode :: Lens' StartPiiEntitiesDetectionJob PiiEntitiesDetectionMode
spedjMode = lens _spedjMode (\s a -> s {_spedjMode = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
spedjDataAccessRoleARN :: Lens' StartPiiEntitiesDetectionJob Text
spedjDataAccessRoleARN = lens _spedjDataAccessRoleARN (\s a -> s {_spedjDataAccessRoleARN = a})

-- | The language of the input documents.
spedjLanguageCode :: Lens' StartPiiEntitiesDetectionJob LanguageCode
spedjLanguageCode = lens _spedjLanguageCode (\s a -> s {_spedjLanguageCode = a})

instance AWSRequest StartPiiEntitiesDetectionJob where
  type
    Rs StartPiiEntitiesDetectionJob =
      StartPiiEntitiesDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartPiiEntitiesDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartPiiEntitiesDetectionJob

instance NFData StartPiiEntitiesDetectionJob

instance ToHeaders StartPiiEntitiesDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StartPiiEntitiesDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartPiiEntitiesDetectionJob where
  toJSON StartPiiEntitiesDetectionJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _spedjJobName,
            ("RedactionConfig" .=) <$> _spedjRedactionConfig,
            ("ClientRequestToken" .=) <$> _spedjClientRequestToken,
            Just ("InputDataConfig" .= _spedjInputDataConfig),
            Just ("OutputDataConfig" .= _spedjOutputDataConfig),
            Just ("Mode" .= _spedjMode),
            Just ("DataAccessRoleArn" .= _spedjDataAccessRoleARN),
            Just ("LanguageCode" .= _spedjLanguageCode)
          ]
      )

instance ToPath StartPiiEntitiesDetectionJob where
  toPath = const "/"

instance ToQuery StartPiiEntitiesDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { _spedjrsJobId ::
      !(Maybe Text),
    _spedjrsJobStatus ::
      !( Maybe
           JobStatus
       ),
    _spedjrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartPiiEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spedjrsJobId' - The identifier generated for the job.
--
-- * 'spedjrsJobStatus' - The status of the job.
--
-- * 'spedjrsResponseStatus' - -- | The response status code.
startPiiEntitiesDetectionJobResponse ::
  -- | 'spedjrsResponseStatus'
  Int ->
  StartPiiEntitiesDetectionJobResponse
startPiiEntitiesDetectionJobResponse pResponseStatus_ =
  StartPiiEntitiesDetectionJobResponse'
    { _spedjrsJobId = Nothing,
      _spedjrsJobStatus = Nothing,
      _spedjrsResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job.
spedjrsJobId :: Lens' StartPiiEntitiesDetectionJobResponse (Maybe Text)
spedjrsJobId = lens _spedjrsJobId (\s a -> s {_spedjrsJobId = a})

-- | The status of the job.
spedjrsJobStatus :: Lens' StartPiiEntitiesDetectionJobResponse (Maybe JobStatus)
spedjrsJobStatus = lens _spedjrsJobStatus (\s a -> s {_spedjrsJobStatus = a})

-- | -- | The response status code.
spedjrsResponseStatus :: Lens' StartPiiEntitiesDetectionJobResponse Int
spedjrsResponseStatus = lens _spedjrsResponseStatus (\s a -> s {_spedjrsResponseStatus = a})

instance NFData StartPiiEntitiesDetectionJobResponse
