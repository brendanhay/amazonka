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
-- Module      : Network.AWS.Comprehend.StartEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous event detection job for a collection of documents.
module Network.AWS.Comprehend.StartEventsDetectionJob
  ( -- * Creating a Request
    startEventsDetectionJob,
    StartEventsDetectionJob,

    -- * Request Lenses
    sJobName,
    sClientRequestToken,
    sInputDataConfig,
    sOutputDataConfig,
    sDataAccessRoleARN,
    sLanguageCode,
    sTargetEventTypes,

    -- * Destructuring the Response
    startEventsDetectionJobResponse,
    StartEventsDetectionJobResponse,

    -- * Response Lenses
    starsJobId,
    starsJobStatus,
    starsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { _sJobName ::
      !(Maybe Text),
    _sClientRequestToken :: !(Maybe Text),
    _sInputDataConfig :: !InputDataConfig,
    _sOutputDataConfig :: !OutputDataConfig,
    _sDataAccessRoleARN :: !Text,
    _sLanguageCode :: !LanguageCode,
    _sTargetEventTypes :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartEventsDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sJobName' - The identifier of the events detection job.
--
-- * 'sClientRequestToken' - An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'sInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'sOutputDataConfig' - Specifies where to send the output files.
--
-- * 'sDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- * 'sLanguageCode' - The language code of the input documents.
--
-- * 'sTargetEventTypes' - The types of events to detect in the input documents.
startEventsDetectionJob ::
  -- | 'sInputDataConfig'
  InputDataConfig ->
  -- | 'sOutputDataConfig'
  OutputDataConfig ->
  -- | 'sDataAccessRoleARN'
  Text ->
  -- | 'sLanguageCode'
  LanguageCode ->
  -- | 'sTargetEventTypes'
  NonEmpty Text ->
  StartEventsDetectionJob
startEventsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pLanguageCode_
  pTargetEventTypes_ =
    StartEventsDetectionJob'
      { _sJobName = Nothing,
        _sClientRequestToken = Nothing,
        _sInputDataConfig = pInputDataConfig_,
        _sOutputDataConfig = pOutputDataConfig_,
        _sDataAccessRoleARN = pDataAccessRoleARN_,
        _sLanguageCode = pLanguageCode_,
        _sTargetEventTypes = _List1 # pTargetEventTypes_
      }

-- | The identifier of the events detection job.
sJobName :: Lens' StartEventsDetectionJob (Maybe Text)
sJobName = lens _sJobName (\s a -> s {_sJobName = a})

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
sClientRequestToken :: Lens' StartEventsDetectionJob (Maybe Text)
sClientRequestToken = lens _sClientRequestToken (\s a -> s {_sClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
sInputDataConfig :: Lens' StartEventsDetectionJob InputDataConfig
sInputDataConfig = lens _sInputDataConfig (\s a -> s {_sInputDataConfig = a})

-- | Specifies where to send the output files.
sOutputDataConfig :: Lens' StartEventsDetectionJob OutputDataConfig
sOutputDataConfig = lens _sOutputDataConfig (\s a -> s {_sOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
sDataAccessRoleARN :: Lens' StartEventsDetectionJob Text
sDataAccessRoleARN = lens _sDataAccessRoleARN (\s a -> s {_sDataAccessRoleARN = a})

-- | The language code of the input documents.
sLanguageCode :: Lens' StartEventsDetectionJob LanguageCode
sLanguageCode = lens _sLanguageCode (\s a -> s {_sLanguageCode = a})

-- | The types of events to detect in the input documents.
sTargetEventTypes :: Lens' StartEventsDetectionJob (NonEmpty Text)
sTargetEventTypes = lens _sTargetEventTypes (\s a -> s {_sTargetEventTypes = a}) . _List1

instance AWSRequest StartEventsDetectionJob where
  type Rs StartEventsDetectionJob = StartEventsDetectionJobResponse
  request = postJSON comprehend
  response =
    receiveJSON
      ( \s h x ->
          StartEventsDetectionJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartEventsDetectionJob

instance NFData StartEventsDetectionJob

instance ToHeaders StartEventsDetectionJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Comprehend_20171127.StartEventsDetectionJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartEventsDetectionJob where
  toJSON StartEventsDetectionJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _sJobName,
            ("ClientRequestToken" .=) <$> _sClientRequestToken,
            Just ("InputDataConfig" .= _sInputDataConfig),
            Just ("OutputDataConfig" .= _sOutputDataConfig),
            Just ("DataAccessRoleArn" .= _sDataAccessRoleARN),
            Just ("LanguageCode" .= _sLanguageCode),
            Just ("TargetEventTypes" .= _sTargetEventTypes)
          ]
      )

instance ToPath StartEventsDetectionJob where
  toPath = const "/"

instance ToQuery StartEventsDetectionJob where
  toQuery = const mempty

-- | /See:/ 'startEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
  { _starsJobId ::
      !(Maybe Text),
    _starsJobStatus ::
      !(Maybe JobStatus),
    _starsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartEventsDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'starsJobId' - An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'starsJobStatus' - The status of the events detection job.
--
-- * 'starsResponseStatus' - -- | The response status code.
startEventsDetectionJobResponse ::
  -- | 'starsResponseStatus'
  Int ->
  StartEventsDetectionJobResponse
startEventsDetectionJobResponse pResponseStatus_ =
  StartEventsDetectionJobResponse'
    { _starsJobId = Nothing,
      _starsJobStatus = Nothing,
      _starsResponseStatus = pResponseStatus_
    }

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
starsJobId :: Lens' StartEventsDetectionJobResponse (Maybe Text)
starsJobId = lens _starsJobId (\s a -> s {_starsJobId = a})

-- | The status of the events detection job.
starsJobStatus :: Lens' StartEventsDetectionJobResponse (Maybe JobStatus)
starsJobStatus = lens _starsJobStatus (\s a -> s {_starsJobStatus = a})

-- | -- | The response status code.
starsResponseStatus :: Lens' StartEventsDetectionJobResponse Int
starsResponseStatus = lens _starsResponseStatus (\s a -> s {_starsResponseStatus = a})

instance NFData StartEventsDetectionJobResponse
