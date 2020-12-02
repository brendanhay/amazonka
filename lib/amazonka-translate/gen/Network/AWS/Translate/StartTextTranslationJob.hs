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
-- Module      : Network.AWS.Translate.StartTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous batch translation job. Batch translation jobs can be used to translate large volumes of text across multiple documents at once. For more information, see 'async' .
--
--
-- Batch translation jobs can be described with the 'DescribeTextTranslationJob' operation, listed with the 'ListTextTranslationJobs' operation, and stopped with the 'StopTextTranslationJob' operation.
module Network.AWS.Translate.StartTextTranslationJob
  ( -- * Creating a Request
    startTextTranslationJob,
    StartTextTranslationJob,

    -- * Request Lenses
    sttjJobName,
    sttjParallelDataNames,
    sttjTerminologyNames,
    sttjInputDataConfig,
    sttjOutputDataConfig,
    sttjDataAccessRoleARN,
    sttjSourceLanguageCode,
    sttjTargetLanguageCodes,
    sttjClientToken,

    -- * Destructuring the Response
    startTextTranslationJobResponse,
    StartTextTranslationJobResponse,

    -- * Response Lenses
    srsJobId,
    srsJobStatus,
    srsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'startTextTranslationJob' smart constructor.
data StartTextTranslationJob = StartTextTranslationJob'
  { _sttjJobName ::
      !(Maybe Text),
    _sttjParallelDataNames :: !(Maybe [Text]),
    _sttjTerminologyNames :: !(Maybe [Text]),
    _sttjInputDataConfig :: !InputDataConfig,
    _sttjOutputDataConfig :: !OutputDataConfig,
    _sttjDataAccessRoleARN :: !Text,
    _sttjSourceLanguageCode :: !Text,
    _sttjTargetLanguageCodes :: !(List1 Text),
    _sttjClientToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTextTranslationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sttjJobName' - The name of the batch translation job to be performed.
--
-- * 'sttjParallelDataNames' - The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
--
-- * 'sttjTerminologyNames' - The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
--
-- * 'sttjInputDataConfig' - Specifies the format and S3 location of the input documents for the translation job.
--
-- * 'sttjOutputDataConfig' - Specifies the S3 folder to which your job output will be saved.
--
-- * 'sttjDataAccessRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
--
-- * 'sttjSourceLanguageCode' - The language code of the input language. For a list of language codes, see 'what-is-languages' . Amazon Translate does not automatically detect a source language during batch translation jobs.
--
-- * 'sttjTargetLanguageCodes' - The language code of the output language.
--
-- * 'sttjClientToken' - A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
startTextTranslationJob ::
  -- | 'sttjInputDataConfig'
  InputDataConfig ->
  -- | 'sttjOutputDataConfig'
  OutputDataConfig ->
  -- | 'sttjDataAccessRoleARN'
  Text ->
  -- | 'sttjSourceLanguageCode'
  Text ->
  -- | 'sttjTargetLanguageCodes'
  NonEmpty Text ->
  -- | 'sttjClientToken'
  Text ->
  StartTextTranslationJob
startTextTranslationJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pSourceLanguageCode_
  pTargetLanguageCodes_
  pClientToken_ =
    StartTextTranslationJob'
      { _sttjJobName = Nothing,
        _sttjParallelDataNames = Nothing,
        _sttjTerminologyNames = Nothing,
        _sttjInputDataConfig = pInputDataConfig_,
        _sttjOutputDataConfig = pOutputDataConfig_,
        _sttjDataAccessRoleARN = pDataAccessRoleARN_,
        _sttjSourceLanguageCode = pSourceLanguageCode_,
        _sttjTargetLanguageCodes = _List1 # pTargetLanguageCodes_,
        _sttjClientToken = pClientToken_
      }

-- | The name of the batch translation job to be performed.
sttjJobName :: Lens' StartTextTranslationJob (Maybe Text)
sttjJobName = lens _sttjJobName (\s a -> s {_sttjJobName = a})

-- | The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
sttjParallelDataNames :: Lens' StartTextTranslationJob [Text]
sttjParallelDataNames = lens _sttjParallelDataNames (\s a -> s {_sttjParallelDataNames = a}) . _Default . _Coerce

-- | The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
sttjTerminologyNames :: Lens' StartTextTranslationJob [Text]
sttjTerminologyNames = lens _sttjTerminologyNames (\s a -> s {_sttjTerminologyNames = a}) . _Default . _Coerce

-- | Specifies the format and S3 location of the input documents for the translation job.
sttjInputDataConfig :: Lens' StartTextTranslationJob InputDataConfig
sttjInputDataConfig = lens _sttjInputDataConfig (\s a -> s {_sttjInputDataConfig = a})

-- | Specifies the S3 folder to which your job output will be saved.
sttjOutputDataConfig :: Lens' StartTextTranslationJob OutputDataConfig
sttjOutputDataConfig = lens _sttjOutputDataConfig (\s a -> s {_sttjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
sttjDataAccessRoleARN :: Lens' StartTextTranslationJob Text
sttjDataAccessRoleARN = lens _sttjDataAccessRoleARN (\s a -> s {_sttjDataAccessRoleARN = a})

-- | The language code of the input language. For a list of language codes, see 'what-is-languages' . Amazon Translate does not automatically detect a source language during batch translation jobs.
sttjSourceLanguageCode :: Lens' StartTextTranslationJob Text
sttjSourceLanguageCode = lens _sttjSourceLanguageCode (\s a -> s {_sttjSourceLanguageCode = a})

-- | The language code of the output language.
sttjTargetLanguageCodes :: Lens' StartTextTranslationJob (NonEmpty Text)
sttjTargetLanguageCodes = lens _sttjTargetLanguageCodes (\s a -> s {_sttjTargetLanguageCodes = a}) . _List1

-- | A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
sttjClientToken :: Lens' StartTextTranslationJob Text
sttjClientToken = lens _sttjClientToken (\s a -> s {_sttjClientToken = a})

instance AWSRequest StartTextTranslationJob where
  type Rs StartTextTranslationJob = StartTextTranslationJobResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          StartTextTranslationJobResponse'
            <$> (x .?> "JobId") <*> (x .?> "JobStatus") <*> (pure (fromEnum s))
      )

instance Hashable StartTextTranslationJob

instance NFData StartTextTranslationJob

instance ToHeaders StartTextTranslationJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.StartTextTranslationJob" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartTextTranslationJob where
  toJSON StartTextTranslationJob' {..} =
    object
      ( catMaybes
          [ ("JobName" .=) <$> _sttjJobName,
            ("ParallelDataNames" .=) <$> _sttjParallelDataNames,
            ("TerminologyNames" .=) <$> _sttjTerminologyNames,
            Just ("InputDataConfig" .= _sttjInputDataConfig),
            Just ("OutputDataConfig" .= _sttjOutputDataConfig),
            Just ("DataAccessRoleArn" .= _sttjDataAccessRoleARN),
            Just ("SourceLanguageCode" .= _sttjSourceLanguageCode),
            Just ("TargetLanguageCodes" .= _sttjTargetLanguageCodes),
            Just ("ClientToken" .= _sttjClientToken)
          ]
      )

instance ToPath StartTextTranslationJob where
  toPath = const "/"

instance ToQuery StartTextTranslationJob where
  toQuery = const mempty

-- | /See:/ 'startTextTranslationJobResponse' smart constructor.
data StartTextTranslationJobResponse = StartTextTranslationJobResponse'
  { _srsJobId ::
      !(Maybe Text),
    _srsJobStatus ::
      !(Maybe JobStatus),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsJobId' - The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
--
-- * 'srsJobStatus' - The status of the job. Possible values include:     * @SUBMITTED@ - The job has been received and is queued for processing.     * @IN_PROGRESS@ - Amazon Translate is processing the job.     * @COMPLETED@ - The job was successfully completed and the output is available.     * @COMPLETED_WITH_ERROR@ - The job was completed with errors. The errors can be analyzed in the job's output.     * @FAILED@ - The job did not complete. To get details, use the 'DescribeTextTranslationJob' operation.     * @STOP_REQUESTED@ - The user who started the job has requested that it be stopped.     * @STOPPED@ - The job has been stopped.
--
-- * 'srsResponseStatus' - -- | The response status code.
startTextTranslationJobResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartTextTranslationJobResponse
startTextTranslationJobResponse pResponseStatus_ =
  StartTextTranslationJobResponse'
    { _srsJobId = Nothing,
      _srsJobStatus = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
srsJobId :: Lens' StartTextTranslationJobResponse (Maybe Text)
srsJobId = lens _srsJobId (\s a -> s {_srsJobId = a})

-- | The status of the job. Possible values include:     * @SUBMITTED@ - The job has been received and is queued for processing.     * @IN_PROGRESS@ - Amazon Translate is processing the job.     * @COMPLETED@ - The job was successfully completed and the output is available.     * @COMPLETED_WITH_ERROR@ - The job was completed with errors. The errors can be analyzed in the job's output.     * @FAILED@ - The job did not complete. To get details, use the 'DescribeTextTranslationJob' operation.     * @STOP_REQUESTED@ - The user who started the job has requested that it be stopped.     * @STOPPED@ - The job has been stopped.
srsJobStatus :: Lens' StartTextTranslationJobResponse (Maybe JobStatus)
srsJobStatus = lens _srsJobStatus (\s a -> s {_srsJobStatus = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartTextTranslationJobResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartTextTranslationJobResponse
