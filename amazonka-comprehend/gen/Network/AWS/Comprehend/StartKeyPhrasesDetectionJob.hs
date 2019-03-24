{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous key phrase detection job for a collection of documents. Use the operation to track the status of a job.
--
--
module Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
    (
    -- * Creating a Request
      startKeyPhrasesDetectionJob
    , StartKeyPhrasesDetectionJob
    -- * Request Lenses
    , skpdjJobName
    , skpdjClientRequestToken
    , skpdjInputDataConfig
    , skpdjOutputDataConfig
    , skpdjDataAccessRoleARN
    , skpdjLanguageCode

    -- * Destructuring the Response
    , startKeyPhrasesDetectionJobResponse
    , StartKeyPhrasesDetectionJobResponse
    -- * Response Lenses
    , starsJobId
    , starsJobStatus
    , starsResponseStatus
    ) where

import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startKeyPhrasesDetectionJob' smart constructor.
data StartKeyPhrasesDetectionJob = StartKeyPhrasesDetectionJob'
  { _skpdjJobName            :: !(Maybe Text)
  , _skpdjClientRequestToken :: !(Maybe Text)
  , _skpdjInputDataConfig    :: !InputDataConfig
  , _skpdjOutputDataConfig   :: !OutputDataConfig
  , _skpdjDataAccessRoleARN  :: !Text
  , _skpdjLanguageCode       :: !LanguageCode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartKeyPhrasesDetectionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpdjJobName' - The identifier of the job.
--
-- * 'skpdjClientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- * 'skpdjInputDataConfig' - Specifies the format and location of the input data for the job.
--
-- * 'skpdjOutputDataConfig' - Specifies where to send the output files.
--
-- * 'skpdjDataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
--
-- * 'skpdjLanguageCode' - The language of the input documents. You can specify English ("en") or Spanish ("es"). All documents must be in the same language.
startKeyPhrasesDetectionJob
    :: InputDataConfig -- ^ 'skpdjInputDataConfig'
    -> OutputDataConfig -- ^ 'skpdjOutputDataConfig'
    -> Text -- ^ 'skpdjDataAccessRoleARN'
    -> LanguageCode -- ^ 'skpdjLanguageCode'
    -> StartKeyPhrasesDetectionJob
startKeyPhrasesDetectionJob pInputDataConfig_ pOutputDataConfig_ pDataAccessRoleARN_ pLanguageCode_ =
  StartKeyPhrasesDetectionJob'
    { _skpdjJobName = Nothing
    , _skpdjClientRequestToken = Nothing
    , _skpdjInputDataConfig = pInputDataConfig_
    , _skpdjOutputDataConfig = pOutputDataConfig_
    , _skpdjDataAccessRoleARN = pDataAccessRoleARN_
    , _skpdjLanguageCode = pLanguageCode_
    }


-- | The identifier of the job.
skpdjJobName :: Lens' StartKeyPhrasesDetectionJob (Maybe Text)
skpdjJobName = lens _skpdjJobName (\ s a -> s{_skpdjJobName = a})

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
skpdjClientRequestToken :: Lens' StartKeyPhrasesDetectionJob (Maybe Text)
skpdjClientRequestToken = lens _skpdjClientRequestToken (\ s a -> s{_skpdjClientRequestToken = a})

-- | Specifies the format and location of the input data for the job.
skpdjInputDataConfig :: Lens' StartKeyPhrasesDetectionJob InputDataConfig
skpdjInputDataConfig = lens _skpdjInputDataConfig (\ s a -> s{_skpdjInputDataConfig = a})

-- | Specifies where to send the output files.
skpdjOutputDataConfig :: Lens' StartKeyPhrasesDetectionJob OutputDataConfig
skpdjOutputDataConfig = lens _skpdjOutputDataConfig (\ s a -> s{_skpdjOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data. For more information, see <https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions https://docs.aws.amazon.com/comprehend/latest/dg/access-control-managing-permissions.html#auth-role-permissions> .
skpdjDataAccessRoleARN :: Lens' StartKeyPhrasesDetectionJob Text
skpdjDataAccessRoleARN = lens _skpdjDataAccessRoleARN (\ s a -> s{_skpdjDataAccessRoleARN = a})

-- | The language of the input documents. You can specify English ("en") or Spanish ("es"). All documents must be in the same language.
skpdjLanguageCode :: Lens' StartKeyPhrasesDetectionJob LanguageCode
skpdjLanguageCode = lens _skpdjLanguageCode (\ s a -> s{_skpdjLanguageCode = a})

instance AWSRequest StartKeyPhrasesDetectionJob where
        type Rs StartKeyPhrasesDetectionJob =
             StartKeyPhrasesDetectionJobResponse
        request = postJSON comprehend
        response
          = receiveJSON
              (\ s h x ->
                 StartKeyPhrasesDetectionJobResponse' <$>
                   (x .?> "JobId") <*> (x .?> "JobStatus") <*>
                     (pure (fromEnum s)))

instance Hashable StartKeyPhrasesDetectionJob where

instance NFData StartKeyPhrasesDetectionJob where

instance ToHeaders StartKeyPhrasesDetectionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Comprehend_20171127.StartKeyPhrasesDetectionJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartKeyPhrasesDetectionJob where
        toJSON StartKeyPhrasesDetectionJob'{..}
          = object
              (catMaybes
                 [("JobName" .=) <$> _skpdjJobName,
                  ("ClientRequestToken" .=) <$>
                    _skpdjClientRequestToken,
                  Just ("InputDataConfig" .= _skpdjInputDataConfig),
                  Just ("OutputDataConfig" .= _skpdjOutputDataConfig),
                  Just
                    ("DataAccessRoleArn" .= _skpdjDataAccessRoleARN),
                  Just ("LanguageCode" .= _skpdjLanguageCode)])

instance ToPath StartKeyPhrasesDetectionJob where
        toPath = const "/"

instance ToQuery StartKeyPhrasesDetectionJob where
        toQuery = const mempty

-- | /See:/ 'startKeyPhrasesDetectionJobResponse' smart constructor.
data StartKeyPhrasesDetectionJobResponse = StartKeyPhrasesDetectionJobResponse'
  { _starsJobId          :: !(Maybe Text)
  , _starsJobStatus      :: !(Maybe JobStatus)
  , _starsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartKeyPhrasesDetectionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'starsJobId' - The identifier generated for the job. To get the status of a job, use this identifier with the operation.
--
-- * 'starsJobStatus' - The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
--
-- * 'starsResponseStatus' - -- | The response status code.
startKeyPhrasesDetectionJobResponse
    :: Int -- ^ 'starsResponseStatus'
    -> StartKeyPhrasesDetectionJobResponse
startKeyPhrasesDetectionJobResponse pResponseStatus_ =
  StartKeyPhrasesDetectionJobResponse'
    { _starsJobId = Nothing
    , _starsJobStatus = Nothing
    , _starsResponseStatus = pResponseStatus_
    }


-- | The identifier generated for the job. To get the status of a job, use this identifier with the operation.
starsJobId :: Lens' StartKeyPhrasesDetectionJobResponse (Maybe Text)
starsJobId = lens _starsJobId (\ s a -> s{_starsJobId = a})

-- | The status of the job.      * SUBMITTED - The job has been received and is queued for processing.     * IN_PROGRESS - Amazon Comprehend is processing the job.     * COMPLETED - The job was successfully completed and the output is available.     * FAILED - The job did not complete. To get details, use the operation.
starsJobStatus :: Lens' StartKeyPhrasesDetectionJobResponse (Maybe JobStatus)
starsJobStatus = lens _starsJobStatus (\ s a -> s{_starsJobStatus = a})

-- | -- | The response status code.
starsResponseStatus :: Lens' StartKeyPhrasesDetectionJobResponse Int
starsResponseStatus = lens _starsResponseStatus (\ s a -> s{_starsResponseStatus = a})

instance NFData StartKeyPhrasesDetectionJobResponse
         where
