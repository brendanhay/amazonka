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
-- Module      : Network.AWS.SageMaker.DescribeCompilationJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a model compilation job.
--
--
-- To create a model compilation job, use 'CreateCompilationJob' . To get information about multiple model compilation jobs, use 'ListCompilationJobs' .
--
module Network.AWS.SageMaker.DescribeCompilationJob
    (
    -- * Creating a Request
      describeCompilationJob
    , DescribeCompilationJob
    -- * Request Lenses
    , dcjCompilationJobName

    -- * Destructuring the Response
    , describeCompilationJobResponse
    , DescribeCompilationJobResponse
    -- * Response Lenses
    , dcjrsCompilationStartTime
    , dcjrsCompilationEndTime
    , dcjrsResponseStatus
    , dcjrsCompilationJobName
    , dcjrsCompilationJobARN
    , dcjrsCompilationJobStatus
    , dcjrsStoppingCondition
    , dcjrsCreationTime
    , dcjrsLastModifiedTime
    , dcjrsFailureReason
    , dcjrsModelArtifacts
    , dcjrsRoleARN
    , dcjrsInputConfig
    , dcjrsOutputConfig
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeCompilationJob' smart constructor.
newtype DescribeCompilationJob = DescribeCompilationJob'
  { _dcjCompilationJobName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCompilationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcjCompilationJobName' - The name of the model compilation job that you want information about.
describeCompilationJob
    :: Text -- ^ 'dcjCompilationJobName'
    -> DescribeCompilationJob
describeCompilationJob pCompilationJobName_ =
  DescribeCompilationJob' {_dcjCompilationJobName = pCompilationJobName_}


-- | The name of the model compilation job that you want information about.
dcjCompilationJobName :: Lens' DescribeCompilationJob Text
dcjCompilationJobName = lens _dcjCompilationJobName (\ s a -> s{_dcjCompilationJobName = a})

instance AWSRequest DescribeCompilationJob where
        type Rs DescribeCompilationJob =
             DescribeCompilationJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCompilationJobResponse' <$>
                   (x .?> "CompilationStartTime") <*>
                     (x .?> "CompilationEndTime")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "CompilationJobName")
                     <*> (x .:> "CompilationJobArn")
                     <*> (x .:> "CompilationJobStatus")
                     <*> (x .:> "StoppingCondition")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "LastModifiedTime")
                     <*> (x .:> "FailureReason")
                     <*> (x .:> "ModelArtifacts")
                     <*> (x .:> "RoleArn")
                     <*> (x .:> "InputConfig")
                     <*> (x .:> "OutputConfig"))

instance Hashable DescribeCompilationJob where

instance NFData DescribeCompilationJob where

instance ToHeaders DescribeCompilationJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeCompilationJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCompilationJob where
        toJSON DescribeCompilationJob'{..}
          = object
              (catMaybes
                 [Just
                    ("CompilationJobName" .= _dcjCompilationJobName)])

instance ToPath DescribeCompilationJob where
        toPath = const "/"

instance ToQuery DescribeCompilationJob where
        toQuery = const mempty

-- | /See:/ 'describeCompilationJobResponse' smart constructor.
data DescribeCompilationJobResponse = DescribeCompilationJobResponse'
  { _dcjrsCompilationStartTime :: !(Maybe POSIX)
  , _dcjrsCompilationEndTime   :: !(Maybe POSIX)
  , _dcjrsResponseStatus       :: !Int
  , _dcjrsCompilationJobName   :: !Text
  , _dcjrsCompilationJobARN    :: !Text
  , _dcjrsCompilationJobStatus :: !CompilationJobStatus
  , _dcjrsStoppingCondition    :: !StoppingCondition
  , _dcjrsCreationTime         :: !POSIX
  , _dcjrsLastModifiedTime     :: !POSIX
  , _dcjrsFailureReason        :: !Text
  , _dcjrsModelArtifacts       :: !ModelArtifacts
  , _dcjrsRoleARN              :: !Text
  , _dcjrsInputConfig          :: !InputConfig
  , _dcjrsOutputConfig         :: !OutputConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCompilationJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcjrsCompilationStartTime' - The time when the model compilation job started the @CompilationJob@ instances.  You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
--
-- * 'dcjrsCompilationEndTime' - The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
--
-- * 'dcjrsResponseStatus' - -- | The response status code.
--
-- * 'dcjrsCompilationJobName' - The name of the model compilation job.
--
-- * 'dcjrsCompilationJobARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
--
-- * 'dcjrsCompilationJobStatus' - The status of the model compilation job.
--
-- * 'dcjrsStoppingCondition' - The duration allowed for model compilation.
--
-- * 'dcjrsCreationTime' - The time that the model compilation job was created.
--
-- * 'dcjrsLastModifiedTime' - The time that the status of the model compilation job was last modified.
--
-- * 'dcjrsFailureReason' - If a model compilation job failed, the reason it failed.
--
-- * 'dcjrsModelArtifacts' - Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
--
-- * 'dcjrsRoleARN' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- * 'dcjrsInputConfig' - Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- * 'dcjrsOutputConfig' - Information about the output location for the compiled model and the target device that the model runs on.
describeCompilationJobResponse
    :: Int -- ^ 'dcjrsResponseStatus'
    -> Text -- ^ 'dcjrsCompilationJobName'
    -> Text -- ^ 'dcjrsCompilationJobARN'
    -> CompilationJobStatus -- ^ 'dcjrsCompilationJobStatus'
    -> StoppingCondition -- ^ 'dcjrsStoppingCondition'
    -> UTCTime -- ^ 'dcjrsCreationTime'
    -> UTCTime -- ^ 'dcjrsLastModifiedTime'
    -> Text -- ^ 'dcjrsFailureReason'
    -> ModelArtifacts -- ^ 'dcjrsModelArtifacts'
    -> Text -- ^ 'dcjrsRoleARN'
    -> InputConfig -- ^ 'dcjrsInputConfig'
    -> OutputConfig -- ^ 'dcjrsOutputConfig'
    -> DescribeCompilationJobResponse
describeCompilationJobResponse pResponseStatus_ pCompilationJobName_ pCompilationJobARN_ pCompilationJobStatus_ pStoppingCondition_ pCreationTime_ pLastModifiedTime_ pFailureReason_ pModelArtifacts_ pRoleARN_ pInputConfig_ pOutputConfig_ =
  DescribeCompilationJobResponse'
    { _dcjrsCompilationStartTime = Nothing
    , _dcjrsCompilationEndTime = Nothing
    , _dcjrsResponseStatus = pResponseStatus_
    , _dcjrsCompilationJobName = pCompilationJobName_
    , _dcjrsCompilationJobARN = pCompilationJobARN_
    , _dcjrsCompilationJobStatus = pCompilationJobStatus_
    , _dcjrsStoppingCondition = pStoppingCondition_
    , _dcjrsCreationTime = _Time # pCreationTime_
    , _dcjrsLastModifiedTime = _Time # pLastModifiedTime_
    , _dcjrsFailureReason = pFailureReason_
    , _dcjrsModelArtifacts = pModelArtifacts_
    , _dcjrsRoleARN = pRoleARN_
    , _dcjrsInputConfig = pInputConfig_
    , _dcjrsOutputConfig = pOutputConfig_
    }


-- | The time when the model compilation job started the @CompilationJob@ instances.  You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
dcjrsCompilationStartTime :: Lens' DescribeCompilationJobResponse (Maybe UTCTime)
dcjrsCompilationStartTime = lens _dcjrsCompilationStartTime (\ s a -> s{_dcjrsCompilationStartTime = a}) . mapping _Time

-- | The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
dcjrsCompilationEndTime :: Lens' DescribeCompilationJobResponse (Maybe UTCTime)
dcjrsCompilationEndTime = lens _dcjrsCompilationEndTime (\ s a -> s{_dcjrsCompilationEndTime = a}) . mapping _Time

-- | -- | The response status code.
dcjrsResponseStatus :: Lens' DescribeCompilationJobResponse Int
dcjrsResponseStatus = lens _dcjrsResponseStatus (\ s a -> s{_dcjrsResponseStatus = a})

-- | The name of the model compilation job.
dcjrsCompilationJobName :: Lens' DescribeCompilationJobResponse Text
dcjrsCompilationJobName = lens _dcjrsCompilationJobName (\ s a -> s{_dcjrsCompilationJobName = a})

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
dcjrsCompilationJobARN :: Lens' DescribeCompilationJobResponse Text
dcjrsCompilationJobARN = lens _dcjrsCompilationJobARN (\ s a -> s{_dcjrsCompilationJobARN = a})

-- | The status of the model compilation job.
dcjrsCompilationJobStatus :: Lens' DescribeCompilationJobResponse CompilationJobStatus
dcjrsCompilationJobStatus = lens _dcjrsCompilationJobStatus (\ s a -> s{_dcjrsCompilationJobStatus = a})

-- | The duration allowed for model compilation.
dcjrsStoppingCondition :: Lens' DescribeCompilationJobResponse StoppingCondition
dcjrsStoppingCondition = lens _dcjrsStoppingCondition (\ s a -> s{_dcjrsStoppingCondition = a})

-- | The time that the model compilation job was created.
dcjrsCreationTime :: Lens' DescribeCompilationJobResponse UTCTime
dcjrsCreationTime = lens _dcjrsCreationTime (\ s a -> s{_dcjrsCreationTime = a}) . _Time

-- | The time that the status of the model compilation job was last modified.
dcjrsLastModifiedTime :: Lens' DescribeCompilationJobResponse UTCTime
dcjrsLastModifiedTime = lens _dcjrsLastModifiedTime (\ s a -> s{_dcjrsLastModifiedTime = a}) . _Time

-- | If a model compilation job failed, the reason it failed.
dcjrsFailureReason :: Lens' DescribeCompilationJobResponse Text
dcjrsFailureReason = lens _dcjrsFailureReason (\ s a -> s{_dcjrsFailureReason = a})

-- | Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
dcjrsModelArtifacts :: Lens' DescribeCompilationJobResponse ModelArtifacts
dcjrsModelArtifacts = lens _dcjrsModelArtifacts (\ s a -> s{_dcjrsModelArtifacts = a})

-- | The Amazon Resource Name (ARN) of the model compilation job.
dcjrsRoleARN :: Lens' DescribeCompilationJobResponse Text
dcjrsRoleARN = lens _dcjrsRoleARN (\ s a -> s{_dcjrsRoleARN = a})

-- | Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
dcjrsInputConfig :: Lens' DescribeCompilationJobResponse InputConfig
dcjrsInputConfig = lens _dcjrsInputConfig (\ s a -> s{_dcjrsInputConfig = a})

-- | Information about the output location for the compiled model and the target device that the model runs on.
dcjrsOutputConfig :: Lens' DescribeCompilationJobResponse OutputConfig
dcjrsOutputConfig = lens _dcjrsOutputConfig (\ s a -> s{_dcjrsOutputConfig = a})

instance NFData DescribeCompilationJobResponse where
