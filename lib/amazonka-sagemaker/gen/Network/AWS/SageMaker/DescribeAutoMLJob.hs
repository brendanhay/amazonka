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
-- Module      : Network.AWS.SageMaker.DescribeAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon SageMaker job.
module Network.AWS.SageMaker.DescribeAutoMLJob
  ( -- * Creating a Request
    describeAutoMLJob,
    DescribeAutoMLJob,

    -- * Request Lenses
    damljAutoMLJobName,

    -- * Destructuring the Response
    describeAutoMLJobResponse,
    DescribeAutoMLJobResponse,

    -- * Response Lenses
    damljrsGenerateCandidateDefinitionsOnly,
    damljrsFailureReason,
    damljrsProblemType,
    damljrsAutoMLJobConfig,
    damljrsAutoMLJobObjective,
    damljrsAutoMLJobArtifacts,
    damljrsResolvedAttributes,
    damljrsEndTime,
    damljrsBestCandidate,
    damljrsResponseStatus,
    damljrsAutoMLJobName,
    damljrsAutoMLJobARN,
    damljrsInputDataConfig,
    damljrsOutputDataConfig,
    damljrsRoleARN,
    damljrsCreationTime,
    damljrsLastModifiedTime,
    damljrsAutoMLJobStatus,
    damljrsAutoMLJobSecondaryStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeAutoMLJob' smart constructor.
newtype DescribeAutoMLJob = DescribeAutoMLJob'
  { _damljAutoMLJobName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAutoMLJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damljAutoMLJobName' - Request information about a job using that job's unique name.
describeAutoMLJob ::
  -- | 'damljAutoMLJobName'
  Text ->
  DescribeAutoMLJob
describeAutoMLJob pAutoMLJobName_ =
  DescribeAutoMLJob' {_damljAutoMLJobName = pAutoMLJobName_}

-- | Request information about a job using that job's unique name.
damljAutoMLJobName :: Lens' DescribeAutoMLJob Text
damljAutoMLJobName = lens _damljAutoMLJobName (\s a -> s {_damljAutoMLJobName = a})

instance AWSRequest DescribeAutoMLJob where
  type Rs DescribeAutoMLJob = DescribeAutoMLJobResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeAutoMLJobResponse'
            <$> (x .?> "GenerateCandidateDefinitionsOnly")
            <*> (x .?> "FailureReason")
            <*> (x .?> "ProblemType")
            <*> (x .?> "AutoMLJobConfig")
            <*> (x .?> "AutoMLJobObjective")
            <*> (x .?> "AutoMLJobArtifacts")
            <*> (x .?> "ResolvedAttributes")
            <*> (x .?> "EndTime")
            <*> (x .?> "BestCandidate")
            <*> (pure (fromEnum s))
            <*> (x .:> "AutoMLJobName")
            <*> (x .:> "AutoMLJobArn")
            <*> (x .:> "InputDataConfig")
            <*> (x .:> "OutputDataConfig")
            <*> (x .:> "RoleArn")
            <*> (x .:> "CreationTime")
            <*> (x .:> "LastModifiedTime")
            <*> (x .:> "AutoMLJobStatus")
            <*> (x .:> "AutoMLJobSecondaryStatus")
      )

instance Hashable DescribeAutoMLJob

instance NFData DescribeAutoMLJob

instance ToHeaders DescribeAutoMLJob where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DescribeAutoMLJob" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeAutoMLJob where
  toJSON DescribeAutoMLJob' {..} =
    object
      (catMaybes [Just ("AutoMLJobName" .= _damljAutoMLJobName)])

instance ToPath DescribeAutoMLJob where
  toPath = const "/"

instance ToQuery DescribeAutoMLJob where
  toQuery = const mempty

-- | /See:/ 'describeAutoMLJobResponse' smart constructor.
data DescribeAutoMLJobResponse = DescribeAutoMLJobResponse'
  { _damljrsGenerateCandidateDefinitionsOnly ::
      !(Maybe Bool),
    _damljrsFailureReason :: !(Maybe Text),
    _damljrsProblemType ::
      !(Maybe ProblemType),
    _damljrsAutoMLJobConfig ::
      !(Maybe AutoMLJobConfig),
    _damljrsAutoMLJobObjective ::
      !(Maybe AutoMLJobObjective),
    _damljrsAutoMLJobArtifacts ::
      !(Maybe AutoMLJobArtifacts),
    _damljrsResolvedAttributes ::
      !(Maybe ResolvedAttributes),
    _damljrsEndTime :: !(Maybe POSIX),
    _damljrsBestCandidate ::
      !(Maybe AutoMLCandidate),
    _damljrsResponseStatus :: !Int,
    _damljrsAutoMLJobName :: !Text,
    _damljrsAutoMLJobARN :: !Text,
    _damljrsInputDataConfig ::
      !(List1 AutoMLChannel),
    _damljrsOutputDataConfig ::
      !AutoMLOutputDataConfig,
    _damljrsRoleARN :: !Text,
    _damljrsCreationTime :: !POSIX,
    _damljrsLastModifiedTime :: !POSIX,
    _damljrsAutoMLJobStatus ::
      !AutoMLJobStatus,
    _damljrsAutoMLJobSecondaryStatus ::
      !AutoMLJobSecondaryStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAutoMLJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'damljrsGenerateCandidateDefinitionsOnly' - Returns the job's output from GenerateCandidateDefinitionsOnly.
--
-- * 'damljrsFailureReason' - Returns the job's FailureReason.
--
-- * 'damljrsProblemType' - Returns the job's problem type.
--
-- * 'damljrsAutoMLJobConfig' - Returns the job's config.
--
-- * 'damljrsAutoMLJobObjective' - Returns the job's objective.
--
-- * 'damljrsAutoMLJobArtifacts' - Returns information on the job's artifacts found in AutoMLJobArtifacts.
--
-- * 'damljrsResolvedAttributes' - This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
--
-- * 'damljrsEndTime' - Returns the job's end time.
--
-- * 'damljrsBestCandidate' - Returns the job's BestCandidate.
--
-- * 'damljrsResponseStatus' - -- | The response status code.
--
-- * 'damljrsAutoMLJobName' - Returns the name of a job.
--
-- * 'damljrsAutoMLJobARN' - Returns the job's ARN.
--
-- * 'damljrsInputDataConfig' - Returns the job's input data config.
--
-- * 'damljrsOutputDataConfig' - Returns the job's output data config.
--
-- * 'damljrsRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
--
-- * 'damljrsCreationTime' - Returns the job's creation time.
--
-- * 'damljrsLastModifiedTime' - Returns the job's last modified time.
--
-- * 'damljrsAutoMLJobStatus' - Returns the job's AutoMLJobStatus.
--
-- * 'damljrsAutoMLJobSecondaryStatus' - Returns the job's AutoMLJobSecondaryStatus.
describeAutoMLJobResponse ::
  -- | 'damljrsResponseStatus'
  Int ->
  -- | 'damljrsAutoMLJobName'
  Text ->
  -- | 'damljrsAutoMLJobARN'
  Text ->
  -- | 'damljrsInputDataConfig'
  NonEmpty AutoMLChannel ->
  -- | 'damljrsOutputDataConfig'
  AutoMLOutputDataConfig ->
  -- | 'damljrsRoleARN'
  Text ->
  -- | 'damljrsCreationTime'
  UTCTime ->
  -- | 'damljrsLastModifiedTime'
  UTCTime ->
  -- | 'damljrsAutoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'damljrsAutoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  DescribeAutoMLJobResponse
describeAutoMLJobResponse
  pResponseStatus_
  pAutoMLJobName_
  pAutoMLJobARN_
  pInputDataConfig_
  pOutputDataConfig_
  pRoleARN_
  pCreationTime_
  pLastModifiedTime_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_ =
    DescribeAutoMLJobResponse'
      { _damljrsGenerateCandidateDefinitionsOnly =
          Nothing,
        _damljrsFailureReason = Nothing,
        _damljrsProblemType = Nothing,
        _damljrsAutoMLJobConfig = Nothing,
        _damljrsAutoMLJobObjective = Nothing,
        _damljrsAutoMLJobArtifacts = Nothing,
        _damljrsResolvedAttributes = Nothing,
        _damljrsEndTime = Nothing,
        _damljrsBestCandidate = Nothing,
        _damljrsResponseStatus = pResponseStatus_,
        _damljrsAutoMLJobName = pAutoMLJobName_,
        _damljrsAutoMLJobARN = pAutoMLJobARN_,
        _damljrsInputDataConfig = _List1 # pInputDataConfig_,
        _damljrsOutputDataConfig = pOutputDataConfig_,
        _damljrsRoleARN = pRoleARN_,
        _damljrsCreationTime = _Time # pCreationTime_,
        _damljrsLastModifiedTime = _Time # pLastModifiedTime_,
        _damljrsAutoMLJobStatus = pAutoMLJobStatus_,
        _damljrsAutoMLJobSecondaryStatus = pAutoMLJobSecondaryStatus_
      }

-- | Returns the job's output from GenerateCandidateDefinitionsOnly.
damljrsGenerateCandidateDefinitionsOnly :: Lens' DescribeAutoMLJobResponse (Maybe Bool)
damljrsGenerateCandidateDefinitionsOnly = lens _damljrsGenerateCandidateDefinitionsOnly (\s a -> s {_damljrsGenerateCandidateDefinitionsOnly = a})

-- | Returns the job's FailureReason.
damljrsFailureReason :: Lens' DescribeAutoMLJobResponse (Maybe Text)
damljrsFailureReason = lens _damljrsFailureReason (\s a -> s {_damljrsFailureReason = a})

-- | Returns the job's problem type.
damljrsProblemType :: Lens' DescribeAutoMLJobResponse (Maybe ProblemType)
damljrsProblemType = lens _damljrsProblemType (\s a -> s {_damljrsProblemType = a})

-- | Returns the job's config.
damljrsAutoMLJobConfig :: Lens' DescribeAutoMLJobResponse (Maybe AutoMLJobConfig)
damljrsAutoMLJobConfig = lens _damljrsAutoMLJobConfig (\s a -> s {_damljrsAutoMLJobConfig = a})

-- | Returns the job's objective.
damljrsAutoMLJobObjective :: Lens' DescribeAutoMLJobResponse (Maybe AutoMLJobObjective)
damljrsAutoMLJobObjective = lens _damljrsAutoMLJobObjective (\s a -> s {_damljrsAutoMLJobObjective = a})

-- | Returns information on the job's artifacts found in AutoMLJobArtifacts.
damljrsAutoMLJobArtifacts :: Lens' DescribeAutoMLJobResponse (Maybe AutoMLJobArtifacts)
damljrsAutoMLJobArtifacts = lens _damljrsAutoMLJobArtifacts (\s a -> s {_damljrsAutoMLJobArtifacts = a})

-- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
damljrsResolvedAttributes :: Lens' DescribeAutoMLJobResponse (Maybe ResolvedAttributes)
damljrsResolvedAttributes = lens _damljrsResolvedAttributes (\s a -> s {_damljrsResolvedAttributes = a})

-- | Returns the job's end time.
damljrsEndTime :: Lens' DescribeAutoMLJobResponse (Maybe UTCTime)
damljrsEndTime = lens _damljrsEndTime (\s a -> s {_damljrsEndTime = a}) . mapping _Time

-- | Returns the job's BestCandidate.
damljrsBestCandidate :: Lens' DescribeAutoMLJobResponse (Maybe AutoMLCandidate)
damljrsBestCandidate = lens _damljrsBestCandidate (\s a -> s {_damljrsBestCandidate = a})

-- | -- | The response status code.
damljrsResponseStatus :: Lens' DescribeAutoMLJobResponse Int
damljrsResponseStatus = lens _damljrsResponseStatus (\s a -> s {_damljrsResponseStatus = a})

-- | Returns the name of a job.
damljrsAutoMLJobName :: Lens' DescribeAutoMLJobResponse Text
damljrsAutoMLJobName = lens _damljrsAutoMLJobName (\s a -> s {_damljrsAutoMLJobName = a})

-- | Returns the job's ARN.
damljrsAutoMLJobARN :: Lens' DescribeAutoMLJobResponse Text
damljrsAutoMLJobARN = lens _damljrsAutoMLJobARN (\s a -> s {_damljrsAutoMLJobARN = a})

-- | Returns the job's input data config.
damljrsInputDataConfig :: Lens' DescribeAutoMLJobResponse (NonEmpty AutoMLChannel)
damljrsInputDataConfig = lens _damljrsInputDataConfig (\s a -> s {_damljrsInputDataConfig = a}) . _List1

-- | Returns the job's output data config.
damljrsOutputDataConfig :: Lens' DescribeAutoMLJobResponse AutoMLOutputDataConfig
damljrsOutputDataConfig = lens _damljrsOutputDataConfig (\s a -> s {_damljrsOutputDataConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
damljrsRoleARN :: Lens' DescribeAutoMLJobResponse Text
damljrsRoleARN = lens _damljrsRoleARN (\s a -> s {_damljrsRoleARN = a})

-- | Returns the job's creation time.
damljrsCreationTime :: Lens' DescribeAutoMLJobResponse UTCTime
damljrsCreationTime = lens _damljrsCreationTime (\s a -> s {_damljrsCreationTime = a}) . _Time

-- | Returns the job's last modified time.
damljrsLastModifiedTime :: Lens' DescribeAutoMLJobResponse UTCTime
damljrsLastModifiedTime = lens _damljrsLastModifiedTime (\s a -> s {_damljrsLastModifiedTime = a}) . _Time

-- | Returns the job's AutoMLJobStatus.
damljrsAutoMLJobStatus :: Lens' DescribeAutoMLJobResponse AutoMLJobStatus
damljrsAutoMLJobStatus = lens _damljrsAutoMLJobStatus (\s a -> s {_damljrsAutoMLJobStatus = a})

-- | Returns the job's AutoMLJobSecondaryStatus.
damljrsAutoMLJobSecondaryStatus :: Lens' DescribeAutoMLJobResponse AutoMLJobSecondaryStatus
damljrsAutoMLJobSecondaryStatus = lens _damljrsAutoMLJobSecondaryStatus (\s a -> s {_damljrsAutoMLJobSecondaryStatus = a})

instance NFData DescribeAutoMLJobResponse
