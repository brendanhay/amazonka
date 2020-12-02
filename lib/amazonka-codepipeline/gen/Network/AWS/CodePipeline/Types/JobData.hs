{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobData where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents other information about a job required for a job worker to complete the job.
--
--
--
-- /See:/ 'jobData' smart constructor.
data JobData = JobData'
  { _jdContinuationToken :: !(Maybe Text),
    _jdOutputArtifacts :: !(Maybe [Artifact]),
    _jdArtifactCredentials ::
      !(Maybe (Sensitive AWSSessionCredentials)),
    _jdPipelineContext :: !(Maybe PipelineContext),
    _jdEncryptionKey :: !(Maybe EncryptionKey),
    _jdActionTypeId :: !(Maybe ActionTypeId),
    _jdInputArtifacts :: !(Maybe [Artifact]),
    _jdActionConfiguration :: !(Maybe ActionConfiguration)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdContinuationToken' - A system-generated token, such as a AWS CodeDeploy deployment ID, required by a job to continue the job asynchronously.
--
-- * 'jdOutputArtifacts' - The output of the job.
--
-- * 'jdArtifactCredentials' - Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifacts for the pipeline in AWS CodePipeline.
--
-- * 'jdPipelineContext' - Represents information about a pipeline to a job worker.
--
-- * 'jdEncryptionKey' - Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- * 'jdActionTypeId' - Represents information about an action type.
--
-- * 'jdInputArtifacts' - The artifact supplied to the job.
--
-- * 'jdActionConfiguration' - Represents information about an action configuration.
jobData ::
  JobData
jobData =
  JobData'
    { _jdContinuationToken = Nothing,
      _jdOutputArtifacts = Nothing,
      _jdArtifactCredentials = Nothing,
      _jdPipelineContext = Nothing,
      _jdEncryptionKey = Nothing,
      _jdActionTypeId = Nothing,
      _jdInputArtifacts = Nothing,
      _jdActionConfiguration = Nothing
    }

-- | A system-generated token, such as a AWS CodeDeploy deployment ID, required by a job to continue the job asynchronously.
jdContinuationToken :: Lens' JobData (Maybe Text)
jdContinuationToken = lens _jdContinuationToken (\s a -> s {_jdContinuationToken = a})

-- | The output of the job.
jdOutputArtifacts :: Lens' JobData [Artifact]
jdOutputArtifacts = lens _jdOutputArtifacts (\s a -> s {_jdOutputArtifacts = a}) . _Default . _Coerce

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifacts for the pipeline in AWS CodePipeline.
jdArtifactCredentials :: Lens' JobData (Maybe AWSSessionCredentials)
jdArtifactCredentials = lens _jdArtifactCredentials (\s a -> s {_jdArtifactCredentials = a}) . mapping _Sensitive

-- | Represents information about a pipeline to a job worker.
jdPipelineContext :: Lens' JobData (Maybe PipelineContext)
jdPipelineContext = lens _jdPipelineContext (\s a -> s {_jdPipelineContext = a})

-- | Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
jdEncryptionKey :: Lens' JobData (Maybe EncryptionKey)
jdEncryptionKey = lens _jdEncryptionKey (\s a -> s {_jdEncryptionKey = a})

-- | Represents information about an action type.
jdActionTypeId :: Lens' JobData (Maybe ActionTypeId)
jdActionTypeId = lens _jdActionTypeId (\s a -> s {_jdActionTypeId = a})

-- | The artifact supplied to the job.
jdInputArtifacts :: Lens' JobData [Artifact]
jdInputArtifacts = lens _jdInputArtifacts (\s a -> s {_jdInputArtifacts = a}) . _Default . _Coerce

-- | Represents information about an action configuration.
jdActionConfiguration :: Lens' JobData (Maybe ActionConfiguration)
jdActionConfiguration = lens _jdActionConfiguration (\s a -> s {_jdActionConfiguration = a})

instance FromJSON JobData where
  parseJSON =
    withObject
      "JobData"
      ( \x ->
          JobData'
            <$> (x .:? "continuationToken")
            <*> (x .:? "outputArtifacts" .!= mempty)
            <*> (x .:? "artifactCredentials")
            <*> (x .:? "pipelineContext")
            <*> (x .:? "encryptionKey")
            <*> (x .:? "actionTypeId")
            <*> (x .:? "inputArtifacts" .!= mempty)
            <*> (x .:? "actionConfiguration")
      )

instance Hashable JobData

instance NFData JobData
