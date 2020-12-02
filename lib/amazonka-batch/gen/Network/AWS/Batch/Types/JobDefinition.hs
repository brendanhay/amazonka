{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDefinition where

import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch job definition.
--
--
--
-- /See:/ 'jobDefinition' smart constructor.
data JobDefinition = JobDefinition'
  { _jddStatus :: !(Maybe Text),
    _jddRetryStrategy :: !(Maybe RetryStrategy),
    _jddParameters :: !(Maybe (Map Text (Text))),
    _jddTimeout :: !(Maybe JobTimeout),
    _jddContainerProperties :: !(Maybe ContainerProperties),
    _jddNodeProperties :: !(Maybe NodeProperties),
    _jddTags :: !(Maybe (Map Text (Text))),
    _jddJobDefinitionName :: !Text,
    _jddJobDefinitionARN :: !Text,
    _jddRevision :: !Int,
    _jddType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jddStatus' - The status of the job definition.
--
-- * 'jddRetryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition.
--
-- * 'jddParameters' - Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
--
-- * 'jddTimeout' - The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
--
-- * 'jddContainerProperties' - An object with various properties specific to container-based jobs.
--
-- * 'jddNodeProperties' - An object with various properties specific to multi-node parallel jobs.
--
-- * 'jddTags' - The tags applied to the job definition.
--
-- * 'jddJobDefinitionName' - The name of the job definition.
--
-- * 'jddJobDefinitionARN' - The Amazon Resource Name (ARN) for the job definition.
--
-- * 'jddRevision' - The revision of the job definition.
--
-- * 'jddType' - The type of job definition.
jobDefinition ::
  -- | 'jddJobDefinitionName'
  Text ->
  -- | 'jddJobDefinitionARN'
  Text ->
  -- | 'jddRevision'
  Int ->
  -- | 'jddType'
  Text ->
  JobDefinition
jobDefinition
  pJobDefinitionName_
  pJobDefinitionARN_
  pRevision_
  pType_ =
    JobDefinition'
      { _jddStatus = Nothing,
        _jddRetryStrategy = Nothing,
        _jddParameters = Nothing,
        _jddTimeout = Nothing,
        _jddContainerProperties = Nothing,
        _jddNodeProperties = Nothing,
        _jddTags = Nothing,
        _jddJobDefinitionName = pJobDefinitionName_,
        _jddJobDefinitionARN = pJobDefinitionARN_,
        _jddRevision = pRevision_,
        _jddType = pType_
      }

-- | The status of the job definition.
jddStatus :: Lens' JobDefinition (Maybe Text)
jddStatus = lens _jddStatus (\s a -> s {_jddStatus = a})

-- | The retry strategy to use for failed jobs that are submitted with this job definition.
jddRetryStrategy :: Lens' JobDefinition (Maybe RetryStrategy)
jddRetryStrategy = lens _jddRetryStrategy (\s a -> s {_jddRetryStrategy = a})

-- | Default parameters or parameter substitution placeholders that are set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition. For more information about specifying parameters, see <https://docs.aws.amazon.com/batch/latest/userguide/job_definition_parameters.html Job Definition Parameters> in the /AWS Batch User Guide/ .
jddParameters :: Lens' JobDefinition (HashMap Text (Text))
jddParameters = lens _jddParameters (\s a -> s {_jddParameters = a}) . _Default . _Map

-- | The timeout configuration for jobs that are submitted with this job definition. You can specify a timeout duration after which AWS Batch terminates your jobs if they have not finished.
jddTimeout :: Lens' JobDefinition (Maybe JobTimeout)
jddTimeout = lens _jddTimeout (\s a -> s {_jddTimeout = a})

-- | An object with various properties specific to container-based jobs.
jddContainerProperties :: Lens' JobDefinition (Maybe ContainerProperties)
jddContainerProperties = lens _jddContainerProperties (\s a -> s {_jddContainerProperties = a})

-- | An object with various properties specific to multi-node parallel jobs.
jddNodeProperties :: Lens' JobDefinition (Maybe NodeProperties)
jddNodeProperties = lens _jddNodeProperties (\s a -> s {_jddNodeProperties = a})

-- | The tags applied to the job definition.
jddTags :: Lens' JobDefinition (HashMap Text (Text))
jddTags = lens _jddTags (\s a -> s {_jddTags = a}) . _Default . _Map

-- | The name of the job definition.
jddJobDefinitionName :: Lens' JobDefinition Text
jddJobDefinitionName = lens _jddJobDefinitionName (\s a -> s {_jddJobDefinitionName = a})

-- | The Amazon Resource Name (ARN) for the job definition.
jddJobDefinitionARN :: Lens' JobDefinition Text
jddJobDefinitionARN = lens _jddJobDefinitionARN (\s a -> s {_jddJobDefinitionARN = a})

-- | The revision of the job definition.
jddRevision :: Lens' JobDefinition Int
jddRevision = lens _jddRevision (\s a -> s {_jddRevision = a})

-- | The type of job definition.
jddType :: Lens' JobDefinition Text
jddType = lens _jddType (\s a -> s {_jddType = a})

instance FromJSON JobDefinition where
  parseJSON =
    withObject
      "JobDefinition"
      ( \x ->
          JobDefinition'
            <$> (x .:? "status")
            <*> (x .:? "retryStrategy")
            <*> (x .:? "parameters" .!= mempty)
            <*> (x .:? "timeout")
            <*> (x .:? "containerProperties")
            <*> (x .:? "nodeProperties")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "jobDefinitionName")
            <*> (x .: "jobDefinitionArn")
            <*> (x .: "revision")
            <*> (x .: "type")
      )

instance Hashable JobDefinition

instance NFData JobDefinition
