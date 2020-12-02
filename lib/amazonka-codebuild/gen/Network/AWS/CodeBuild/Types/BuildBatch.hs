{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatch where

import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildBatchPhase
import Network.AWS.CodeBuild.Types.BuildGroup
import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a batch build.
--
--
--
-- /See:/ 'buildBatch' smart constructor.
data BuildBatch = BuildBatch'
  { _bbPhases ::
      !(Maybe [BuildBatchPhase]),
    _bbSecondaryArtifacts :: !(Maybe [BuildArtifacts]),
    _bbBuildTimeoutInMinutes :: !(Maybe Int),
    _bbArn :: !(Maybe Text),
    _bbStartTime :: !(Maybe POSIX),
    _bbArtifacts :: !(Maybe BuildArtifacts),
    _bbEnvironment :: !(Maybe ProjectEnvironment),
    _bbInitiator :: !(Maybe Text),
    _bbSecondarySourceVersions :: !(Maybe [ProjectSourceVersion]),
    _bbBuildBatchStatus :: !(Maybe StatusType),
    _bbCurrentPhase :: !(Maybe Text),
    _bbBuildBatchNumber :: !(Maybe Integer),
    _bbQueuedTimeoutInMinutes :: !(Maybe Int),
    _bbCache :: !(Maybe ProjectCache),
    _bbSecondarySources :: !(Maybe [ProjectSource]),
    _bbSourceVersion :: !(Maybe Text),
    _bbResolvedSourceVersion :: !(Maybe Text),
    _bbVpcConfig :: !(Maybe VPCConfig),
    _bbEndTime :: !(Maybe POSIX),
    _bbProjectName :: !(Maybe Text),
    _bbBuildGroups :: !(Maybe [BuildGroup]),
    _bbSource :: !(Maybe ProjectSource),
    _bbId :: !(Maybe Text),
    _bbFileSystemLocations :: !(Maybe [ProjectFileSystemLocation]),
    _bbBuildBatchConfig :: !(Maybe ProjectBuildBatchConfig),
    _bbEncryptionKey :: !(Maybe Text),
    _bbLogConfig :: !(Maybe LogsConfig),
    _bbServiceRole :: !(Maybe Text),
    _bbComplete :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bbPhases' - An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
--
-- * 'bbSecondaryArtifacts' - An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
--
-- * 'bbBuildTimeoutInMinutes' - Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
--
-- * 'bbArn' - The ARN of the batch build.
--
-- * 'bbStartTime' - The date and time that the batch build started.
--
-- * 'bbArtifacts' - A @BuildArtifacts@ object the defines the build artifacts for this batch build.
--
-- * 'bbEnvironment' - Undocumented member.
--
-- * 'bbInitiator' - The entity that started the batch build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name.     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
-- * 'bbSecondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
-- * 'bbBuildBatchStatus' - The status of the batch build.
--
-- * 'bbCurrentPhase' - The current phase of the batch build.
--
-- * 'bbBuildBatchNumber' - The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
--
-- * 'bbQueuedTimeoutInMinutes' - Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
--
-- * 'bbCache' - Undocumented member.
--
-- * 'bbSecondarySources' - An array of @ProjectSource@ objects that define the sources for the batch build.
--
-- * 'bbSourceVersion' - The identifier of the version of the source code to be built.
--
-- * 'bbResolvedSourceVersion' - The identifier of the resolved version of this batch build's source code.     * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.     * For AWS CodePipeline, the source revision provided by AWS CodePipeline.     * For Amazon Simple Storage Service (Amazon S3), this does not apply.
--
-- * 'bbVpcConfig' - Undocumented member.
--
-- * 'bbEndTime' - The date and time that the batch build ended.
--
-- * 'bbProjectName' - The name of the batch build project.
--
-- * 'bbBuildGroups' - An array of @BuildGroup@ objects that define the build groups for the batch build.
--
-- * 'bbSource' - Undocumented member.
--
-- * 'bbId' - The identifier of the batch build.
--
-- * 'bbFileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- * 'bbBuildBatchConfig' - Undocumented member.
--
-- * 'bbEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- * 'bbLogConfig' - Undocumented member.
--
-- * 'bbServiceRole' - The name of a service role used for builds in the batch.
--
-- * 'bbComplete' - Indicates if the batch build is complete.
buildBatch ::
  BuildBatch
buildBatch =
  BuildBatch'
    { _bbPhases = Nothing,
      _bbSecondaryArtifacts = Nothing,
      _bbBuildTimeoutInMinutes = Nothing,
      _bbArn = Nothing,
      _bbStartTime = Nothing,
      _bbArtifacts = Nothing,
      _bbEnvironment = Nothing,
      _bbInitiator = Nothing,
      _bbSecondarySourceVersions = Nothing,
      _bbBuildBatchStatus = Nothing,
      _bbCurrentPhase = Nothing,
      _bbBuildBatchNumber = Nothing,
      _bbQueuedTimeoutInMinutes = Nothing,
      _bbCache = Nothing,
      _bbSecondarySources = Nothing,
      _bbSourceVersion = Nothing,
      _bbResolvedSourceVersion = Nothing,
      _bbVpcConfig = Nothing,
      _bbEndTime = Nothing,
      _bbProjectName = Nothing,
      _bbBuildGroups = Nothing,
      _bbSource = Nothing,
      _bbId = Nothing,
      _bbFileSystemLocations = Nothing,
      _bbBuildBatchConfig = Nothing,
      _bbEncryptionKey = Nothing,
      _bbLogConfig = Nothing,
      _bbServiceRole = Nothing,
      _bbComplete = Nothing
    }

-- | An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
bbPhases :: Lens' BuildBatch [BuildBatchPhase]
bbPhases = lens _bbPhases (\s a -> s {_bbPhases = a}) . _Default . _Coerce

-- | An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
bbSecondaryArtifacts :: Lens' BuildBatch [BuildArtifacts]
bbSecondaryArtifacts = lens _bbSecondaryArtifacts (\s a -> s {_bbSecondaryArtifacts = a}) . _Default . _Coerce

-- | Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
bbBuildTimeoutInMinutes :: Lens' BuildBatch (Maybe Int)
bbBuildTimeoutInMinutes = lens _bbBuildTimeoutInMinutes (\s a -> s {_bbBuildTimeoutInMinutes = a})

-- | The ARN of the batch build.
bbArn :: Lens' BuildBatch (Maybe Text)
bbArn = lens _bbArn (\s a -> s {_bbArn = a})

-- | The date and time that the batch build started.
bbStartTime :: Lens' BuildBatch (Maybe UTCTime)
bbStartTime = lens _bbStartTime (\s a -> s {_bbStartTime = a}) . mapping _Time

-- | A @BuildArtifacts@ object the defines the build artifacts for this batch build.
bbArtifacts :: Lens' BuildBatch (Maybe BuildArtifacts)
bbArtifacts = lens _bbArtifacts (\s a -> s {_bbArtifacts = a})

-- | Undocumented member.
bbEnvironment :: Lens' BuildBatch (Maybe ProjectEnvironment)
bbEnvironment = lens _bbEnvironment (\s a -> s {_bbEnvironment = a})

-- | The entity that started the batch build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name.     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
bbInitiator :: Lens' BuildBatch (Maybe Text)
bbInitiator = lens _bbInitiator (\s a -> s {_bbInitiator = a})

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
bbSecondarySourceVersions :: Lens' BuildBatch [ProjectSourceVersion]
bbSecondarySourceVersions = lens _bbSecondarySourceVersions (\s a -> s {_bbSecondarySourceVersions = a}) . _Default . _Coerce

-- | The status of the batch build.
bbBuildBatchStatus :: Lens' BuildBatch (Maybe StatusType)
bbBuildBatchStatus = lens _bbBuildBatchStatus (\s a -> s {_bbBuildBatchStatus = a})

-- | The current phase of the batch build.
bbCurrentPhase :: Lens' BuildBatch (Maybe Text)
bbCurrentPhase = lens _bbCurrentPhase (\s a -> s {_bbCurrentPhase = a})

-- | The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
bbBuildBatchNumber :: Lens' BuildBatch (Maybe Integer)
bbBuildBatchNumber = lens _bbBuildBatchNumber (\s a -> s {_bbBuildBatchNumber = a})

-- | Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
bbQueuedTimeoutInMinutes :: Lens' BuildBatch (Maybe Int)
bbQueuedTimeoutInMinutes = lens _bbQueuedTimeoutInMinutes (\s a -> s {_bbQueuedTimeoutInMinutes = a})

-- | Undocumented member.
bbCache :: Lens' BuildBatch (Maybe ProjectCache)
bbCache = lens _bbCache (\s a -> s {_bbCache = a})

-- | An array of @ProjectSource@ objects that define the sources for the batch build.
bbSecondarySources :: Lens' BuildBatch [ProjectSource]
bbSecondarySources = lens _bbSecondarySources (\s a -> s {_bbSecondarySources = a}) . _Default . _Coerce

-- | The identifier of the version of the source code to be built.
bbSourceVersion :: Lens' BuildBatch (Maybe Text)
bbSourceVersion = lens _bbSourceVersion (\s a -> s {_bbSourceVersion = a})

-- | The identifier of the resolved version of this batch build's source code.     * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.     * For AWS CodePipeline, the source revision provided by AWS CodePipeline.     * For Amazon Simple Storage Service (Amazon S3), this does not apply.
bbResolvedSourceVersion :: Lens' BuildBatch (Maybe Text)
bbResolvedSourceVersion = lens _bbResolvedSourceVersion (\s a -> s {_bbResolvedSourceVersion = a})

-- | Undocumented member.
bbVpcConfig :: Lens' BuildBatch (Maybe VPCConfig)
bbVpcConfig = lens _bbVpcConfig (\s a -> s {_bbVpcConfig = a})

-- | The date and time that the batch build ended.
bbEndTime :: Lens' BuildBatch (Maybe UTCTime)
bbEndTime = lens _bbEndTime (\s a -> s {_bbEndTime = a}) . mapping _Time

-- | The name of the batch build project.
bbProjectName :: Lens' BuildBatch (Maybe Text)
bbProjectName = lens _bbProjectName (\s a -> s {_bbProjectName = a})

-- | An array of @BuildGroup@ objects that define the build groups for the batch build.
bbBuildGroups :: Lens' BuildBatch [BuildGroup]
bbBuildGroups = lens _bbBuildGroups (\s a -> s {_bbBuildGroups = a}) . _Default . _Coerce

-- | Undocumented member.
bbSource :: Lens' BuildBatch (Maybe ProjectSource)
bbSource = lens _bbSource (\s a -> s {_bbSource = a})

-- | The identifier of the batch build.
bbId :: Lens' BuildBatch (Maybe Text)
bbId = lens _bbId (\s a -> s {_bbId = a})

-- | An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
bbFileSystemLocations :: Lens' BuildBatch [ProjectFileSystemLocation]
bbFileSystemLocations = lens _bbFileSystemLocations (\s a -> s {_bbFileSystemLocations = a}) . _Default . _Coerce

-- | Undocumented member.
bbBuildBatchConfig :: Lens' BuildBatch (Maybe ProjectBuildBatchConfig)
bbBuildBatchConfig = lens _bbBuildBatchConfig (\s a -> s {_bbBuildBatchConfig = a})

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
bbEncryptionKey :: Lens' BuildBatch (Maybe Text)
bbEncryptionKey = lens _bbEncryptionKey (\s a -> s {_bbEncryptionKey = a})

-- | Undocumented member.
bbLogConfig :: Lens' BuildBatch (Maybe LogsConfig)
bbLogConfig = lens _bbLogConfig (\s a -> s {_bbLogConfig = a})

-- | The name of a service role used for builds in the batch.
bbServiceRole :: Lens' BuildBatch (Maybe Text)
bbServiceRole = lens _bbServiceRole (\s a -> s {_bbServiceRole = a})

-- | Indicates if the batch build is complete.
bbComplete :: Lens' BuildBatch (Maybe Bool)
bbComplete = lens _bbComplete (\s a -> s {_bbComplete = a})

instance FromJSON BuildBatch where
  parseJSON =
    withObject
      "BuildBatch"
      ( \x ->
          BuildBatch'
            <$> (x .:? "phases" .!= mempty)
            <*> (x .:? "secondaryArtifacts" .!= mempty)
            <*> (x .:? "buildTimeoutInMinutes")
            <*> (x .:? "arn")
            <*> (x .:? "startTime")
            <*> (x .:? "artifacts")
            <*> (x .:? "environment")
            <*> (x .:? "initiator")
            <*> (x .:? "secondarySourceVersions" .!= mempty)
            <*> (x .:? "buildBatchStatus")
            <*> (x .:? "currentPhase")
            <*> (x .:? "buildBatchNumber")
            <*> (x .:? "queuedTimeoutInMinutes")
            <*> (x .:? "cache")
            <*> (x .:? "secondarySources" .!= mempty)
            <*> (x .:? "sourceVersion")
            <*> (x .:? "resolvedSourceVersion")
            <*> (x .:? "vpcConfig")
            <*> (x .:? "endTime")
            <*> (x .:? "projectName")
            <*> (x .:? "buildGroups" .!= mempty)
            <*> (x .:? "source")
            <*> (x .:? "id")
            <*> (x .:? "fileSystemLocations" .!= mempty)
            <*> (x .:? "buildBatchConfig")
            <*> (x .:? "encryptionKey")
            <*> (x .:? "logConfig")
            <*> (x .:? "serviceRole")
            <*> (x .:? "complete")
      )

instance Hashable BuildBatch

instance NFData BuildBatch
