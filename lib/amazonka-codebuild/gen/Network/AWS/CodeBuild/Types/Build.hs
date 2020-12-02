{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Build
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Build where

import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildPhase
import Network.AWS.CodeBuild.Types.DebugSession
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
import Network.AWS.CodeBuild.Types.LogsLocation
import Network.AWS.CodeBuild.Types.NetworkInterface
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a build.
--
--
--
-- /See:/ 'build' smart constructor.
data Build = Build'
  { _bPhases :: !(Maybe [BuildPhase]),
    _bBuildComplete :: !(Maybe Bool),
    _bSecondaryArtifacts :: !(Maybe [BuildArtifacts]),
    _bArn :: !(Maybe Text),
    _bExportedEnvironmentVariables ::
      !(Maybe [ExportedEnvironmentVariable]),
    _bBuildNumber :: !(Maybe Integer),
    _bStartTime :: !(Maybe POSIX),
    _bArtifacts :: !(Maybe BuildArtifacts),
    _bEnvironment :: !(Maybe ProjectEnvironment),
    _bInitiator :: !(Maybe Text),
    _bNetworkInterface :: !(Maybe NetworkInterface),
    _bSecondarySourceVersions :: !(Maybe [ProjectSourceVersion]),
    _bCurrentPhase :: !(Maybe Text),
    _bQueuedTimeoutInMinutes :: !(Maybe Int),
    _bCache :: !(Maybe ProjectCache),
    _bSecondarySources :: !(Maybe [ProjectSource]),
    _bDebugSession :: !(Maybe DebugSession),
    _bSourceVersion :: !(Maybe Text),
    _bBuildBatchARN :: !(Maybe Text),
    _bLogs :: !(Maybe LogsLocation),
    _bResolvedSourceVersion :: !(Maybe Text),
    _bVpcConfig :: !(Maybe VPCConfig),
    _bEndTime :: !(Maybe POSIX),
    _bProjectName :: !(Maybe Text),
    _bBuildStatus :: !(Maybe StatusType),
    _bSource :: !(Maybe ProjectSource),
    _bId :: !(Maybe Text),
    _bFileSystemLocations :: !(Maybe [ProjectFileSystemLocation]),
    _bReportARNs :: !(Maybe [Text]),
    _bEncryptionKey :: !(Maybe Text),
    _bServiceRole :: !(Maybe Text),
    _bTimeoutInMinutes :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bPhases' - Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
--
-- * 'bBuildComplete' - Whether the build is complete. True if complete; otherwise, false.
--
-- * 'bSecondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- * 'bArn' - The Amazon Resource Name (ARN) of the build.
--
-- * 'bExportedEnvironmentVariables' - A list of exported environment variables for this build.
--
-- * 'bBuildNumber' - The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
--
-- * 'bStartTime' - When the build process started, expressed in Unix time format.
--
-- * 'bArtifacts' - Information about the output artifacts for the build.
--
-- * 'bEnvironment' - Information about the build environment for this build.
--
-- * 'bInitiator' - The entity that started the build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example, @MyUserName@ ).     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
-- * 'bNetworkInterface' - Describes a network interface.
--
-- * 'bSecondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
-- * 'bCurrentPhase' - The current build phase.
--
-- * 'bQueuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
--
-- * 'bCache' - Information about the cache for the build.
--
-- * 'bSecondarySources' - An array of @ProjectSource@ objects.
--
-- * 'bDebugSession' - Contains information about the debug session for this build.
--
-- * 'bSourceVersion' - Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- * 'bBuildBatchARN' - The ARN of the batch build that this build is a member of, if applicable.
--
-- * 'bLogs' - Information about the build's logs in Amazon CloudWatch Logs.
--
-- * 'bResolvedSourceVersion' - An identifier for the version of this build's source code.      * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.      * For AWS CodePipeline, the source revision provided by AWS CodePipeline.      * For Amazon Simple Storage Service (Amazon S3), this does not apply.
--
-- * 'bVpcConfig' - If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
--
-- * 'bEndTime' - When the build process ended, expressed in Unix time format.
--
-- * 'bProjectName' - The name of the AWS CodeBuild project.
--
-- * 'bBuildStatus' - The current status of the build. Valid values include:     * @FAILED@ : The build failed.     * @FAULT@ : The build faulted.     * @IN_PROGRESS@ : The build is still in progress.     * @STOPPED@ : The build stopped.     * @SUCCEEDED@ : The build succeeded.     * @TIMED_OUT@ : The build timed out.
--
-- * 'bSource' - Information about the source code to be built.
--
-- * 'bId' - The unique ID for the build.
--
-- * 'bFileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- * 'bReportARNs' - An array of the ARNs associated with this build's reports.
--
-- * 'bEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- * 'bServiceRole' - The name of a service role used for this build.
--
-- * 'bTimeoutInMinutes' - How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
build ::
  Build
build =
  Build'
    { _bPhases = Nothing,
      _bBuildComplete = Nothing,
      _bSecondaryArtifacts = Nothing,
      _bArn = Nothing,
      _bExportedEnvironmentVariables = Nothing,
      _bBuildNumber = Nothing,
      _bStartTime = Nothing,
      _bArtifacts = Nothing,
      _bEnvironment = Nothing,
      _bInitiator = Nothing,
      _bNetworkInterface = Nothing,
      _bSecondarySourceVersions = Nothing,
      _bCurrentPhase = Nothing,
      _bQueuedTimeoutInMinutes = Nothing,
      _bCache = Nothing,
      _bSecondarySources = Nothing,
      _bDebugSession = Nothing,
      _bSourceVersion = Nothing,
      _bBuildBatchARN = Nothing,
      _bLogs = Nothing,
      _bResolvedSourceVersion = Nothing,
      _bVpcConfig = Nothing,
      _bEndTime = Nothing,
      _bProjectName = Nothing,
      _bBuildStatus = Nothing,
      _bSource = Nothing,
      _bId = Nothing,
      _bFileSystemLocations = Nothing,
      _bReportARNs = Nothing,
      _bEncryptionKey = Nothing,
      _bServiceRole = Nothing,
      _bTimeoutInMinutes = Nothing
    }

-- | Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
bPhases :: Lens' Build [BuildPhase]
bPhases = lens _bPhases (\s a -> s {_bPhases = a}) . _Default . _Coerce

-- | Whether the build is complete. True if complete; otherwise, false.
bBuildComplete :: Lens' Build (Maybe Bool)
bBuildComplete = lens _bBuildComplete (\s a -> s {_bBuildComplete = a})

-- | An array of @ProjectArtifacts@ objects.
bSecondaryArtifacts :: Lens' Build [BuildArtifacts]
bSecondaryArtifacts = lens _bSecondaryArtifacts (\s a -> s {_bSecondaryArtifacts = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the build.
bArn :: Lens' Build (Maybe Text)
bArn = lens _bArn (\s a -> s {_bArn = a})

-- | A list of exported environment variables for this build.
bExportedEnvironmentVariables :: Lens' Build [ExportedEnvironmentVariable]
bExportedEnvironmentVariables = lens _bExportedEnvironmentVariables (\s a -> s {_bExportedEnvironmentVariables = a}) . _Default . _Coerce

-- | The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
bBuildNumber :: Lens' Build (Maybe Integer)
bBuildNumber = lens _bBuildNumber (\s a -> s {_bBuildNumber = a})

-- | When the build process started, expressed in Unix time format.
bStartTime :: Lens' Build (Maybe UTCTime)
bStartTime = lens _bStartTime (\s a -> s {_bStartTime = a}) . mapping _Time

-- | Information about the output artifacts for the build.
bArtifacts :: Lens' Build (Maybe BuildArtifacts)
bArtifacts = lens _bArtifacts (\s a -> s {_bArtifacts = a})

-- | Information about the build environment for this build.
bEnvironment :: Lens' Build (Maybe ProjectEnvironment)
bEnvironment = lens _bEnvironment (\s a -> s {_bEnvironment = a})

-- | The entity that started the build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example, @MyUserName@ ).     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
bInitiator :: Lens' Build (Maybe Text)
bInitiator = lens _bInitiator (\s a -> s {_bInitiator = a})

-- | Describes a network interface.
bNetworkInterface :: Lens' Build (Maybe NetworkInterface)
bNetworkInterface = lens _bNetworkInterface (\s a -> s {_bNetworkInterface = a})

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
bSecondarySourceVersions :: Lens' Build [ProjectSourceVersion]
bSecondarySourceVersions = lens _bSecondarySourceVersions (\s a -> s {_bSecondarySourceVersions = a}) . _Default . _Coerce

-- | The current build phase.
bCurrentPhase :: Lens' Build (Maybe Text)
bCurrentPhase = lens _bCurrentPhase (\s a -> s {_bCurrentPhase = a})

-- | The number of minutes a build is allowed to be queued before it times out.
bQueuedTimeoutInMinutes :: Lens' Build (Maybe Int)
bQueuedTimeoutInMinutes = lens _bQueuedTimeoutInMinutes (\s a -> s {_bQueuedTimeoutInMinutes = a})

-- | Information about the cache for the build.
bCache :: Lens' Build (Maybe ProjectCache)
bCache = lens _bCache (\s a -> s {_bCache = a})

-- | An array of @ProjectSource@ objects.
bSecondarySources :: Lens' Build [ProjectSource]
bSecondarySources = lens _bSecondarySources (\s a -> s {_bSecondarySources = a}) . _Default . _Coerce

-- | Contains information about the debug session for this build.
bDebugSession :: Lens' Build (Maybe DebugSession)
bDebugSession = lens _bDebugSession (\s a -> s {_bDebugSession = a})

-- | Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
bSourceVersion :: Lens' Build (Maybe Text)
bSourceVersion = lens _bSourceVersion (\s a -> s {_bSourceVersion = a})

-- | The ARN of the batch build that this build is a member of, if applicable.
bBuildBatchARN :: Lens' Build (Maybe Text)
bBuildBatchARN = lens _bBuildBatchARN (\s a -> s {_bBuildBatchARN = a})

-- | Information about the build's logs in Amazon CloudWatch Logs.
bLogs :: Lens' Build (Maybe LogsLocation)
bLogs = lens _bLogs (\s a -> s {_bLogs = a})

-- | An identifier for the version of this build's source code.      * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.      * For AWS CodePipeline, the source revision provided by AWS CodePipeline.      * For Amazon Simple Storage Service (Amazon S3), this does not apply.
bResolvedSourceVersion :: Lens' Build (Maybe Text)
bResolvedSourceVersion = lens _bResolvedSourceVersion (\s a -> s {_bResolvedSourceVersion = a})

-- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
bVpcConfig :: Lens' Build (Maybe VPCConfig)
bVpcConfig = lens _bVpcConfig (\s a -> s {_bVpcConfig = a})

-- | When the build process ended, expressed in Unix time format.
bEndTime :: Lens' Build (Maybe UTCTime)
bEndTime = lens _bEndTime (\s a -> s {_bEndTime = a}) . mapping _Time

-- | The name of the AWS CodeBuild project.
bProjectName :: Lens' Build (Maybe Text)
bProjectName = lens _bProjectName (\s a -> s {_bProjectName = a})

-- | The current status of the build. Valid values include:     * @FAILED@ : The build failed.     * @FAULT@ : The build faulted.     * @IN_PROGRESS@ : The build is still in progress.     * @STOPPED@ : The build stopped.     * @SUCCEEDED@ : The build succeeded.     * @TIMED_OUT@ : The build timed out.
bBuildStatus :: Lens' Build (Maybe StatusType)
bBuildStatus = lens _bBuildStatus (\s a -> s {_bBuildStatus = a})

-- | Information about the source code to be built.
bSource :: Lens' Build (Maybe ProjectSource)
bSource = lens _bSource (\s a -> s {_bSource = a})

-- | The unique ID for the build.
bId :: Lens' Build (Maybe Text)
bId = lens _bId (\s a -> s {_bId = a})

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
bFileSystemLocations :: Lens' Build [ProjectFileSystemLocation]
bFileSystemLocations = lens _bFileSystemLocations (\s a -> s {_bFileSystemLocations = a}) . _Default . _Coerce

-- | An array of the ARNs associated with this build's reports.
bReportARNs :: Lens' Build [Text]
bReportARNs = lens _bReportARNs (\s a -> s {_bReportARNs = a}) . _Default . _Coerce

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
bEncryptionKey :: Lens' Build (Maybe Text)
bEncryptionKey = lens _bEncryptionKey (\s a -> s {_bEncryptionKey = a})

-- | The name of a service role used for this build.
bServiceRole :: Lens' Build (Maybe Text)
bServiceRole = lens _bServiceRole (\s a -> s {_bServiceRole = a})

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
bTimeoutInMinutes :: Lens' Build (Maybe Int)
bTimeoutInMinutes = lens _bTimeoutInMinutes (\s a -> s {_bTimeoutInMinutes = a})

instance FromJSON Build where
  parseJSON =
    withObject
      "Build"
      ( \x ->
          Build'
            <$> (x .:? "phases" .!= mempty)
            <*> (x .:? "buildComplete")
            <*> (x .:? "secondaryArtifacts" .!= mempty)
            <*> (x .:? "arn")
            <*> (x .:? "exportedEnvironmentVariables" .!= mempty)
            <*> (x .:? "buildNumber")
            <*> (x .:? "startTime")
            <*> (x .:? "artifacts")
            <*> (x .:? "environment")
            <*> (x .:? "initiator")
            <*> (x .:? "networkInterface")
            <*> (x .:? "secondarySourceVersions" .!= mempty)
            <*> (x .:? "currentPhase")
            <*> (x .:? "queuedTimeoutInMinutes")
            <*> (x .:? "cache")
            <*> (x .:? "secondarySources" .!= mempty)
            <*> (x .:? "debugSession")
            <*> (x .:? "sourceVersion")
            <*> (x .:? "buildBatchArn")
            <*> (x .:? "logs")
            <*> (x .:? "resolvedSourceVersion")
            <*> (x .:? "vpcConfig")
            <*> (x .:? "endTime")
            <*> (x .:? "projectName")
            <*> (x .:? "buildStatus")
            <*> (x .:? "source")
            <*> (x .:? "id")
            <*> (x .:? "fileSystemLocations" .!= mempty)
            <*> (x .:? "reportArns" .!= mempty)
            <*> (x .:? "encryptionKey")
            <*> (x .:? "serviceRole")
            <*> (x .:? "timeoutInMinutes")
      )

instance Hashable Build

instance NFData Build
