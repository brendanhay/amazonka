{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Project
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Project where

import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.ProjectArtifacts
import Network.AWS.CodeBuild.Types.ProjectBadge
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a build project.
--
--
--
-- /See:/ 'project' smart constructor.
data Project = Project'
  { _pSecondaryArtifacts ::
      !(Maybe [ProjectArtifacts]),
    _pArn :: !(Maybe Text),
    _pArtifacts :: !(Maybe ProjectArtifacts),
    _pEnvironment :: !(Maybe ProjectEnvironment),
    _pCreated :: !(Maybe POSIX),
    _pSecondarySourceVersions :: !(Maybe [ProjectSourceVersion]),
    _pQueuedTimeoutInMinutes :: !(Maybe Nat),
    _pCache :: !(Maybe ProjectCache),
    _pSecondarySources :: !(Maybe [ProjectSource]),
    _pSourceVersion :: !(Maybe Text),
    _pName :: !(Maybe Text),
    _pVpcConfig :: !(Maybe VPCConfig),
    _pSource :: !(Maybe ProjectSource),
    _pBadge :: !(Maybe ProjectBadge),
    _pLogsConfig :: !(Maybe LogsConfig),
    _pFileSystemLocations :: !(Maybe [ProjectFileSystemLocation]),
    _pBuildBatchConfig :: !(Maybe ProjectBuildBatchConfig),
    _pEncryptionKey :: !(Maybe Text),
    _pLastModified :: !(Maybe POSIX),
    _pWebhook :: !(Maybe Webhook),
    _pDescription :: !(Maybe Text),
    _pServiceRole :: !(Maybe Text),
    _pTags :: !(Maybe [Tag]),
    _pTimeoutInMinutes :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pSecondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- * 'pArn' - The Amazon Resource Name (ARN) of the build project.
--
-- * 'pArtifacts' - Information about the build output artifacts for the build project.
--
-- * 'pEnvironment' - Information about the build environment for this build project.
--
-- * 'pCreated' - When the build project was created, expressed in Unix time format.
--
-- * 'pSecondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- * 'pQueuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
--
-- * 'pCache' - Information about the cache for the build project.
--
-- * 'pSecondarySources' - An array of @ProjectSource@ objects.
--
-- * 'pSourceVersion' - A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- * 'pName' - The name of the build project.
--
-- * 'pVpcConfig' - Information about the VPC configuration that AWS CodeBuild accesses.
--
-- * 'pSource' - Information about the build input source code for this build project.
--
-- * 'pBadge' - Information about the build badge for the build project.
--
-- * 'pLogsConfig' - Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
--
-- * 'pFileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- * 'pBuildBatchConfig' - A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
--
-- * 'pEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- * 'pLastModified' - When the build project's settings were last modified, expressed in Unix time format.
--
-- * 'pWebhook' - Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- * 'pDescription' - A description that makes the build project easy to identify.
--
-- * 'pServiceRole' - The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'pTags' - A list of tag key and value pairs associated with this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- * 'pTimeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
project ::
  Project
project =
  Project'
    { _pSecondaryArtifacts = Nothing,
      _pArn = Nothing,
      _pArtifacts = Nothing,
      _pEnvironment = Nothing,
      _pCreated = Nothing,
      _pSecondarySourceVersions = Nothing,
      _pQueuedTimeoutInMinutes = Nothing,
      _pCache = Nothing,
      _pSecondarySources = Nothing,
      _pSourceVersion = Nothing,
      _pName = Nothing,
      _pVpcConfig = Nothing,
      _pSource = Nothing,
      _pBadge = Nothing,
      _pLogsConfig = Nothing,
      _pFileSystemLocations = Nothing,
      _pBuildBatchConfig = Nothing,
      _pEncryptionKey = Nothing,
      _pLastModified = Nothing,
      _pWebhook = Nothing,
      _pDescription = Nothing,
      _pServiceRole = Nothing,
      _pTags = Nothing,
      _pTimeoutInMinutes = Nothing
    }

-- | An array of @ProjectArtifacts@ objects.
pSecondaryArtifacts :: Lens' Project [ProjectArtifacts]
pSecondaryArtifacts = lens _pSecondaryArtifacts (\s a -> s {_pSecondaryArtifacts = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the build project.
pArn :: Lens' Project (Maybe Text)
pArn = lens _pArn (\s a -> s {_pArn = a})

-- | Information about the build output artifacts for the build project.
pArtifacts :: Lens' Project (Maybe ProjectArtifacts)
pArtifacts = lens _pArtifacts (\s a -> s {_pArtifacts = a})

-- | Information about the build environment for this build project.
pEnvironment :: Lens' Project (Maybe ProjectEnvironment)
pEnvironment = lens _pEnvironment (\s a -> s {_pEnvironment = a})

-- | When the build project was created, expressed in Unix time format.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\s a -> s {_pCreated = a}) . mapping _Time

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
pSecondarySourceVersions :: Lens' Project [ProjectSourceVersion]
pSecondarySourceVersions = lens _pSecondarySourceVersions (\s a -> s {_pSecondarySourceVersions = a}) . _Default . _Coerce

-- | The number of minutes a build is allowed to be queued before it times out.
pQueuedTimeoutInMinutes :: Lens' Project (Maybe Natural)
pQueuedTimeoutInMinutes = lens _pQueuedTimeoutInMinutes (\s a -> s {_pQueuedTimeoutInMinutes = a}) . mapping _Nat

-- | Information about the cache for the build project.
pCache :: Lens' Project (Maybe ProjectCache)
pCache = lens _pCache (\s a -> s {_pCache = a})

-- | An array of @ProjectSource@ objects.
pSecondarySources :: Lens' Project [ProjectSource]
pSecondarySources = lens _pSecondarySources (\s a -> s {_pSecondarySources = a}) . _Default . _Coerce

-- | A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
pSourceVersion :: Lens' Project (Maybe Text)
pSourceVersion = lens _pSourceVersion (\s a -> s {_pSourceVersion = a})

-- | The name of the build project.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

-- | Information about the VPC configuration that AWS CodeBuild accesses.
pVpcConfig :: Lens' Project (Maybe VPCConfig)
pVpcConfig = lens _pVpcConfig (\s a -> s {_pVpcConfig = a})

-- | Information about the build input source code for this build project.
pSource :: Lens' Project (Maybe ProjectSource)
pSource = lens _pSource (\s a -> s {_pSource = a})

-- | Information about the build badge for the build project.
pBadge :: Lens' Project (Maybe ProjectBadge)
pBadge = lens _pBadge (\s a -> s {_pBadge = a})

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
pLogsConfig :: Lens' Project (Maybe LogsConfig)
pLogsConfig = lens _pLogsConfig (\s a -> s {_pLogsConfig = a})

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
pFileSystemLocations :: Lens' Project [ProjectFileSystemLocation]
pFileSystemLocations = lens _pFileSystemLocations (\s a -> s {_pFileSystemLocations = a}) . _Default . _Coerce

-- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
pBuildBatchConfig :: Lens' Project (Maybe ProjectBuildBatchConfig)
pBuildBatchConfig = lens _pBuildBatchConfig (\s a -> s {_pBuildBatchConfig = a})

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
pEncryptionKey :: Lens' Project (Maybe Text)
pEncryptionKey = lens _pEncryptionKey (\s a -> s {_pEncryptionKey = a})

-- | When the build project's settings were last modified, expressed in Unix time format.
pLastModified :: Lens' Project (Maybe UTCTime)
pLastModified = lens _pLastModified (\s a -> s {_pLastModified = a}) . mapping _Time

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
pWebhook :: Lens' Project (Maybe Webhook)
pWebhook = lens _pWebhook (\s a -> s {_pWebhook = a})

-- | A description that makes the build project easy to identify.
pDescription :: Lens' Project (Maybe Text)
pDescription = lens _pDescription (\s a -> s {_pDescription = a})

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
pServiceRole :: Lens' Project (Maybe Text)
pServiceRole = lens _pServiceRole (\s a -> s {_pServiceRole = a})

-- | A list of tag key and value pairs associated with this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
pTags :: Lens' Project [Tag]
pTags = lens _pTags (\s a -> s {_pTags = a}) . _Default . _Coerce

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
pTimeoutInMinutes :: Lens' Project (Maybe Natural)
pTimeoutInMinutes = lens _pTimeoutInMinutes (\s a -> s {_pTimeoutInMinutes = a}) . mapping _Nat

instance FromJSON Project where
  parseJSON =
    withObject
      "Project"
      ( \x ->
          Project'
            <$> (x .:? "secondaryArtifacts" .!= mempty)
            <*> (x .:? "arn")
            <*> (x .:? "artifacts")
            <*> (x .:? "environment")
            <*> (x .:? "created")
            <*> (x .:? "secondarySourceVersions" .!= mempty)
            <*> (x .:? "queuedTimeoutInMinutes")
            <*> (x .:? "cache")
            <*> (x .:? "secondarySources" .!= mempty)
            <*> (x .:? "sourceVersion")
            <*> (x .:? "name")
            <*> (x .:? "vpcConfig")
            <*> (x .:? "source")
            <*> (x .:? "badge")
            <*> (x .:? "logsConfig")
            <*> (x .:? "fileSystemLocations" .!= mempty)
            <*> (x .:? "buildBatchConfig")
            <*> (x .:? "encryptionKey")
            <*> (x .:? "lastModified")
            <*> (x .:? "webhook")
            <*> (x .:? "description")
            <*> (x .:? "serviceRole")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "timeoutInMinutes")
      )

instance Hashable Project

instance NFData Project
