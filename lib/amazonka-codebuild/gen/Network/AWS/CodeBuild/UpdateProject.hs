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
-- Module      : Network.AWS.CodeBuild.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of a build project.
module Network.AWS.CodeBuild.UpdateProject
  ( -- * Creating a Request
    updateProject,
    UpdateProject,

    -- * Request Lenses
    upSecondaryArtifacts,
    upArtifacts,
    upEnvironment,
    upBadgeEnabled,
    upSecondarySourceVersions,
    upQueuedTimeoutInMinutes,
    upCache,
    upSecondarySources,
    upSourceVersion,
    upVpcConfig,
    upSource,
    upLogsConfig,
    upFileSystemLocations,
    upBuildBatchConfig,
    upEncryptionKey,
    upDescription,
    upServiceRole,
    upTags,
    upTimeoutInMinutes,
    upName,

    -- * Destructuring the Response
    updateProjectResponse,
    UpdateProjectResponse,

    -- * Response Lenses
    uprsProject,
    uprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateProject' smart constructor.
data UpdateProject = UpdateProject'
  { _upSecondaryArtifacts ::
      !(Maybe [ProjectArtifacts]),
    _upArtifacts :: !(Maybe ProjectArtifacts),
    _upEnvironment :: !(Maybe ProjectEnvironment),
    _upBadgeEnabled :: !(Maybe Bool),
    _upSecondarySourceVersions :: !(Maybe [ProjectSourceVersion]),
    _upQueuedTimeoutInMinutes :: !(Maybe Nat),
    _upCache :: !(Maybe ProjectCache),
    _upSecondarySources :: !(Maybe [ProjectSource]),
    _upSourceVersion :: !(Maybe Text),
    _upVpcConfig :: !(Maybe VPCConfig),
    _upSource :: !(Maybe ProjectSource),
    _upLogsConfig :: !(Maybe LogsConfig),
    _upFileSystemLocations :: !(Maybe [ProjectFileSystemLocation]),
    _upBuildBatchConfig :: !(Maybe ProjectBuildBatchConfig),
    _upEncryptionKey :: !(Maybe Text),
    _upDescription :: !(Maybe Text),
    _upServiceRole :: !(Maybe Text),
    _upTags :: !(Maybe [Tag]),
    _upTimeoutInMinutes :: !(Maybe Nat),
    _upName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upSecondaryArtifacts' - An array of @ProjectSource@ objects.
--
-- * 'upArtifacts' - Information to be changed about the build output artifacts for the build project.
--
-- * 'upEnvironment' - Information to be changed about the build environment for the build project.
--
-- * 'upBadgeEnabled' - Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- * 'upSecondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- * 'upQueuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
--
-- * 'upCache' - Stores recently used information so that it can be quickly accessed at a later time.
--
-- * 'upSecondarySources' - An array of @ProjectSource@ objects.
--
-- * 'upSourceVersion' - A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- * 'upVpcConfig' - VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- * 'upSource' - Information to be changed about the build input source code for the build project.
--
-- * 'upLogsConfig' - Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
--
-- * 'upFileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- * 'upBuildBatchConfig' - Undocumented member.
--
-- * 'upEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- * 'upDescription' - A new or replacement description of the build project.
--
-- * 'upServiceRole' - The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'upTags' - An updated list of tag key and value pairs associated with this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- * 'upTimeoutInMinutes' - The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
--
-- * 'upName' - The name of the build project.
updateProject ::
  -- | 'upName'
  Text ->
  UpdateProject
updateProject pName_ =
  UpdateProject'
    { _upSecondaryArtifacts = Nothing,
      _upArtifacts = Nothing,
      _upEnvironment = Nothing,
      _upBadgeEnabled = Nothing,
      _upSecondarySourceVersions = Nothing,
      _upQueuedTimeoutInMinutes = Nothing,
      _upCache = Nothing,
      _upSecondarySources = Nothing,
      _upSourceVersion = Nothing,
      _upVpcConfig = Nothing,
      _upSource = Nothing,
      _upLogsConfig = Nothing,
      _upFileSystemLocations = Nothing,
      _upBuildBatchConfig = Nothing,
      _upEncryptionKey = Nothing,
      _upDescription = Nothing,
      _upServiceRole = Nothing,
      _upTags = Nothing,
      _upTimeoutInMinutes = Nothing,
      _upName = pName_
    }

-- | An array of @ProjectSource@ objects.
upSecondaryArtifacts :: Lens' UpdateProject [ProjectArtifacts]
upSecondaryArtifacts = lens _upSecondaryArtifacts (\s a -> s {_upSecondaryArtifacts = a}) . _Default . _Coerce

-- | Information to be changed about the build output artifacts for the build project.
upArtifacts :: Lens' UpdateProject (Maybe ProjectArtifacts)
upArtifacts = lens _upArtifacts (\s a -> s {_upArtifacts = a})

-- | Information to be changed about the build environment for the build project.
upEnvironment :: Lens' UpdateProject (Maybe ProjectEnvironment)
upEnvironment = lens _upEnvironment (\s a -> s {_upEnvironment = a})

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
upBadgeEnabled :: Lens' UpdateProject (Maybe Bool)
upBadgeEnabled = lens _upBadgeEnabled (\s a -> s {_upBadgeEnabled = a})

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
upSecondarySourceVersions :: Lens' UpdateProject [ProjectSourceVersion]
upSecondarySourceVersions = lens _upSecondarySourceVersions (\s a -> s {_upSecondarySourceVersions = a}) . _Default . _Coerce

-- | The number of minutes a build is allowed to be queued before it times out.
upQueuedTimeoutInMinutes :: Lens' UpdateProject (Maybe Natural)
upQueuedTimeoutInMinutes = lens _upQueuedTimeoutInMinutes (\s a -> s {_upQueuedTimeoutInMinutes = a}) . mapping _Nat

-- | Stores recently used information so that it can be quickly accessed at a later time.
upCache :: Lens' UpdateProject (Maybe ProjectCache)
upCache = lens _upCache (\s a -> s {_upCache = a})

-- | An array of @ProjectSource@ objects.
upSecondarySources :: Lens' UpdateProject [ProjectSource]
upSecondarySources = lens _upSecondarySources (\s a -> s {_upSecondarySources = a}) . _Default . _Coerce

-- | A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:      * For AWS CodeCommit: the commit ID, branch, or Git tag to use.     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
upSourceVersion :: Lens' UpdateProject (Maybe Text)
upSourceVersion = lens _upSourceVersion (\s a -> s {_upSourceVersion = a})

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
upVpcConfig :: Lens' UpdateProject (Maybe VPCConfig)
upVpcConfig = lens _upVpcConfig (\s a -> s {_upVpcConfig = a})

-- | Information to be changed about the build input source code for the build project.
upSource :: Lens' UpdateProject (Maybe ProjectSource)
upSource = lens _upSource (\s a -> s {_upSource = a})

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
upLogsConfig :: Lens' UpdateProject (Maybe LogsConfig)
upLogsConfig = lens _upLogsConfig (\s a -> s {_upLogsConfig = a})

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
upFileSystemLocations :: Lens' UpdateProject [ProjectFileSystemLocation]
upFileSystemLocations = lens _upFileSystemLocations (\s a -> s {_upFileSystemLocations = a}) . _Default . _Coerce

-- | Undocumented member.
upBuildBatchConfig :: Lens' UpdateProject (Maybe ProjectBuildBatchConfig)
upBuildBatchConfig = lens _upBuildBatchConfig (\s a -> s {_upBuildBatchConfig = a})

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
upEncryptionKey :: Lens' UpdateProject (Maybe Text)
upEncryptionKey = lens _upEncryptionKey (\s a -> s {_upEncryptionKey = a})

-- | A new or replacement description of the build project.
upDescription :: Lens' UpdateProject (Maybe Text)
upDescription = lens _upDescription (\s a -> s {_upDescription = a})

-- | The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
upServiceRole :: Lens' UpdateProject (Maybe Text)
upServiceRole = lens _upServiceRole (\s a -> s {_upServiceRole = a})

-- | An updated list of tag key and value pairs associated with this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
upTags :: Lens' UpdateProject [Tag]
upTags = lens _upTags (\s a -> s {_upTags = a}) . _Default . _Coerce

-- | The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
upTimeoutInMinutes :: Lens' UpdateProject (Maybe Natural)
upTimeoutInMinutes = lens _upTimeoutInMinutes (\s a -> s {_upTimeoutInMinutes = a}) . mapping _Nat

-- | The name of the build project.
upName :: Lens' UpdateProject Text
upName = lens _upName (\s a -> s {_upName = a})

instance AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            <$> (x .?> "project") <*> (pure (fromEnum s))
      )

instance Hashable UpdateProject

instance NFData UpdateProject

instance ToHeaders UpdateProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.UpdateProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    object
      ( catMaybes
          [ ("secondaryArtifacts" .=) <$> _upSecondaryArtifacts,
            ("artifacts" .=) <$> _upArtifacts,
            ("environment" .=) <$> _upEnvironment,
            ("badgeEnabled" .=) <$> _upBadgeEnabled,
            ("secondarySourceVersions" .=) <$> _upSecondarySourceVersions,
            ("queuedTimeoutInMinutes" .=) <$> _upQueuedTimeoutInMinutes,
            ("cache" .=) <$> _upCache,
            ("secondarySources" .=) <$> _upSecondarySources,
            ("sourceVersion" .=) <$> _upSourceVersion,
            ("vpcConfig" .=) <$> _upVpcConfig,
            ("source" .=) <$> _upSource,
            ("logsConfig" .=) <$> _upLogsConfig,
            ("fileSystemLocations" .=) <$> _upFileSystemLocations,
            ("buildBatchConfig" .=) <$> _upBuildBatchConfig,
            ("encryptionKey" .=) <$> _upEncryptionKey,
            ("description" .=) <$> _upDescription,
            ("serviceRole" .=) <$> _upServiceRole,
            ("tags" .=) <$> _upTags,
            ("timeoutInMinutes" .=) <$> _upTimeoutInMinutes,
            Just ("name" .= _upName)
          ]
      )

instance ToPath UpdateProject where
  toPath = const "/"

instance ToQuery UpdateProject where
  toQuery = const mempty

-- | /See:/ 'updateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { _uprsProject ::
      !(Maybe Project),
    _uprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsProject' - Information about the build project that was changed.
--
-- * 'uprsResponseStatus' - -- | The response status code.
updateProjectResponse ::
  -- | 'uprsResponseStatus'
  Int ->
  UpdateProjectResponse
updateProjectResponse pResponseStatus_ =
  UpdateProjectResponse'
    { _uprsProject = Nothing,
      _uprsResponseStatus = pResponseStatus_
    }

-- | Information about the build project that was changed.
uprsProject :: Lens' UpdateProjectResponse (Maybe Project)
uprsProject = lens _uprsProject (\s a -> s {_uprsProject = a})

-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdateProjectResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\s a -> s {_uprsResponseStatus = a})

instance NFData UpdateProjectResponse
