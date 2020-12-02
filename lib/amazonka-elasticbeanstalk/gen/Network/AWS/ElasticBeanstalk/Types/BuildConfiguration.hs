{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.BuildConfiguration where

import Network.AWS.ElasticBeanstalk.Types.ComputeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for an AWS CodeBuild build.
--
--
--
-- /See:/ 'buildConfiguration' smart constructor.
data BuildConfiguration = BuildConfiguration'
  { _bcArtifactName ::
      !(Maybe Text),
    _bcComputeType :: !(Maybe ComputeType),
    _bcTimeoutInMinutes :: !(Maybe Int),
    _bcCodeBuildServiceRole :: !Text,
    _bcImage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcArtifactName' - The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
--
-- * 'bcComputeType' - Information about the compute resources the build project will use.     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@      * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@      * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
--
-- * 'bcTimeoutInMinutes' - How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
--
-- * 'bcCodeBuildServiceRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'bcImage' - The ID of the Docker image to use for this build project.
buildConfiguration ::
  -- | 'bcCodeBuildServiceRole'
  Text ->
  -- | 'bcImage'
  Text ->
  BuildConfiguration
buildConfiguration pCodeBuildServiceRole_ pImage_ =
  BuildConfiguration'
    { _bcArtifactName = Nothing,
      _bcComputeType = Nothing,
      _bcTimeoutInMinutes = Nothing,
      _bcCodeBuildServiceRole = pCodeBuildServiceRole_,
      _bcImage = pImage_
    }

-- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
bcArtifactName :: Lens' BuildConfiguration (Maybe Text)
bcArtifactName = lens _bcArtifactName (\s a -> s {_bcArtifactName = a})

-- | Information about the compute resources the build project will use.     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@      * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@      * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
bcComputeType :: Lens' BuildConfiguration (Maybe ComputeType)
bcComputeType = lens _bcComputeType (\s a -> s {_bcComputeType = a})

-- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
bcTimeoutInMinutes :: Lens' BuildConfiguration (Maybe Int)
bcTimeoutInMinutes = lens _bcTimeoutInMinutes (\s a -> s {_bcTimeoutInMinutes = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
bcCodeBuildServiceRole :: Lens' BuildConfiguration Text
bcCodeBuildServiceRole = lens _bcCodeBuildServiceRole (\s a -> s {_bcCodeBuildServiceRole = a})

-- | The ID of the Docker image to use for this build project.
bcImage :: Lens' BuildConfiguration Text
bcImage = lens _bcImage (\s a -> s {_bcImage = a})

instance Hashable BuildConfiguration

instance NFData BuildConfiguration

instance ToQuery BuildConfiguration where
  toQuery BuildConfiguration' {..} =
    mconcat
      [ "ArtifactName" =: _bcArtifactName,
        "ComputeType" =: _bcComputeType,
        "TimeoutInMinutes" =: _bcTimeoutInMinutes,
        "CodeBuildServiceRole" =: _bcCodeBuildServiceRole,
        "Image" =: _bcImage
      ]
