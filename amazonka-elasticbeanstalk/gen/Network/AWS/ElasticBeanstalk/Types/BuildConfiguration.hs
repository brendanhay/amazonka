{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.BuildConfiguration where

import Network.AWS.ElasticBeanstalk.Types.ComputeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for an AWS CodeBuild build.
--
-- /See:/ 'newBuildConfiguration' smart constructor.
data BuildConfiguration = BuildConfiguration'
  { -- | The name of the artifact of the CodeBuild build. If provided, Elastic
    -- Beanstalk stores the build artifact in the S3 location
    -- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/-/artifact-name/.zip.
    -- If not provided, Elastic Beanstalk stores the build artifact in the S3
    -- location
    -- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/.zip.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
    -- until timing out any related build that does not get marked as
    -- completed. The default is 60 minutes.
    timeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Information about the compute resources the build project will use.
    --
    -- -   @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
    --
    -- -   @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
    --
    -- -   @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
    computeType :: Prelude.Maybe ComputeType,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that enables AWS CodeBuild to interact with dependent AWS
    -- services on behalf of the AWS account.
    codeBuildServiceRole :: Prelude.Text,
    -- | The ID of the Docker image to use for this build project.
    image :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BuildConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactName', 'buildConfiguration_artifactName' - The name of the artifact of the CodeBuild build. If provided, Elastic
-- Beanstalk stores the build artifact in the S3 location
-- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/-/artifact-name/.zip.
-- If not provided, Elastic Beanstalk stores the build artifact in the S3
-- location
-- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/.zip.
--
-- 'timeoutInMinutes', 'buildConfiguration_timeoutInMinutes' - How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
-- until timing out any related build that does not get marked as
-- completed. The default is 60 minutes.
--
-- 'computeType', 'buildConfiguration_computeType' - Information about the compute resources the build project will use.
--
-- -   @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
--
-- -   @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
--
-- -   @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
--
-- 'codeBuildServiceRole', 'buildConfiguration_codeBuildServiceRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that enables AWS CodeBuild to interact with dependent AWS
-- services on behalf of the AWS account.
--
-- 'image', 'buildConfiguration_image' - The ID of the Docker image to use for this build project.
newBuildConfiguration ::
  -- | 'codeBuildServiceRole'
  Prelude.Text ->
  -- | 'image'
  Prelude.Text ->
  BuildConfiguration
newBuildConfiguration pCodeBuildServiceRole_ pImage_ =
  BuildConfiguration'
    { artifactName = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      computeType = Prelude.Nothing,
      codeBuildServiceRole = pCodeBuildServiceRole_,
      image = pImage_
    }

-- | The name of the artifact of the CodeBuild build. If provided, Elastic
-- Beanstalk stores the build artifact in the S3 location
-- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/-/artifact-name/.zip.
-- If not provided, Elastic Beanstalk stores the build artifact in the S3
-- location
-- /S3-bucket/\/resources\//application-name/\/codebuild\/codebuild-/version-label/.zip.
buildConfiguration_artifactName :: Lens.Lens' BuildConfiguration (Prelude.Maybe Prelude.Text)
buildConfiguration_artifactName = Lens.lens (\BuildConfiguration' {artifactName} -> artifactName) (\s@BuildConfiguration' {} a -> s {artifactName = a} :: BuildConfiguration)

-- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
-- until timing out any related build that does not get marked as
-- completed. The default is 60 minutes.
buildConfiguration_timeoutInMinutes :: Lens.Lens' BuildConfiguration (Prelude.Maybe Prelude.Int)
buildConfiguration_timeoutInMinutes = Lens.lens (\BuildConfiguration' {timeoutInMinutes} -> timeoutInMinutes) (\s@BuildConfiguration' {} a -> s {timeoutInMinutes = a} :: BuildConfiguration)

-- | Information about the compute resources the build project will use.
--
-- -   @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
--
-- -   @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
--
-- -   @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
buildConfiguration_computeType :: Lens.Lens' BuildConfiguration (Prelude.Maybe ComputeType)
buildConfiguration_computeType = Lens.lens (\BuildConfiguration' {computeType} -> computeType) (\s@BuildConfiguration' {} a -> s {computeType = a} :: BuildConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that enables AWS CodeBuild to interact with dependent AWS
-- services on behalf of the AWS account.
buildConfiguration_codeBuildServiceRole :: Lens.Lens' BuildConfiguration Prelude.Text
buildConfiguration_codeBuildServiceRole = Lens.lens (\BuildConfiguration' {codeBuildServiceRole} -> codeBuildServiceRole) (\s@BuildConfiguration' {} a -> s {codeBuildServiceRole = a} :: BuildConfiguration)

-- | The ID of the Docker image to use for this build project.
buildConfiguration_image :: Lens.Lens' BuildConfiguration Prelude.Text
buildConfiguration_image = Lens.lens (\BuildConfiguration' {image} -> image) (\s@BuildConfiguration' {} a -> s {image = a} :: BuildConfiguration)

instance Prelude.Hashable BuildConfiguration

instance Prelude.NFData BuildConfiguration

instance Prelude.ToQuery BuildConfiguration where
  toQuery BuildConfiguration' {..} =
    Prelude.mconcat
      [ "ArtifactName" Prelude.=: artifactName,
        "TimeoutInMinutes" Prelude.=: timeoutInMinutes,
        "ComputeType" Prelude.=: computeType,
        "CodeBuildServiceRole"
          Prelude.=: codeBuildServiceRole,
        "Image" Prelude.=: image
      ]
