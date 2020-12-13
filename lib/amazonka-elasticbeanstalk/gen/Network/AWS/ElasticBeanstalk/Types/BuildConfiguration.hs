{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
  ( BuildConfiguration (..),

    -- * Smart constructor
    mkBuildConfiguration,

    -- * Lenses
    bcImage,
    bcArtifactName,
    bcCodeBuildServiceRole,
    bcComputeType,
    bcTimeoutInMinutes,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ComputeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for an AWS CodeBuild build.
--
-- /See:/ 'mkBuildConfiguration' smart constructor.
data BuildConfiguration = BuildConfiguration'
  { -- | The ID of the Docker image to use for this build project.
    image :: Lude.Text,
    -- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
    artifactName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
    codeBuildServiceRole :: Lude.Text,
    -- | Information about the compute resources the build project will use.
    --
    --
    --     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
    --
    --
    --     * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
    --
    --
    --     * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
    computeType :: Lude.Maybe ComputeType,
    -- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
    timeoutInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildConfiguration' with the minimum fields required to make a request.
--
-- * 'image' - The ID of the Docker image to use for this build project.
-- * 'artifactName' - The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
-- * 'codeBuildServiceRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
-- * 'computeType' - Information about the compute resources the build project will use.
--
--
--     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
--
--
--     * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
--
--
--     * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
--
--
-- * 'timeoutInMinutes' - How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
mkBuildConfiguration ::
  -- | 'image'
  Lude.Text ->
  -- | 'codeBuildServiceRole'
  Lude.Text ->
  BuildConfiguration
mkBuildConfiguration pImage_ pCodeBuildServiceRole_ =
  BuildConfiguration'
    { image = pImage_,
      artifactName = Lude.Nothing,
      codeBuildServiceRole = pCodeBuildServiceRole_,
      computeType = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing
    }

-- | The ID of the Docker image to use for this build project.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcImage :: Lens.Lens' BuildConfiguration Lude.Text
bcImage = Lens.lens (image :: BuildConfiguration -> Lude.Text) (\s a -> s {image = a} :: BuildConfiguration)
{-# DEPRECATED bcImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
--
-- /Note:/ Consider using 'artifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcArtifactName :: Lens.Lens' BuildConfiguration (Lude.Maybe Lude.Text)
bcArtifactName = Lens.lens (artifactName :: BuildConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {artifactName = a} :: BuildConfiguration)
{-# DEPRECATED bcArtifactName "Use generic-lens or generic-optics with 'artifactName' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'codeBuildServiceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCodeBuildServiceRole :: Lens.Lens' BuildConfiguration Lude.Text
bcCodeBuildServiceRole = Lens.lens (codeBuildServiceRole :: BuildConfiguration -> Lude.Text) (\s a -> s {codeBuildServiceRole = a} :: BuildConfiguration)
{-# DEPRECATED bcCodeBuildServiceRole "Use generic-lens or generic-optics with 'codeBuildServiceRole' instead." #-}

-- | Information about the compute resources the build project will use.
--
--
--     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@
--
--
--     * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@
--
--
--     * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
--
--
--
-- /Note:/ Consider using 'computeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcComputeType :: Lens.Lens' BuildConfiguration (Lude.Maybe ComputeType)
bcComputeType = Lens.lens (computeType :: BuildConfiguration -> Lude.Maybe ComputeType) (\s a -> s {computeType = a} :: BuildConfiguration)
{-# DEPRECATED bcComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcTimeoutInMinutes :: Lens.Lens' BuildConfiguration (Lude.Maybe Lude.Int)
bcTimeoutInMinutes = Lens.lens (timeoutInMinutes :: BuildConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {timeoutInMinutes = a} :: BuildConfiguration)
{-# DEPRECATED bcTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Lude.ToQuery BuildConfiguration where
  toQuery BuildConfiguration' {..} =
    Lude.mconcat
      [ "Image" Lude.=: image,
        "ArtifactName" Lude.=: artifactName,
        "CodeBuildServiceRole" Lude.=: codeBuildServiceRole,
        "ComputeType" Lude.=: computeType,
        "TimeoutInMinutes" Lude.=: timeoutInMinutes
      ]
