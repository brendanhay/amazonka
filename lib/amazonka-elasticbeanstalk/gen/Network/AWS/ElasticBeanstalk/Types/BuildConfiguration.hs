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
    bcCodeBuildServiceRole,
    bcImage,
    bcArtifactName,
    bcComputeType,
    bcTimeoutInMinutes,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ComputeType as Types
import qualified Network.AWS.ElasticBeanstalk.Types.NonEmptyString as Types
import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for an AWS CodeBuild build.
--
-- /See:/ 'mkBuildConfiguration' smart constructor.
data BuildConfiguration = BuildConfiguration'
  { -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
    codeBuildServiceRole :: Types.NonEmptyString,
    -- | The ID of the Docker image to use for this build project.
    image :: Types.NonEmptyString,
    -- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
    artifactName :: Core.Maybe Types.String,
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
    computeType :: Core.Maybe Types.ComputeType,
    -- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
    timeoutInMinutes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildConfiguration' value with any optional fields omitted.
mkBuildConfiguration ::
  -- | 'codeBuildServiceRole'
  Types.NonEmptyString ->
  -- | 'image'
  Types.NonEmptyString ->
  BuildConfiguration
mkBuildConfiguration codeBuildServiceRole image =
  BuildConfiguration'
    { codeBuildServiceRole,
      image,
      artifactName = Core.Nothing,
      computeType = Core.Nothing,
      timeoutInMinutes = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'codeBuildServiceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCodeBuildServiceRole :: Lens.Lens' BuildConfiguration Types.NonEmptyString
bcCodeBuildServiceRole = Lens.field @"codeBuildServiceRole"
{-# DEPRECATED bcCodeBuildServiceRole "Use generic-lens or generic-optics with 'codeBuildServiceRole' instead." #-}

-- | The ID of the Docker image to use for this build project.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcImage :: Lens.Lens' BuildConfiguration Types.NonEmptyString
bcImage = Lens.field @"image"
{-# DEPRECATED bcImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
--
-- /Note:/ Consider using 'artifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcArtifactName :: Lens.Lens' BuildConfiguration (Core.Maybe Types.String)
bcArtifactName = Lens.field @"artifactName"
{-# DEPRECATED bcArtifactName "Use generic-lens or generic-optics with 'artifactName' instead." #-}

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
bcComputeType :: Lens.Lens' BuildConfiguration (Core.Maybe Types.ComputeType)
bcComputeType = Lens.field @"computeType"
{-# DEPRECATED bcComputeType "Use generic-lens or generic-optics with 'computeType' instead." #-}

-- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcTimeoutInMinutes :: Lens.Lens' BuildConfiguration (Core.Maybe Core.Int)
bcTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED bcTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}
