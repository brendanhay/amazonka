{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectEnvironment where

import Network.AWS.CodeBuild.Types.ComputeType
import Network.AWS.CodeBuild.Types.EnvironmentType
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
import Network.AWS.CodeBuild.Types.RegistryCredential
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the build environment of the build project.
--
--
--
-- /See:/ 'projectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
  { _peImagePullCredentialsType ::
      !(Maybe ImagePullCredentialsType),
    _pePrivilegedMode :: !(Maybe Bool),
    _peRegistryCredential :: !(Maybe RegistryCredential),
    _peCertificate :: !(Maybe Text),
    _peEnvironmentVariables ::
      !(Maybe [EnvironmentVariable]),
    _peType :: !EnvironmentType,
    _peImage :: !Text,
    _peComputeType :: !ComputeType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peImagePullCredentialsType' - The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:      * @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.      * @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build project's service role.  When you use a cross-account or private registry image, you must use SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image, you must use CODEBUILD credentials.
--
-- * 'pePrivilegedMode' - Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ . You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file: If the operating system's base image is Ubuntu Linux: @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@  @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@  If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ : @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@  @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
--
-- * 'peRegistryCredential' - The credentials for access to a private registry.
--
-- * 'peCertificate' - The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
--
-- * 'peEnvironmentVariables' - A set of environment variables to make available to builds for this build project.
--
-- * 'peType' - The type of build environment to use for related builds.     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
--
-- * 'peImage' - The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .      * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
--
-- * 'peComputeType' - Information about the compute resources the build project uses. Available values include:     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.     * @BUILD_GENERAL1_LARGE@ : Use up to 16 GB memory and 8 vCPUs for builds, depending on your environment type.     * @BUILD_GENERAL1_2XLARGE@ : Use up to 145 GB memory, 72 vCPUs, and 824 GB of SSD storage for builds. This compute type supports Docker images up to 100 GB uncompressed. If you use @BUILD_GENERAL1_LARGE@ :      * For environment type @LINUX_CONTAINER@ , you can use up to 15 GB memory and 8 vCPUs for builds.      * For environment type @LINUX_GPU_CONTAINER@ , you can use up to 255 GB memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.     * For environment type @ARM_CONTAINER@ , you can use up to 16 GB memory and 8 vCPUs on ARM-based processors for builds. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types> in the /AWS CodeBuild User Guide./
projectEnvironment ::
  -- | 'peType'
  EnvironmentType ->
  -- | 'peImage'
  Text ->
  -- | 'peComputeType'
  ComputeType ->
  ProjectEnvironment
projectEnvironment pType_ pImage_ pComputeType_ =
  ProjectEnvironment'
    { _peImagePullCredentialsType = Nothing,
      _pePrivilegedMode = Nothing,
      _peRegistryCredential = Nothing,
      _peCertificate = Nothing,
      _peEnvironmentVariables = Nothing,
      _peType = pType_,
      _peImage = pImage_,
      _peComputeType = pComputeType_
    }

-- | The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:      * @CODEBUILD@ specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.      * @SERVICE_ROLE@ specifies that AWS CodeBuild uses your build project's service role.  When you use a cross-account or private registry image, you must use SERVICE_ROLE credentials. When you use an AWS CodeBuild curated image, you must use CODEBUILD credentials.
peImagePullCredentialsType :: Lens' ProjectEnvironment (Maybe ImagePullCredentialsType)
peImagePullCredentialsType = lens _peImagePullCredentialsType (\s a -> s {_peImagePullCredentialsType = a})

-- | Enables running the Docker daemon inside a Docker container. Set to true only if the build project is used to build Docker images. Otherwise, a build that attempts to interact with the Docker daemon fails. The default setting is @false@ . You can initialize the Docker daemon during the install phase of your build by adding one of the following sets of commands to the install phase of your buildspec file: If the operating system's base image is Ubuntu Linux: @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@  @- timeout 15 sh -c "until docker info; do echo .; sleep 1; done"@  If the operating system's base image is Alpine Linux and the previous command does not work, add the @-t@ argument to @timeout@ : @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=overlay&@  @- timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
pePrivilegedMode :: Lens' ProjectEnvironment (Maybe Bool)
pePrivilegedMode = lens _pePrivilegedMode (\s a -> s {_pePrivilegedMode = a})

-- | The credentials for access to a private registry.
peRegistryCredential :: Lens' ProjectEnvironment (Maybe RegistryCredential)
peRegistryCredential = lens _peRegistryCredential (\s a -> s {_peRegistryCredential = a})

-- | The ARN of the Amazon Simple Storage Service (Amazon S3) bucket, path prefix, and object key that contains the PEM-encoded certificate for the build project. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/create-project-cli.html#cli.environment.certificate certificate> in the /AWS CodeBuild User Guide/ .
peCertificate :: Lens' ProjectEnvironment (Maybe Text)
peCertificate = lens _peCertificate (\s a -> s {_peCertificate = a})

-- | A set of environment variables to make available to builds for this build project.
peEnvironmentVariables :: Lens' ProjectEnvironment [EnvironmentVariable]
peEnvironmentVariables = lens _peEnvironmentVariables (\s a -> s {_peEnvironmentVariables = a}) . _Default . _Coerce

-- | The type of build environment to use for related builds.     * The environment type @ARM_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), EU (Ireland), Asia Pacific (Mumbai), Asia Pacific (Tokyo), Asia Pacific (Sydney), and EU (Frankfurt).     * The environment type @LINUX_CONTAINER@ with compute type @build.general1.2xlarge@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney), China (Beijing), and China (Ningxia).     * The environment type @LINUX_GPU_CONTAINER@ is available only in regions US East (N. Virginia), US East (Ohio), US West (Oregon), Canada (Central), EU (Ireland), EU (London), EU (Frankfurt), Asia Pacific (Tokyo), Asia Pacific (Seoul), Asia Pacific (Singapore), Asia Pacific (Sydney) , China (Beijing), and China (Ningxia).
peType :: Lens' ProjectEnvironment EnvironmentType
peType = lens _peType (\s a -> s {_peType = a})

-- | The image tag or image digest that identifies the Docker image to use for this build project. Use the following formats:     * For an image tag: @<registry>/<repository>:<tag>@ . For example, in the Docker repository that CodeBuild uses to manage its Docker images, this would be @aws/codebuild/standard:4.0@ .      * For an image digest: @<registry>/<repository>@<digest>@ . For example, to specify an image with the digest "sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf," use @<registry>/<repository>@sha256:cbbf2f9a99b47fc460d422812b6a5adff7dfee951d8fa2e4a98caa0382cfbdbf@ .
peImage :: Lens' ProjectEnvironment Text
peImage = lens _peImage (\s a -> s {_peImage = a})

-- | Information about the compute resources the build project uses. Available values include:     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.     * @BUILD_GENERAL1_LARGE@ : Use up to 16 GB memory and 8 vCPUs for builds, depending on your environment type.     * @BUILD_GENERAL1_2XLARGE@ : Use up to 145 GB memory, 72 vCPUs, and 824 GB of SSD storage for builds. This compute type supports Docker images up to 100 GB uncompressed. If you use @BUILD_GENERAL1_LARGE@ :      * For environment type @LINUX_CONTAINER@ , you can use up to 15 GB memory and 8 vCPUs for builds.      * For environment type @LINUX_GPU_CONTAINER@ , you can use up to 255 GB memory, 32 vCPUs, and 4 NVIDIA Tesla V100 GPUs for builds.     * For environment type @ARM_CONTAINER@ , you can use up to 16 GB memory and 8 vCPUs on ARM-based processors for builds. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html Build Environment Compute Types> in the /AWS CodeBuild User Guide./
peComputeType :: Lens' ProjectEnvironment ComputeType
peComputeType = lens _peComputeType (\s a -> s {_peComputeType = a})

instance FromJSON ProjectEnvironment where
  parseJSON =
    withObject
      "ProjectEnvironment"
      ( \x ->
          ProjectEnvironment'
            <$> (x .:? "imagePullCredentialsType")
            <*> (x .:? "privilegedMode")
            <*> (x .:? "registryCredential")
            <*> (x .:? "certificate")
            <*> (x .:? "environmentVariables" .!= mempty)
            <*> (x .: "type")
            <*> (x .: "image")
            <*> (x .: "computeType")
      )

instance Hashable ProjectEnvironment

instance NFData ProjectEnvironment

instance ToJSON ProjectEnvironment where
  toJSON ProjectEnvironment' {..} =
    object
      ( catMaybes
          [ ("imagePullCredentialsType" .=) <$> _peImagePullCredentialsType,
            ("privilegedMode" .=) <$> _pePrivilegedMode,
            ("registryCredential" .=) <$> _peRegistryCredential,
            ("certificate" .=) <$> _peCertificate,
            ("environmentVariables" .=) <$> _peEnvironmentVariables,
            Just ("type" .= _peType),
            Just ("image" .= _peImage),
            Just ("computeType" .= _peComputeType)
          ]
      )
