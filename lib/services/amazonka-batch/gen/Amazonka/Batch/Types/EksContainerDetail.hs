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
-- Module      : Amazonka.Batch.Types.EksContainerDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerDetail where

import Amazonka.Batch.Types.EksContainerEnvironmentVariable
import Amazonka.Batch.Types.EksContainerResourceRequirements
import Amazonka.Batch.Types.EksContainerSecurityContext
import Amazonka.Batch.Types.EksContainerVolumeMount
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details for container properties that are returned by @DescribeJobs@
-- for jobs that use Amazon EKS.
--
-- /See:/ 'newEksContainerDetail' smart constructor.
data EksContainerDetail = EksContainerDetail'
  { -- | An array of arguments to the entrypoint. If this isn\'t specified, the
    -- @CMD@ of the container image is used. This corresponds to the @args@
    -- member in the
    -- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
    -- portion of the
    -- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/ Pod>
    -- in Kubernetes. Environment variable references are expanded using the
    -- container\'s environment.
    --
    -- If the referenced environment variable doesn\'t exist, the reference in
    -- the command isn\'t changed. For example, if the reference is to
    -- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
    -- command string will remain \"@$(NAME1)@\". @$$@ is replaced with @$@ and
    -- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
    -- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
    -- variable exists. For more information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
    -- /Dockerfile reference/ and
    -- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
    -- in the /Kubernetes documentation/.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The entrypoint for the container. For more information, see
    -- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
    -- in the /Kubernetes documentation/.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to pass to a container.
    --
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    env :: Prelude.Maybe [EksContainerEnvironmentVariable],
    -- | The exit code for the job attempt. A non-zero exit code is considered
    -- failed.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The Docker image used to start the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The image pull policy for the container. Supported values are @Always@,
    -- @IfNotPresent@, and @Never@. This parameter defaults to @Always@ if the
    -- @:latest@ tag is specified, @IfNotPresent@ otherwise. For more
    -- information, see
    -- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
    -- in the /Kubernetes documentation/.
    imagePullPolicy :: Prelude.Maybe Prelude.Text,
    -- | The name of the container. If the name isn\'t specified, the default
    -- name \"@Default@\" is used. Each container in a pod must have a unique
    -- name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A short human-readable string to provide additional details for a
    -- running or stopped container. It can be up to 255 characters long.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. The supported
    -- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
    -- information, see
    -- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
    -- in the /Kubernetes documentation/.
    resources :: Prelude.Maybe EksContainerResourceRequirements,
    -- | The security context for a job. For more information, see
    -- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
    -- in the /Kubernetes documentation/.
    securityContext :: Prelude.Maybe EksContainerSecurityContext,
    -- | The volume mounts for the container. Batch supports @emptyDir@,
    -- @hostPath@, and @secret@ volume types. For more information about
    -- volumes and volume mounts in Kubernetes, see
    -- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
    -- /Kubernetes documentation/.
    volumeMounts :: Prelude.Maybe [EksContainerVolumeMount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'eksContainerDetail_args' - An array of arguments to the entrypoint. If this isn\'t specified, the
-- @CMD@ of the container image is used. This corresponds to the @args@
-- member in the
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- portion of the
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/ Pod>
-- in Kubernetes. Environment variable references are expanded using the
-- container\'s environment.
--
-- If the referenced environment variable doesn\'t exist, the reference in
-- the command isn\'t changed. For example, if the reference is to
-- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
-- command string will remain \"@$(NAME1)@\". @$$@ is replaced with @$@ and
-- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
-- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
-- in the /Kubernetes documentation/.
--
-- 'command', 'eksContainerDetail_command' - The entrypoint for the container. For more information, see
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- in the /Kubernetes documentation/.
--
-- 'env', 'eksContainerDetail_env' - The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'exitCode', 'eksContainerDetail_exitCode' - The exit code for the job attempt. A non-zero exit code is considered
-- failed.
--
-- 'image', 'eksContainerDetail_image' - The Docker image used to start the container.
--
-- 'imagePullPolicy', 'eksContainerDetail_imagePullPolicy' - The image pull policy for the container. Supported values are @Always@,
-- @IfNotPresent@, and @Never@. This parameter defaults to @Always@ if the
-- @:latest@ tag is specified, @IfNotPresent@ otherwise. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
-- in the /Kubernetes documentation/.
--
-- 'name', 'eksContainerDetail_name' - The name of the container. If the name isn\'t specified, the default
-- name \"@Default@\" is used. Each container in a pod must have a unique
-- name.
--
-- 'reason', 'eksContainerDetail_reason' - A short human-readable string to provide additional details for a
-- running or stopped container. It can be up to 255 characters long.
--
-- 'resources', 'eksContainerDetail_resources' - The type and amount of resources to assign to a container. The supported
-- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
--
-- 'securityContext', 'eksContainerDetail_securityContext' - The security context for a job. For more information, see
-- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
-- in the /Kubernetes documentation/.
--
-- 'volumeMounts', 'eksContainerDetail_volumeMounts' - The volume mounts for the container. Batch supports @emptyDir@,
-- @hostPath@, and @secret@ volume types. For more information about
-- volumes and volume mounts in Kubernetes, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
-- /Kubernetes documentation/.
newEksContainerDetail ::
  EksContainerDetail
newEksContainerDetail =
  EksContainerDetail'
    { args = Prelude.Nothing,
      command = Prelude.Nothing,
      env = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      image = Prelude.Nothing,
      imagePullPolicy = Prelude.Nothing,
      name = Prelude.Nothing,
      reason = Prelude.Nothing,
      resources = Prelude.Nothing,
      securityContext = Prelude.Nothing,
      volumeMounts = Prelude.Nothing
    }

-- | An array of arguments to the entrypoint. If this isn\'t specified, the
-- @CMD@ of the container image is used. This corresponds to the @args@
-- member in the
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- portion of the
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/ Pod>
-- in Kubernetes. Environment variable references are expanded using the
-- container\'s environment.
--
-- If the referenced environment variable doesn\'t exist, the reference in
-- the command isn\'t changed. For example, if the reference is to
-- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
-- command string will remain \"@$(NAME1)@\". @$$@ is replaced with @$@ and
-- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
-- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
-- in the /Kubernetes documentation/.
eksContainerDetail_args :: Lens.Lens' EksContainerDetail (Prelude.Maybe [Prelude.Text])
eksContainerDetail_args = Lens.lens (\EksContainerDetail' {args} -> args) (\s@EksContainerDetail' {} a -> s {args = a} :: EksContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The entrypoint for the container. For more information, see
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- in the /Kubernetes documentation/.
eksContainerDetail_command :: Lens.Lens' EksContainerDetail (Prelude.Maybe [Prelude.Text])
eksContainerDetail_command = Lens.lens (\EksContainerDetail' {command} -> command) (\s@EksContainerDetail' {} a -> s {command = a} :: EksContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
eksContainerDetail_env :: Lens.Lens' EksContainerDetail (Prelude.Maybe [EksContainerEnvironmentVariable])
eksContainerDetail_env = Lens.lens (\EksContainerDetail' {env} -> env) (\s@EksContainerDetail' {} a -> s {env = a} :: EksContainerDetail) Prelude.. Lens.mapping Lens.coerced

-- | The exit code for the job attempt. A non-zero exit code is considered
-- failed.
eksContainerDetail_exitCode :: Lens.Lens' EksContainerDetail (Prelude.Maybe Prelude.Int)
eksContainerDetail_exitCode = Lens.lens (\EksContainerDetail' {exitCode} -> exitCode) (\s@EksContainerDetail' {} a -> s {exitCode = a} :: EksContainerDetail)

-- | The Docker image used to start the container.
eksContainerDetail_image :: Lens.Lens' EksContainerDetail (Prelude.Maybe Prelude.Text)
eksContainerDetail_image = Lens.lens (\EksContainerDetail' {image} -> image) (\s@EksContainerDetail' {} a -> s {image = a} :: EksContainerDetail)

-- | The image pull policy for the container. Supported values are @Always@,
-- @IfNotPresent@, and @Never@. This parameter defaults to @Always@ if the
-- @:latest@ tag is specified, @IfNotPresent@ otherwise. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
-- in the /Kubernetes documentation/.
eksContainerDetail_imagePullPolicy :: Lens.Lens' EksContainerDetail (Prelude.Maybe Prelude.Text)
eksContainerDetail_imagePullPolicy = Lens.lens (\EksContainerDetail' {imagePullPolicy} -> imagePullPolicy) (\s@EksContainerDetail' {} a -> s {imagePullPolicy = a} :: EksContainerDetail)

-- | The name of the container. If the name isn\'t specified, the default
-- name \"@Default@\" is used. Each container in a pod must have a unique
-- name.
eksContainerDetail_name :: Lens.Lens' EksContainerDetail (Prelude.Maybe Prelude.Text)
eksContainerDetail_name = Lens.lens (\EksContainerDetail' {name} -> name) (\s@EksContainerDetail' {} a -> s {name = a} :: EksContainerDetail)

-- | A short human-readable string to provide additional details for a
-- running or stopped container. It can be up to 255 characters long.
eksContainerDetail_reason :: Lens.Lens' EksContainerDetail (Prelude.Maybe Prelude.Text)
eksContainerDetail_reason = Lens.lens (\EksContainerDetail' {reason} -> reason) (\s@EksContainerDetail' {} a -> s {reason = a} :: EksContainerDetail)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
eksContainerDetail_resources :: Lens.Lens' EksContainerDetail (Prelude.Maybe EksContainerResourceRequirements)
eksContainerDetail_resources = Lens.lens (\EksContainerDetail' {resources} -> resources) (\s@EksContainerDetail' {} a -> s {resources = a} :: EksContainerDetail)

-- | The security context for a job. For more information, see
-- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
-- in the /Kubernetes documentation/.
eksContainerDetail_securityContext :: Lens.Lens' EksContainerDetail (Prelude.Maybe EksContainerSecurityContext)
eksContainerDetail_securityContext = Lens.lens (\EksContainerDetail' {securityContext} -> securityContext) (\s@EksContainerDetail' {} a -> s {securityContext = a} :: EksContainerDetail)

-- | The volume mounts for the container. Batch supports @emptyDir@,
-- @hostPath@, and @secret@ volume types. For more information about
-- volumes and volume mounts in Kubernetes, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
-- /Kubernetes documentation/.
eksContainerDetail_volumeMounts :: Lens.Lens' EksContainerDetail (Prelude.Maybe [EksContainerVolumeMount])
eksContainerDetail_volumeMounts = Lens.lens (\EksContainerDetail' {volumeMounts} -> volumeMounts) (\s@EksContainerDetail' {} a -> s {volumeMounts = a} :: EksContainerDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EksContainerDetail where
  parseJSON =
    Data.withObject
      "EksContainerDetail"
      ( \x ->
          EksContainerDetail'
            Prelude.<$> (x Data..:? "args" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "env" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "imagePullPolicy")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "resources")
            Prelude.<*> (x Data..:? "securityContext")
            Prelude.<*> (x Data..:? "volumeMounts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EksContainerDetail where
  hashWithSalt _salt EksContainerDetail' {..} =
    _salt `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` env
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` imagePullPolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` securityContext
      `Prelude.hashWithSalt` volumeMounts

instance Prelude.NFData EksContainerDetail where
  rnf EksContainerDetail' {..} =
    Prelude.rnf args
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf env
      `Prelude.seq` Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf imagePullPolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf securityContext
      `Prelude.seq` Prelude.rnf volumeMounts
