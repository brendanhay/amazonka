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
-- Module      : Amazonka.Batch.Types.EksContainer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainer where

import Amazonka.Batch.Types.EksContainerEnvironmentVariable
import Amazonka.Batch.Types.EksContainerResourceRequirements
import Amazonka.Batch.Types.EksContainerSecurityContext
import Amazonka.Batch.Types.EksContainerVolumeMount
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | EKS container properties are used in job definitions for Amazon EKS
-- based job definitions to describe the properties for a container node in
-- the pod that\'s launched as part of a job. This can\'t be specified for
-- Amazon ECS based job definitions.
--
-- /See:/ 'newEksContainer' smart constructor.
data EksContainer = EksContainer'
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
    -- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@,
    -- and the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
    -- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
    -- variable exists. For more information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
    -- /Dockerfile reference/ and
    -- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
    -- in the /Kubernetes documentation/.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The entrypoint for the container. This isn\'t run within a shell. If
    -- this isn\'t specified, the @ENTRYPOINT@ of the container image is used.
    -- Environment variable references are expanded using the container\'s
    -- environment.
    --
    -- If the referenced environment variable doesn\'t exist, the reference in
    -- the command isn\'t changed. For example, if the reference is to
    -- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
    -- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@ and
    -- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ will
    -- be passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
    -- variable exists. The entrypoint can\'t be updated. For more information,
    -- see
    -- <https://docs.docker.com/engine/reference/builder/#entrypoint ENTRYPOINT>
    -- in the /Dockerfile reference/ and
    -- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a container>
    -- and
    -- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
    -- in the /Kubernetes documentation/.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to pass to a container.
    --
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    env :: Prelude.Maybe [EksContainerEnvironmentVariable],
    -- | The image pull policy for the container. Supported values are @Always@,
    -- @IfNotPresent@, and @Never@. This parameter defaults to @IfNotPresent@.
    -- However, if the @:latest@ tag is specified, it defaults to @Always@. For
    -- more information, see
    -- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
    -- in the /Kubernetes documentation/.
    imagePullPolicy :: Prelude.Maybe Prelude.Text,
    -- | The name of the container. If the name isn\'t specified, the default
    -- name \"@Default@\" is used. Each container in a pod must have a unique
    -- name.
    name :: Prelude.Maybe Prelude.Text,
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
    volumeMounts :: Prelude.Maybe [EksContainerVolumeMount],
    -- | The Docker image used to start the container.
    image :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'eksContainer_args' - An array of arguments to the entrypoint. If this isn\'t specified, the
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
-- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@,
-- and the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
-- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
-- in the /Kubernetes documentation/.
--
-- 'command', 'eksContainer_command' - The entrypoint for the container. This isn\'t run within a shell. If
-- this isn\'t specified, the @ENTRYPOINT@ of the container image is used.
-- Environment variable references are expanded using the container\'s
-- environment.
--
-- If the referenced environment variable doesn\'t exist, the reference in
-- the command isn\'t changed. For example, if the reference is to
-- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
-- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@ and
-- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ will
-- be passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. The entrypoint can\'t be updated. For more information,
-- see
-- <https://docs.docker.com/engine/reference/builder/#entrypoint ENTRYPOINT>
-- in the /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a container>
-- and
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- in the /Kubernetes documentation/.
--
-- 'env', 'eksContainer_env' - The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'imagePullPolicy', 'eksContainer_imagePullPolicy' - The image pull policy for the container. Supported values are @Always@,
-- @IfNotPresent@, and @Never@. This parameter defaults to @IfNotPresent@.
-- However, if the @:latest@ tag is specified, it defaults to @Always@. For
-- more information, see
-- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
-- in the /Kubernetes documentation/.
--
-- 'name', 'eksContainer_name' - The name of the container. If the name isn\'t specified, the default
-- name \"@Default@\" is used. Each container in a pod must have a unique
-- name.
--
-- 'resources', 'eksContainer_resources' - The type and amount of resources to assign to a container. The supported
-- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
--
-- 'securityContext', 'eksContainer_securityContext' - The security context for a job. For more information, see
-- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
-- in the /Kubernetes documentation/.
--
-- 'volumeMounts', 'eksContainer_volumeMounts' - The volume mounts for the container. Batch supports @emptyDir@,
-- @hostPath@, and @secret@ volume types. For more information about
-- volumes and volume mounts in Kubernetes, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
-- /Kubernetes documentation/.
--
-- 'image', 'eksContainer_image' - The Docker image used to start the container.
newEksContainer ::
  -- | 'image'
  Prelude.Text ->
  EksContainer
newEksContainer pImage_ =
  EksContainer'
    { args = Prelude.Nothing,
      command = Prelude.Nothing,
      env = Prelude.Nothing,
      imagePullPolicy = Prelude.Nothing,
      name = Prelude.Nothing,
      resources = Prelude.Nothing,
      securityContext = Prelude.Nothing,
      volumeMounts = Prelude.Nothing,
      image = pImage_
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
-- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@,
-- and the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ is
-- passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. For more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a pod>
-- in the /Kubernetes documentation/.
eksContainer_args :: Lens.Lens' EksContainer (Prelude.Maybe [Prelude.Text])
eksContainer_args = Lens.lens (\EksContainer' {args} -> args) (\s@EksContainer' {} a -> s {args = a} :: EksContainer) Prelude.. Lens.mapping Lens.coerced

-- | The entrypoint for the container. This isn\'t run within a shell. If
-- this isn\'t specified, the @ENTRYPOINT@ of the container image is used.
-- Environment variable references are expanded using the container\'s
-- environment.
--
-- If the referenced environment variable doesn\'t exist, the reference in
-- the command isn\'t changed. For example, if the reference is to
-- \"@$(NAME1)@\" and the @NAME1@ environment variable doesn\'t exist, the
-- command string will remain \"@$(NAME1)@.\" @$$@ is replaced with @$@ and
-- the resulting string isn\'t expanded. For example, @$$(VAR_NAME)@ will
-- be passed as @$(VAR_NAME)@ whether or not the @VAR_NAME@ environment
-- variable exists. The entrypoint can\'t be updated. For more information,
-- see
-- <https://docs.docker.com/engine/reference/builder/#entrypoint ENTRYPOINT>
-- in the /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command and arguments for a container>
-- and
-- <https://kubernetes.io/docs/reference/kubernetes-api/workload-resources/pod-v1/#entrypoint Entrypoint>
-- in the /Kubernetes documentation/.
eksContainer_command :: Lens.Lens' EksContainer (Prelude.Maybe [Prelude.Text])
eksContainer_command = Lens.lens (\EksContainer' {command} -> command) (\s@EksContainer' {} a -> s {command = a} :: EksContainer) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to pass to a container.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
eksContainer_env :: Lens.Lens' EksContainer (Prelude.Maybe [EksContainerEnvironmentVariable])
eksContainer_env = Lens.lens (\EksContainer' {env} -> env) (\s@EksContainer' {} a -> s {env = a} :: EksContainer) Prelude.. Lens.mapping Lens.coerced

-- | The image pull policy for the container. Supported values are @Always@,
-- @IfNotPresent@, and @Never@. This parameter defaults to @IfNotPresent@.
-- However, if the @:latest@ tag is specified, it defaults to @Always@. For
-- more information, see
-- <https://kubernetes.io/docs/concepts/containers/images/#updating-images Updating images>
-- in the /Kubernetes documentation/.
eksContainer_imagePullPolicy :: Lens.Lens' EksContainer (Prelude.Maybe Prelude.Text)
eksContainer_imagePullPolicy = Lens.lens (\EksContainer' {imagePullPolicy} -> imagePullPolicy) (\s@EksContainer' {} a -> s {imagePullPolicy = a} :: EksContainer)

-- | The name of the container. If the name isn\'t specified, the default
-- name \"@Default@\" is used. Each container in a pod must have a unique
-- name.
eksContainer_name :: Lens.Lens' EksContainer (Prelude.Maybe Prelude.Text)
eksContainer_name = Lens.lens (\EksContainer' {name} -> name) (\s@EksContainer' {} a -> s {name = a} :: EksContainer)

-- | The type and amount of resources to assign to a container. The supported
-- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
eksContainer_resources :: Lens.Lens' EksContainer (Prelude.Maybe EksContainerResourceRequirements)
eksContainer_resources = Lens.lens (\EksContainer' {resources} -> resources) (\s@EksContainer' {} a -> s {resources = a} :: EksContainer)

-- | The security context for a job. For more information, see
-- <https://kubernetes.io/docs/tasks/configure-pod-container/security-context/ Configure a security context for a pod or container>
-- in the /Kubernetes documentation/.
eksContainer_securityContext :: Lens.Lens' EksContainer (Prelude.Maybe EksContainerSecurityContext)
eksContainer_securityContext = Lens.lens (\EksContainer' {securityContext} -> securityContext) (\s@EksContainer' {} a -> s {securityContext = a} :: EksContainer)

-- | The volume mounts for the container. Batch supports @emptyDir@,
-- @hostPath@, and @secret@ volume types. For more information about
-- volumes and volume mounts in Kubernetes, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
-- /Kubernetes documentation/.
eksContainer_volumeMounts :: Lens.Lens' EksContainer (Prelude.Maybe [EksContainerVolumeMount])
eksContainer_volumeMounts = Lens.lens (\EksContainer' {volumeMounts} -> volumeMounts) (\s@EksContainer' {} a -> s {volumeMounts = a} :: EksContainer) Prelude.. Lens.mapping Lens.coerced

-- | The Docker image used to start the container.
eksContainer_image :: Lens.Lens' EksContainer Prelude.Text
eksContainer_image = Lens.lens (\EksContainer' {image} -> image) (\s@EksContainer' {} a -> s {image = a} :: EksContainer)

instance Data.FromJSON EksContainer where
  parseJSON =
    Data.withObject
      "EksContainer"
      ( \x ->
          EksContainer'
            Prelude.<$> (x Data..:? "args" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "env" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "imagePullPolicy")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resources")
            Prelude.<*> (x Data..:? "securityContext")
            Prelude.<*> (x Data..:? "volumeMounts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "image")
      )

instance Prelude.Hashable EksContainer where
  hashWithSalt _salt EksContainer' {..} =
    _salt
      `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` env
      `Prelude.hashWithSalt` imagePullPolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resources
      `Prelude.hashWithSalt` securityContext
      `Prelude.hashWithSalt` volumeMounts
      `Prelude.hashWithSalt` image

instance Prelude.NFData EksContainer where
  rnf EksContainer' {..} =
    Prelude.rnf args `Prelude.seq`
      Prelude.rnf command `Prelude.seq`
        Prelude.rnf env `Prelude.seq`
          Prelude.rnf imagePullPolicy `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf resources `Prelude.seq`
                Prelude.rnf securityContext `Prelude.seq`
                  Prelude.rnf volumeMounts `Prelude.seq`
                    Prelude.rnf image

instance Data.ToJSON EksContainer where
  toJSON EksContainer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("args" Data..=) Prelude.<$> args,
            ("command" Data..=) Prelude.<$> command,
            ("env" Data..=) Prelude.<$> env,
            ("imagePullPolicy" Data..=)
              Prelude.<$> imagePullPolicy,
            ("name" Data..=) Prelude.<$> name,
            ("resources" Data..=) Prelude.<$> resources,
            ("securityContext" Data..=)
              Prelude.<$> securityContext,
            ("volumeMounts" Data..=) Prelude.<$> volumeMounts,
            Prelude.Just ("image" Data..= image)
          ]
      )
