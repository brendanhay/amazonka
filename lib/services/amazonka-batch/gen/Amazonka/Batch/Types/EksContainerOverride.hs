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
-- Module      : Amazonka.Batch.Types.EksContainerOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerOverride where

import Amazonka.Batch.Types.EksContainerEnvironmentVariable
import Amazonka.Batch.Types.EksContainerResourceRequirements
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object representing any Kubernetes overrides to a job definition that\'s
-- used in a SubmitJob API operation.
--
-- /See:/ 'newEksContainerOverride' smart constructor.
data EksContainerOverride = EksContainerOverride'
  { -- | The arguments to the entrypoint to send to the container that overrides
    -- the default arguments from the Docker image or the job definition. For
    -- more information, see
    -- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
    -- /Dockerfile reference/ and
    -- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command an arguments for a pod>
    -- in the /Kubernetes documentation/.
    args :: Prelude.Maybe [Prelude.Text],
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the job definition.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch. Or,
    -- you can override the existing environment variables from the Docker
    -- image or the job definition.
    --
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    env :: Prelude.Maybe [EksContainerEnvironmentVariable],
    -- | The override of the Docker image that\'s used to start the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. These
    -- override the settings in the job definition. The supported resources
    -- include @memory@, @cpu@, and @nvidia.com\/gpu@. For more information,
    -- see
    -- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
    -- in the /Kubernetes documentation/.
    resources :: Prelude.Maybe EksContainerResourceRequirements
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'eksContainerOverride_args' - The arguments to the entrypoint to send to the container that overrides
-- the default arguments from the Docker image or the job definition. For
-- more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command an arguments for a pod>
-- in the /Kubernetes documentation/.
--
-- 'command', 'eksContainerOverride_command' - The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
--
-- 'env', 'eksContainerOverride_env' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch. Or,
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'image', 'eksContainerOverride_image' - The override of the Docker image that\'s used to start the container.
--
-- 'resources', 'eksContainerOverride_resources' - The type and amount of resources to assign to a container. These
-- override the settings in the job definition. The supported resources
-- include @memory@, @cpu@, and @nvidia.com\/gpu@. For more information,
-- see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
newEksContainerOverride ::
  EksContainerOverride
newEksContainerOverride =
  EksContainerOverride'
    { args = Prelude.Nothing,
      command = Prelude.Nothing,
      env = Prelude.Nothing,
      image = Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | The arguments to the entrypoint to send to the container that overrides
-- the default arguments from the Docker image or the job definition. For
-- more information, see
-- <https://docs.docker.com/engine/reference/builder/#cmd CMD> in the
-- /Dockerfile reference/ and
-- <https://kubernetes.io/docs/tasks/inject-data-application/define-command-argument-container/ Define a command an arguments for a pod>
-- in the /Kubernetes documentation/.
eksContainerOverride_args :: Lens.Lens' EksContainerOverride (Prelude.Maybe [Prelude.Text])
eksContainerOverride_args = Lens.lens (\EksContainerOverride' {args} -> args) (\s@EksContainerOverride' {} a -> s {args = a} :: EksContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
eksContainerOverride_command :: Lens.Lens' EksContainerOverride (Prelude.Maybe [Prelude.Text])
eksContainerOverride_command = Lens.lens (\EksContainerOverride' {command} -> command) (\s@EksContainerOverride' {} a -> s {command = a} :: EksContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch. Or,
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
eksContainerOverride_env :: Lens.Lens' EksContainerOverride (Prelude.Maybe [EksContainerEnvironmentVariable])
eksContainerOverride_env = Lens.lens (\EksContainerOverride' {env} -> env) (\s@EksContainerOverride' {} a -> s {env = a} :: EksContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The override of the Docker image that\'s used to start the container.
eksContainerOverride_image :: Lens.Lens' EksContainerOverride (Prelude.Maybe Prelude.Text)
eksContainerOverride_image = Lens.lens (\EksContainerOverride' {image} -> image) (\s@EksContainerOverride' {} a -> s {image = a} :: EksContainerOverride)

-- | The type and amount of resources to assign to a container. These
-- override the settings in the job definition. The supported resources
-- include @memory@, @cpu@, and @nvidia.com\/gpu@. For more information,
-- see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
eksContainerOverride_resources :: Lens.Lens' EksContainerOverride (Prelude.Maybe EksContainerResourceRequirements)
eksContainerOverride_resources = Lens.lens (\EksContainerOverride' {resources} -> resources) (\s@EksContainerOverride' {} a -> s {resources = a} :: EksContainerOverride)

instance Prelude.Hashable EksContainerOverride where
  hashWithSalt _salt EksContainerOverride' {..} =
    _salt
      `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` env
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` resources

instance Prelude.NFData EksContainerOverride where
  rnf EksContainerOverride' {..} =
    Prelude.rnf args
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf env
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf resources

instance Data.ToJSON EksContainerOverride where
  toJSON EksContainerOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("args" Data..=) Prelude.<$> args,
            ("command" Data..=) Prelude.<$> command,
            ("env" Data..=) Prelude.<$> env,
            ("image" Data..=) Prelude.<$> image,
            ("resources" Data..=) Prelude.<$> resources
          ]
      )
