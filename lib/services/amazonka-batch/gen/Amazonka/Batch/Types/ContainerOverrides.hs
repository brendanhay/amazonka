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
-- Module      : Amazonka.Batch.Types.ContainerOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ContainerOverrides where

import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.ResourceRequirement
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The overrides that should be sent to a container.
--
-- /See:/ 'newContainerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { -- | The command to send to the container that overrides the default command
    -- from the Docker image or the job definition.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the job definition.
    --
    -- Environment variables cannot start with \"@AWS_BATCH@\". This naming
    -- convention is reserved for variables that Batch sets.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The instance type to use for a multi-node parallel job.
    --
    -- This parameter isn\'t applicable to single-node container jobs or jobs
    -- that run on Fargate resources, and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | This parameter is deprecated, use @resourceRequirements@ to override the
    -- memory requirements specified in the job definition. It\'s not supported
    -- for jobs running on Fargate resources. For jobs that run on EC2
    -- resources, it overrides the @memory@ parameter set in the job
    -- definition, but doesn\'t override any memory requirement that\'s
    -- specified in the @resourceRequirements@ structure in the job definition.
    -- To override memory requirements that are specified in the
    -- @resourceRequirements@ structure in the job definition,
    -- @resourceRequirements@ must be specified in the @SubmitJob@ request,
    -- with @type@ set to @MEMORY@ and @value@ set to the new value. For more
    -- information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
    -- in the /Batch User Guide/.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The type and amount of resources to assign to a container. This
    -- overrides the settings in the job definition. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | This parameter is deprecated, use @resourceRequirements@ to override the
    -- @vcpus@ parameter that\'s set in the job definition. It\'s not supported
    -- for jobs running on Fargate resources. For jobs that run on EC2
    -- resources, it overrides the @vcpus@ parameter set in the job definition,
    -- but doesn\'t override any vCPU requirement specified in the
    -- @resourceRequirements@ structure in the job definition. To override vCPU
    -- requirements that are specified in the @resourceRequirements@ structure
    -- in the job definition, @resourceRequirements@ must be specified in the
    -- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
    -- new value. For more information, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
    -- in the /Batch User Guide/.
    vcpus :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'containerOverrides_command' - The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
--
-- 'environment', 'containerOverrides_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'instanceType', 'containerOverrides_instanceType' - The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
--
-- 'memory', 'containerOverrides_memory' - This parameter is deprecated, use @resourceRequirements@ to override the
-- memory requirements specified in the job definition. It\'s not supported
-- for jobs running on Fargate resources. For jobs that run on EC2
-- resources, it overrides the @memory@ parameter set in the job
-- definition, but doesn\'t override any memory requirement that\'s
-- specified in the @resourceRequirements@ structure in the job definition.
-- To override memory requirements that are specified in the
-- @resourceRequirements@ structure in the job definition,
-- @resourceRequirements@ must be specified in the @SubmitJob@ request,
-- with @type@ set to @MEMORY@ and @value@ set to the new value. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
-- in the /Batch User Guide/.
--
-- 'resourceRequirements', 'containerOverrides_resourceRequirements' - The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'vcpus', 'containerOverrides_vcpus' - This parameter is deprecated, use @resourceRequirements@ to override the
-- @vcpus@ parameter that\'s set in the job definition. It\'s not supported
-- for jobs running on Fargate resources. For jobs that run on EC2
-- resources, it overrides the @vcpus@ parameter set in the job definition,
-- but doesn\'t override any vCPU requirement specified in the
-- @resourceRequirements@ structure in the job definition. To override vCPU
-- requirements that are specified in the @resourceRequirements@ structure
-- in the job definition, @resourceRequirements@ must be specified in the
-- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
-- new value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
-- in the /Batch User Guide/.
newContainerOverrides ::
  ContainerOverrides
newContainerOverrides =
  ContainerOverrides'
    { command = Prelude.Nothing,
      environment = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      memory = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      vcpus = Prelude.Nothing
    }

-- | The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
containerOverrides_command :: Lens.Lens' ContainerOverrides (Prelude.Maybe [Prelude.Text])
containerOverrides_command = Lens.lens (\ContainerOverrides' {command} -> command) (\s@ContainerOverrides' {} a -> s {command = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables cannot start with \"@AWS_BATCH@\". This naming
-- convention is reserved for variables that Batch sets.
containerOverrides_environment :: Lens.Lens' ContainerOverrides (Prelude.Maybe [KeyValuePair])
containerOverrides_environment = Lens.lens (\ContainerOverrides' {environment} -> environment) (\s@ContainerOverrides' {} a -> s {environment = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
containerOverrides_instanceType :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Text)
containerOverrides_instanceType = Lens.lens (\ContainerOverrides' {instanceType} -> instanceType) (\s@ContainerOverrides' {} a -> s {instanceType = a} :: ContainerOverrides)

-- | This parameter is deprecated, use @resourceRequirements@ to override the
-- memory requirements specified in the job definition. It\'s not supported
-- for jobs running on Fargate resources. For jobs that run on EC2
-- resources, it overrides the @memory@ parameter set in the job
-- definition, but doesn\'t override any memory requirement that\'s
-- specified in the @resourceRequirements@ structure in the job definition.
-- To override memory requirements that are specified in the
-- @resourceRequirements@ structure in the job definition,
-- @resourceRequirements@ must be specified in the @SubmitJob@ request,
-- with @type@ set to @MEMORY@ and @value@ set to the new value. For more
-- information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
-- in the /Batch User Guide/.
containerOverrides_memory :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_memory = Lens.lens (\ContainerOverrides' {memory} -> memory) (\s@ContainerOverrides' {} a -> s {memory = a} :: ContainerOverrides)

-- | The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
containerOverrides_resourceRequirements :: Lens.Lens' ContainerOverrides (Prelude.Maybe [ResourceRequirement])
containerOverrides_resourceRequirements = Lens.lens (\ContainerOverrides' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverrides' {} a -> s {resourceRequirements = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | This parameter is deprecated, use @resourceRequirements@ to override the
-- @vcpus@ parameter that\'s set in the job definition. It\'s not supported
-- for jobs running on Fargate resources. For jobs that run on EC2
-- resources, it overrides the @vcpus@ parameter set in the job definition,
-- but doesn\'t override any vCPU requirement specified in the
-- @resourceRequirements@ structure in the job definition. To override vCPU
-- requirements that are specified in the @resourceRequirements@ structure
-- in the job definition, @resourceRequirements@ must be specified in the
-- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
-- new value. For more information, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#override-resource-requirements Can\'t override job definition resource requirements>
-- in the /Batch User Guide/.
containerOverrides_vcpus :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_vcpus = Lens.lens (\ContainerOverrides' {vcpus} -> vcpus) (\s@ContainerOverrides' {} a -> s {vcpus = a} :: ContainerOverrides)

instance Prelude.Hashable ContainerOverrides where
  hashWithSalt _salt ContainerOverrides' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` vcpus

instance Prelude.NFData ContainerOverrides where
  rnf ContainerOverrides' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf resourceRequirements
      `Prelude.seq` Prelude.rnf vcpus

instance Data.ToJSON ContainerOverrides where
  toJSON ContainerOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("command" Data..=) Prelude.<$> command,
            ("environment" Data..=) Prelude.<$> environment,
            ("instanceType" Data..=) Prelude.<$> instanceType,
            ("memory" Data..=) Prelude.<$> memory,
            ("resourceRequirements" Data..=)
              Prelude.<$> resourceRequirements,
            ("vcpus" Data..=) Prelude.<$> vcpus
          ]
      )
