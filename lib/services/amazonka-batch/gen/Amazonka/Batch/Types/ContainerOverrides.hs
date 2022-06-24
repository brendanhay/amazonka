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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ContainerOverrides where

import Amazonka.Batch.Types.KeyValuePair
import Amazonka.Batch.Types.ResourceRequirement
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The overrides that should be sent to a container.
--
-- /See:/ 'newContainerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the job definition.
    --
    -- Environment variables must not start with @AWS_BATCH@; this naming
    -- convention is reserved for variables that are set by the Batch service.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The type and amount of resources to assign to a container. This
    -- overrides the settings in the job definition. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | This parameter indicates the amount of memory (in MiB) that\'s reserved
    -- for the job. It overrides the @memory@ parameter set in the job
    -- definition, but doesn\'t override any memory requirement specified in
    -- the @ResourceRequirement@ structure in the job definition. To override
    -- memory requirements that are specified in the @ResourceRequirement@
    -- structure in the job definition, @ResourceRequirement@ must be specified
    -- in the @SubmitJob@ request, with @type@ set to @MEMORY@ and @value@ set
    -- to the new value.
    --
    -- This parameter is supported for jobs that run on EC2 resources, but
    -- isn\'t supported for jobs that run on Fargate resources. For these
    -- resources, use @resourceRequirement@ instead.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the job definition.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The instance type to use for a multi-node parallel job.
    --
    -- This parameter isn\'t applicable to single-node container jobs or jobs
    -- that run on Fargate resources, and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | This parameter indicates the number of vCPUs reserved for the
    -- container.It overrides the @vcpus@ parameter that\'s set in the job
    -- definition, but doesn\'t override any vCPU requirement specified in the
    -- @resourceRequirement@ structure in the job definition. To override vCPU
    -- requirements that are specified in the @ResourceRequirement@ structure
    -- in the job definition, @ResourceRequirement@ must be specified in the
    -- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
    -- new value.
    --
    -- This parameter maps to @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
    -- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
    --
    -- This parameter is supported for jobs that run on EC2 resources, but
    -- isn\'t supported for jobs that run on Fargate resources. For Fargate
    -- resources, you can only use @resourceRequirement@. For EC2 resources,
    -- you can use either this parameter or @resourceRequirement@ but not both.
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
-- 'environment', 'containerOverrides_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the Batch service.
--
-- 'resourceRequirements', 'containerOverrides_resourceRequirements' - The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
--
-- 'memory', 'containerOverrides_memory' - This parameter indicates the amount of memory (in MiB) that\'s reserved
-- for the job. It overrides the @memory@ parameter set in the job
-- definition, but doesn\'t override any memory requirement specified in
-- the @ResourceRequirement@ structure in the job definition. To override
-- memory requirements that are specified in the @ResourceRequirement@
-- structure in the job definition, @ResourceRequirement@ must be specified
-- in the @SubmitJob@ request, with @type@ set to @MEMORY@ and @value@ set
-- to the new value.
--
-- This parameter is supported for jobs that run on EC2 resources, but
-- isn\'t supported for jobs that run on Fargate resources. For these
-- resources, use @resourceRequirement@ instead.
--
-- 'command', 'containerOverrides_command' - The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
--
-- 'instanceType', 'containerOverrides_instanceType' - The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
--
-- 'vcpus', 'containerOverrides_vcpus' - This parameter indicates the number of vCPUs reserved for the
-- container.It overrides the @vcpus@ parameter that\'s set in the job
-- definition, but doesn\'t override any vCPU requirement specified in the
-- @resourceRequirement@ structure in the job definition. To override vCPU
-- requirements that are specified in the @ResourceRequirement@ structure
-- in the job definition, @ResourceRequirement@ must be specified in the
-- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
-- new value.
--
-- This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
--
-- This parameter is supported for jobs that run on EC2 resources, but
-- isn\'t supported for jobs that run on Fargate resources. For Fargate
-- resources, you can only use @resourceRequirement@. For EC2 resources,
-- you can use either this parameter or @resourceRequirement@ but not both.
newContainerOverrides ::
  ContainerOverrides
newContainerOverrides =
  ContainerOverrides'
    { environment = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      memory = Prelude.Nothing,
      command = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      vcpus = Prelude.Nothing
    }

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the Batch service.
containerOverrides_environment :: Lens.Lens' ContainerOverrides (Prelude.Maybe [KeyValuePair])
containerOverrides_environment = Lens.lens (\ContainerOverrides' {environment} -> environment) (\s@ContainerOverrides' {} a -> s {environment = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
containerOverrides_resourceRequirements :: Lens.Lens' ContainerOverrides (Prelude.Maybe [ResourceRequirement])
containerOverrides_resourceRequirements = Lens.lens (\ContainerOverrides' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverrides' {} a -> s {resourceRequirements = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | This parameter indicates the amount of memory (in MiB) that\'s reserved
-- for the job. It overrides the @memory@ parameter set in the job
-- definition, but doesn\'t override any memory requirement specified in
-- the @ResourceRequirement@ structure in the job definition. To override
-- memory requirements that are specified in the @ResourceRequirement@
-- structure in the job definition, @ResourceRequirement@ must be specified
-- in the @SubmitJob@ request, with @type@ set to @MEMORY@ and @value@ set
-- to the new value.
--
-- This parameter is supported for jobs that run on EC2 resources, but
-- isn\'t supported for jobs that run on Fargate resources. For these
-- resources, use @resourceRequirement@ instead.
containerOverrides_memory :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_memory = Lens.lens (\ContainerOverrides' {memory} -> memory) (\s@ContainerOverrides' {} a -> s {memory = a} :: ContainerOverrides)

-- | The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
containerOverrides_command :: Lens.Lens' ContainerOverrides (Prelude.Maybe [Prelude.Text])
containerOverrides_command = Lens.lens (\ContainerOverrides' {command} -> command) (\s@ContainerOverrides' {} a -> s {command = a} :: ContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
containerOverrides_instanceType :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Text)
containerOverrides_instanceType = Lens.lens (\ContainerOverrides' {instanceType} -> instanceType) (\s@ContainerOverrides' {} a -> s {instanceType = a} :: ContainerOverrides)

-- | This parameter indicates the number of vCPUs reserved for the
-- container.It overrides the @vcpus@ parameter that\'s set in the job
-- definition, but doesn\'t override any vCPU requirement specified in the
-- @resourceRequirement@ structure in the job definition. To override vCPU
-- requirements that are specified in the @ResourceRequirement@ structure
-- in the job definition, @ResourceRequirement@ must be specified in the
-- @SubmitJob@ request, with @type@ set to @VCPU@ and @value@ set to the
-- new value.
--
-- This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
--
-- This parameter is supported for jobs that run on EC2 resources, but
-- isn\'t supported for jobs that run on Fargate resources. For Fargate
-- resources, you can only use @resourceRequirement@. For EC2 resources,
-- you can use either this parameter or @resourceRequirement@ but not both.
containerOverrides_vcpus :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_vcpus = Lens.lens (\ContainerOverrides' {vcpus} -> vcpus) (\s@ContainerOverrides' {} a -> s {vcpus = a} :: ContainerOverrides)

instance Prelude.Hashable ContainerOverrides where
  hashWithSalt _salt ContainerOverrides' {..} =
    _salt `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` vcpus

instance Prelude.NFData ContainerOverrides where
  rnf ContainerOverrides' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf resourceRequirements
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf vcpus

instance Core.ToJSON ContainerOverrides where
  toJSON ContainerOverrides' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("environment" Core..=) Prelude.<$> environment,
            ("resourceRequirements" Core..=)
              Prelude.<$> resourceRequirements,
            ("memory" Core..=) Prelude.<$> memory,
            ("command" Core..=) Prelude.<$> command,
            ("instanceType" Core..=) Prelude.<$> instanceType,
            ("vcpus" Core..=) Prelude.<$> vcpus
          ]
      )
