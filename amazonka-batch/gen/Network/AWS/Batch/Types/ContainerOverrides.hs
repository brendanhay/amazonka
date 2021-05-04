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
-- Module      : Network.AWS.Batch.Types.ContainerOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerOverrides where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.ResourceRequirement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The overrides that should be sent to a container.
--
-- /See:/ 'newContainerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { -- | This parameter is deprecated and not supported for jobs run on Fargate
    -- resources, use @ResourceRequirement@. For jobs run on EC2 resource, the
    -- number of MiB of memory reserved for the job. This value overrides the
    -- value set in the job definition.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The instance type to use for a multi-node parallel job.
    --
    -- This parameter isn\'t applicable to single-node container jobs or for
    -- jobs running on Fargate resources and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | This parameter is deprecated and not supported for jobs run on Fargate
    -- resources, see @resourceRequirement@. For jobs run on EC2 resources, the
    -- number of vCPUs to reserve for the container. This value overrides the
    -- value set in the job definition. Jobs run on EC2 resources can specify
    -- the vCPU requirement using @resourceRequirement@ but the vCPU
    -- requirements can\'t be specified both here and in @resourceRequirement@.
    -- This parameter maps to @CpuShares@ in the
    -- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
    -- @--cpu-shares@ option to
    -- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
    -- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
    --
    -- This parameter isn\'t applicable to jobs running on Fargate resources
    -- and shouldn\'t be provided. Jobs running on Fargate resources must
    -- specify the vCPU requirement for the job using @resourceRequirements@.
    vcpus :: Prelude.Maybe Prelude.Int,
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the job definition.
    --
    -- Environment variables must not start with @AWS_BATCH@; this naming
    -- convention is reserved for variables that are set by the AWS Batch
    -- service.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the job definition.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The type and amount of resources to assign to a container. This
    -- overrides the settings in the job definition. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memory', 'containerOverrides_memory' - This parameter is deprecated and not supported for jobs run on Fargate
-- resources, use @ResourceRequirement@. For jobs run on EC2 resource, the
-- number of MiB of memory reserved for the job. This value overrides the
-- value set in the job definition.
--
-- 'instanceType', 'containerOverrides_instanceType' - The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or for
-- jobs running on Fargate resources and shouldn\'t be provided.
--
-- 'vcpus', 'containerOverrides_vcpus' - This parameter is deprecated and not supported for jobs run on Fargate
-- resources, see @resourceRequirement@. For jobs run on EC2 resources, the
-- number of vCPUs to reserve for the container. This value overrides the
-- value set in the job definition. Jobs run on EC2 resources can specify
-- the vCPU requirement using @resourceRequirement@ but the vCPU
-- requirements can\'t be specified both here and in @resourceRequirement@.
-- This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided. Jobs running on Fargate resources must
-- specify the vCPU requirement for the job using @resourceRequirements@.
--
-- 'environment', 'containerOverrides_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
--
-- 'command', 'containerOverrides_command' - The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
--
-- 'resourceRequirements', 'containerOverrides_resourceRequirements' - The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
newContainerOverrides ::
  ContainerOverrides
newContainerOverrides =
  ContainerOverrides'
    { memory = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      vcpus = Prelude.Nothing,
      environment = Prelude.Nothing,
      command = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing
    }

-- | This parameter is deprecated and not supported for jobs run on Fargate
-- resources, use @ResourceRequirement@. For jobs run on EC2 resource, the
-- number of MiB of memory reserved for the job. This value overrides the
-- value set in the job definition.
containerOverrides_memory :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_memory = Lens.lens (\ContainerOverrides' {memory} -> memory) (\s@ContainerOverrides' {} a -> s {memory = a} :: ContainerOverrides)

-- | The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or for
-- jobs running on Fargate resources and shouldn\'t be provided.
containerOverrides_instanceType :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Text)
containerOverrides_instanceType = Lens.lens (\ContainerOverrides' {instanceType} -> instanceType) (\s@ContainerOverrides' {} a -> s {instanceType = a} :: ContainerOverrides)

-- | This parameter is deprecated and not supported for jobs run on Fargate
-- resources, see @resourceRequirement@. For jobs run on EC2 resources, the
-- number of vCPUs to reserve for the container. This value overrides the
-- value set in the job definition. Jobs run on EC2 resources can specify
-- the vCPU requirement using @resourceRequirement@ but the vCPU
-- requirements can\'t be specified both here and in @resourceRequirement@.
-- This parameter maps to @CpuShares@ in the
-- <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
-- section of the
-- <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the
-- @--cpu-shares@ option to
-- <https://docs.docker.com/engine/reference/run/ docker run>. Each vCPU is
-- equivalent to 1,024 CPU shares. You must specify at least one vCPU.
--
-- This parameter isn\'t applicable to jobs running on Fargate resources
-- and shouldn\'t be provided. Jobs running on Fargate resources must
-- specify the vCPU requirement for the job using @resourceRequirements@.
containerOverrides_vcpus :: Lens.Lens' ContainerOverrides (Prelude.Maybe Prelude.Int)
containerOverrides_vcpus = Lens.lens (\ContainerOverrides' {vcpus} -> vcpus) (\s@ContainerOverrides' {} a -> s {vcpus = a} :: ContainerOverrides)

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the job definition.
--
-- Environment variables must not start with @AWS_BATCH@; this naming
-- convention is reserved for variables that are set by the AWS Batch
-- service.
containerOverrides_environment :: Lens.Lens' ContainerOverrides (Prelude.Maybe [KeyValuePair])
containerOverrides_environment = Lens.lens (\ContainerOverrides' {environment} -> environment) (\s@ContainerOverrides' {} a -> s {environment = a} :: ContainerOverrides) Prelude.. Lens.mapping Prelude._Coerce

-- | The command to send to the container that overrides the default command
-- from the Docker image or the job definition.
containerOverrides_command :: Lens.Lens' ContainerOverrides (Prelude.Maybe [Prelude.Text])
containerOverrides_command = Lens.lens (\ContainerOverrides' {command} -> command) (\s@ContainerOverrides' {} a -> s {command = a} :: ContainerOverrides) Prelude.. Lens.mapping Prelude._Coerce

-- | The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
containerOverrides_resourceRequirements :: Lens.Lens' ContainerOverrides (Prelude.Maybe [ResourceRequirement])
containerOverrides_resourceRequirements = Lens.lens (\ContainerOverrides' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverrides' {} a -> s {resourceRequirements = a} :: ContainerOverrides) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable ContainerOverrides

instance Prelude.NFData ContainerOverrides

instance Prelude.ToJSON ContainerOverrides where
  toJSON ContainerOverrides' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("memory" Prelude..=) Prelude.<$> memory,
            ("instanceType" Prelude..=) Prelude.<$> instanceType,
            ("vcpus" Prelude..=) Prelude.<$> vcpus,
            ("environment" Prelude..=) Prelude.<$> environment,
            ("command" Prelude..=) Prelude.<$> command,
            ("resourceRequirements" Prelude..=)
              Prelude.<$> resourceRequirements
          ]
      )
