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
-- Module      : Network.AWS.Batch.Types.ResourceRequirement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ResourceRequirement where

import Network.AWS.Batch.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The type and amount of a resource to assign to a container. The
-- supported resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- /See:/ 'newResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { -- | The quantity of the specified resource to reserve for the container. The
    -- values vary based on the @type@ specified.
    --
    -- [type=\"GPU\"]
    --     The number of physical GPUs to reserve for the container. The number
    --     of GPUs reserved for all containers in a job shouldn\'t exceed the
    --     number of available GPUs on the compute resource that the job is
    --     launched on.
    --
    --     GPUs are not available for jobs running on Fargate resources.
    --
    -- [type=\"MEMORY\"]
    --     For jobs running on EC2 resources, the hard limit (in MiB) of memory
    --     to present to the container. If your container attempts to exceed
    --     the memory specified here, the container is killed. This parameter
    --     maps to @Memory@ in the
    --     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    --     section of the
    --     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
    --     the @--memory@ option to
    --     <https://docs.docker.com/engine/reference/run/ docker run>. You must
    --     specify at least 4 MiB of memory for a job. This is required but can
    --     be specified in several places for multi-node parallel (MNP) jobs.
    --     It must be specified for each node at least once. This parameter
    --     maps to @Memory@ in the
    --     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    --     section of the
    --     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
    --     the @--memory@ option to
    --     <https://docs.docker.com/engine/reference/run/ docker run>.
    --
    --     If you\'re trying to maximize your resource utilization by providing
    --     your jobs as much memory as possible for a particular instance type,
    --     see
    --     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
    --     in the /AWS Batch User Guide/.
    --
    --     For jobs running on Fargate resources, then @value@ is the hard
    --     limit (in MiB), and must match one of the supported values and the
    --     @VCPU@ values must be one of the values supported for that memory
    --     value.
    --
    --     [value = 512]
    --         @VCPU@ = 0.25
    --
    --     [value = 1024]
    --         @VCPU@ = 0.25 or 0.5
    --
    --     [value = 2048]
    --         @VCPU@ = 0.25, 0.5, or 1
    --
    --     [value = 3072]
    --         @VCPU@ = 0.5, or 1
    --
    --     [value = 4096]
    --         @VCPU@ = 0.5, 1, or 2
    --
    --     [value = 5120, 6144, or 7168]
    --         @VCPU@ = 1 or 2
    --
    --     [value = 8192]
    --         @VCPU@ = 1, 2, or 4
    --
    --     [value = 9216, 10240, 11264, 12288, 13312, 14336, 15360, or 16384]
    --         @VCPU@ = 2 or 4
    --
    --     [value = 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576, 25600, 26624, 27648, 28672, 29696, or 30720]
    --         @VCPU@ = 4
    --
    -- [type=\"VCPU\"]
    --     The number of vCPUs reserved for the container. This parameter maps
    --     to @CpuShares@ in the
    --     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
    --     section of the
    --     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
    --     the @--cpu-shares@ option to
    --     <https://docs.docker.com/engine/reference/run/ docker run>. Each
    --     vCPU is equivalent to 1,024 CPU shares. For EC2 resources, you must
    --     specify at least one vCPU. This is required but can be specified in
    --     several places; it must be specified for each node at least once.
    --
    --     For jobs running on Fargate resources, then @value@ must match one
    --     of the supported values and the @MEMORY@ values must be one of the
    --     values supported for that VCPU value. The supported values are 0.25,
    --     0.5, 1, 2, and 4
    --
    --     [value = 0.25]
    --         @MEMORY@ = 512, 1024, or 2048
    --
    --     [value = 0.5]
    --         @MEMORY@ = 1024, 2048, 3072, or 4096
    --
    --     [value = 1]
    --         @MEMORY@ = 2048, 3072, 4096, 5120, 6144, 7168, or 8192
    --
    --     [value = 2]
    --         @MEMORY@ = 4096, 5120, 6144, 7168, 8192, 9216, 10240, 11264,
    --         12288, 13312, 14336, 15360, or 16384
    --
    --     [value = 4]
    --         @MEMORY@ = 8192, 9216, 10240, 11264, 12288, 13312, 14336, 15360,
    --         16384, 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576,
    --         25600, 26624, 27648, 28672, 29696, or 30720
    value :: Prelude.Text,
    -- | The type of resource to assign to a container. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    type' :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceRequirement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceRequirement_value' - The quantity of the specified resource to reserve for the container. The
-- values vary based on the @type@ specified.
--
-- [type=\"GPU\"]
--     The number of physical GPUs to reserve for the container. The number
--     of GPUs reserved for all containers in a job shouldn\'t exceed the
--     number of available GPUs on the compute resource that the job is
--     launched on.
--
--     GPUs are not available for jobs running on Fargate resources.
--
-- [type=\"MEMORY\"]
--     For jobs running on EC2 resources, the hard limit (in MiB) of memory
--     to present to the container. If your container attempts to exceed
--     the memory specified here, the container is killed. This parameter
--     maps to @Memory@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--memory@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>. You must
--     specify at least 4 MiB of memory for a job. This is required but can
--     be specified in several places for multi-node parallel (MNP) jobs.
--     It must be specified for each node at least once. This parameter
--     maps to @Memory@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--memory@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>.
--
--     If you\'re trying to maximize your resource utilization by providing
--     your jobs as much memory as possible for a particular instance type,
--     see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
--     in the /AWS Batch User Guide/.
--
--     For jobs running on Fargate resources, then @value@ is the hard
--     limit (in MiB), and must match one of the supported values and the
--     @VCPU@ values must be one of the values supported for that memory
--     value.
--
--     [value = 512]
--         @VCPU@ = 0.25
--
--     [value = 1024]
--         @VCPU@ = 0.25 or 0.5
--
--     [value = 2048]
--         @VCPU@ = 0.25, 0.5, or 1
--
--     [value = 3072]
--         @VCPU@ = 0.5, or 1
--
--     [value = 4096]
--         @VCPU@ = 0.5, 1, or 2
--
--     [value = 5120, 6144, or 7168]
--         @VCPU@ = 1 or 2
--
--     [value = 8192]
--         @VCPU@ = 1, 2, or 4
--
--     [value = 9216, 10240, 11264, 12288, 13312, 14336, 15360, or 16384]
--         @VCPU@ = 2 or 4
--
--     [value = 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576, 25600, 26624, 27648, 28672, 29696, or 30720]
--         @VCPU@ = 4
--
-- [type=\"VCPU\"]
--     The number of vCPUs reserved for the container. This parameter maps
--     to @CpuShares@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--cpu-shares@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>. Each
--     vCPU is equivalent to 1,024 CPU shares. For EC2 resources, you must
--     specify at least one vCPU. This is required but can be specified in
--     several places; it must be specified for each node at least once.
--
--     For jobs running on Fargate resources, then @value@ must match one
--     of the supported values and the @MEMORY@ values must be one of the
--     values supported for that VCPU value. The supported values are 0.25,
--     0.5, 1, 2, and 4
--
--     [value = 0.25]
--         @MEMORY@ = 512, 1024, or 2048
--
--     [value = 0.5]
--         @MEMORY@ = 1024, 2048, 3072, or 4096
--
--     [value = 1]
--         @MEMORY@ = 2048, 3072, 4096, 5120, 6144, 7168, or 8192
--
--     [value = 2]
--         @MEMORY@ = 4096, 5120, 6144, 7168, 8192, 9216, 10240, 11264,
--         12288, 13312, 14336, 15360, or 16384
--
--     [value = 4]
--         @MEMORY@ = 8192, 9216, 10240, 11264, 12288, 13312, 14336, 15360,
--         16384, 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576,
--         25600, 26624, 27648, 28672, 29696, or 30720
--
-- 'type'', 'resourceRequirement_type' - The type of resource to assign to a container. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
newResourceRequirement ::
  -- | 'value'
  Prelude.Text ->
  -- | 'type''
  ResourceType ->
  ResourceRequirement
newResourceRequirement pValue_ pType_ =
  ResourceRequirement'
    { value = pValue_,
      type' = pType_
    }

-- | The quantity of the specified resource to reserve for the container. The
-- values vary based on the @type@ specified.
--
-- [type=\"GPU\"]
--     The number of physical GPUs to reserve for the container. The number
--     of GPUs reserved for all containers in a job shouldn\'t exceed the
--     number of available GPUs on the compute resource that the job is
--     launched on.
--
--     GPUs are not available for jobs running on Fargate resources.
--
-- [type=\"MEMORY\"]
--     For jobs running on EC2 resources, the hard limit (in MiB) of memory
--     to present to the container. If your container attempts to exceed
--     the memory specified here, the container is killed. This parameter
--     maps to @Memory@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--memory@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>. You must
--     specify at least 4 MiB of memory for a job. This is required but can
--     be specified in several places for multi-node parallel (MNP) jobs.
--     It must be specified for each node at least once. This parameter
--     maps to @Memory@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--memory@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>.
--
--     If you\'re trying to maximize your resource utilization by providing
--     your jobs as much memory as possible for a particular instance type,
--     see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory Management>
--     in the /AWS Batch User Guide/.
--
--     For jobs running on Fargate resources, then @value@ is the hard
--     limit (in MiB), and must match one of the supported values and the
--     @VCPU@ values must be one of the values supported for that memory
--     value.
--
--     [value = 512]
--         @VCPU@ = 0.25
--
--     [value = 1024]
--         @VCPU@ = 0.25 or 0.5
--
--     [value = 2048]
--         @VCPU@ = 0.25, 0.5, or 1
--
--     [value = 3072]
--         @VCPU@ = 0.5, or 1
--
--     [value = 4096]
--         @VCPU@ = 0.5, 1, or 2
--
--     [value = 5120, 6144, or 7168]
--         @VCPU@ = 1 or 2
--
--     [value = 8192]
--         @VCPU@ = 1, 2, or 4
--
--     [value = 9216, 10240, 11264, 12288, 13312, 14336, 15360, or 16384]
--         @VCPU@ = 2 or 4
--
--     [value = 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576, 25600, 26624, 27648, 28672, 29696, or 30720]
--         @VCPU@ = 4
--
-- [type=\"VCPU\"]
--     The number of vCPUs reserved for the container. This parameter maps
--     to @CpuShares@ in the
--     <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container>
--     section of the
--     <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and
--     the @--cpu-shares@ option to
--     <https://docs.docker.com/engine/reference/run/ docker run>. Each
--     vCPU is equivalent to 1,024 CPU shares. For EC2 resources, you must
--     specify at least one vCPU. This is required but can be specified in
--     several places; it must be specified for each node at least once.
--
--     For jobs running on Fargate resources, then @value@ must match one
--     of the supported values and the @MEMORY@ values must be one of the
--     values supported for that VCPU value. The supported values are 0.25,
--     0.5, 1, 2, and 4
--
--     [value = 0.25]
--         @MEMORY@ = 512, 1024, or 2048
--
--     [value = 0.5]
--         @MEMORY@ = 1024, 2048, 3072, or 4096
--
--     [value = 1]
--         @MEMORY@ = 2048, 3072, 4096, 5120, 6144, 7168, or 8192
--
--     [value = 2]
--         @MEMORY@ = 4096, 5120, 6144, 7168, 8192, 9216, 10240, 11264,
--         12288, 13312, 14336, 15360, or 16384
--
--     [value = 4]
--         @MEMORY@ = 8192, 9216, 10240, 11264, 12288, 13312, 14336, 15360,
--         16384, 17408, 18432, 19456, 20480, 21504, 22528, 23552, 24576,
--         25600, 26624, 27648, 28672, 29696, or 30720
resourceRequirement_value :: Lens.Lens' ResourceRequirement Prelude.Text
resourceRequirement_value = Lens.lens (\ResourceRequirement' {value} -> value) (\s@ResourceRequirement' {} a -> s {value = a} :: ResourceRequirement)

-- | The type of resource to assign to a container. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
resourceRequirement_type :: Lens.Lens' ResourceRequirement ResourceType
resourceRequirement_type = Lens.lens (\ResourceRequirement' {type'} -> type') (\s@ResourceRequirement' {} a -> s {type' = a} :: ResourceRequirement)

instance Prelude.FromJSON ResourceRequirement where
  parseJSON =
    Prelude.withObject
      "ResourceRequirement"
      ( \x ->
          ResourceRequirement'
            Prelude.<$> (x Prelude..: "value")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable ResourceRequirement

instance Prelude.NFData ResourceRequirement

instance Prelude.ToJSON ResourceRequirement where
  toJSON ResourceRequirement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Prelude..= value),
            Prelude.Just ("type" Prelude..= type')
          ]
      )
