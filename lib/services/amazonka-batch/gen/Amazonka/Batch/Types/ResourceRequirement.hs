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
-- Module      : Amazonka.Batch.Types.ResourceRequirement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ResourceRequirement where

import Amazonka.Batch.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type and amount of a resource to assign to a container. The
-- supported resources include @GPU@, @MEMORY@, and @VCPU@.
--
-- /See:/ 'newResourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { -- | The quantity of the specified resource to reserve for the container. The
    -- values vary based on the @type@ specified.
    --
    -- [type=\"GPU\"]
    --     The number of physical GPUs to reserve for the container. Make sure
    --     that the number of GPUs reserved for all containers in a job
    --     doesn\'t exceed the number of available GPUs on the compute resource
    --     that the job is launched on.
    --
    --     GPUs aren\'t available for jobs that are running on Fargate
    --     resources.
    --
    -- [type=\"MEMORY\"]
    --     The memory hard limit (in MiB) present to the container. This
    --     parameter is supported for jobs that are running on EC2 resources.
    --     If your container attempts to exceed the memory specified, the
    --     container is terminated. This parameter maps to @Memory@ in the
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
    --     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
    --     in the /Batch User Guide/.
    --
    --     For jobs that are running on Fargate resources, then @value@ is the
    --     hard limit (in MiB), and must match one of the supported values and
    --     the @VCPU@ values must be one of the values supported for that
    --     memory value.
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
    --         @VCPU@ = 1, 2, 4, or 8
    --
    --     [value = 9216, 10240, 11264, 12288, 13312, 14336, or 15360]
    --         @VCPU@ = 2 or 4
    --
    --     [value = 16384]
    --         @VCPU@ = 2, 4, or 8
    --
    --     [value = 17408, 18432, 19456, 21504, 22528, 23552, 25600, 26624, 27648, 29696, or 30720]
    --         @VCPU@ = 4
    --
    --     [value = 20480, 24576, or 28672]
    --         @VCPU@ = 4 or 8
    --
    --     [value = 36864, 45056, 53248, or 61440]
    --         @VCPU@ = 8
    --
    --     [value = 32768, 40960, 49152, or 57344]
    --         @VCPU@ = 8 or 16
    --
    --     [value = 65536, 73728, 81920, 90112, 98304, 106496, 114688, or 122880]
    --         @VCPU@ = 16
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
    --     The default for the Fargate On-Demand vCPU resource count quota is 6
    --     vCPUs. For more information about Fargate quotas, see
    --     <https://docs.aws.amazon.com/general/latest/gr/ecs-service.html#service-quotas-fargate Fargate quotas>
    --     in the /Amazon Web Services General Reference/.
    --
    --     For jobs that are running on Fargate resources, then @value@ must
    --     match one of the supported values and the @MEMORY@ values must be
    --     one of the values supported for that @VCPU@ value. The supported
    --     values are 0.25, 0.5, 1, 2, 4, 8, and 16
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
    --     [value = 8]
    --         @MEMORY@ = 16384, 20480, 24576, 28672, 32768, 36864, 40960,
    --         45056, 49152, 53248, 57344, or 61440
    --
    --     [value = 16]
    --         @MEMORY@ = 32768, 40960, 49152, 57344, 65536, 73728, 81920,
    --         90112, 98304, 106496, 114688, or 122880
    value :: Prelude.Text,
    -- | The type of resource to assign to a container. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    type' :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     The number of physical GPUs to reserve for the container. Make sure
--     that the number of GPUs reserved for all containers in a job
--     doesn\'t exceed the number of available GPUs on the compute resource
--     that the job is launched on.
--
--     GPUs aren\'t available for jobs that are running on Fargate
--     resources.
--
-- [type=\"MEMORY\"]
--     The memory hard limit (in MiB) present to the container. This
--     parameter is supported for jobs that are running on EC2 resources.
--     If your container attempts to exceed the memory specified, the
--     container is terminated. This parameter maps to @Memory@ in the
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
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
--     For jobs that are running on Fargate resources, then @value@ is the
--     hard limit (in MiB), and must match one of the supported values and
--     the @VCPU@ values must be one of the values supported for that
--     memory value.
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
--         @VCPU@ = 1, 2, 4, or 8
--
--     [value = 9216, 10240, 11264, 12288, 13312, 14336, or 15360]
--         @VCPU@ = 2 or 4
--
--     [value = 16384]
--         @VCPU@ = 2, 4, or 8
--
--     [value = 17408, 18432, 19456, 21504, 22528, 23552, 25600, 26624, 27648, 29696, or 30720]
--         @VCPU@ = 4
--
--     [value = 20480, 24576, or 28672]
--         @VCPU@ = 4 or 8
--
--     [value = 36864, 45056, 53248, or 61440]
--         @VCPU@ = 8
--
--     [value = 32768, 40960, 49152, or 57344]
--         @VCPU@ = 8 or 16
--
--     [value = 65536, 73728, 81920, 90112, 98304, 106496, 114688, or 122880]
--         @VCPU@ = 16
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
--     The default for the Fargate On-Demand vCPU resource count quota is 6
--     vCPUs. For more information about Fargate quotas, see
--     <https://docs.aws.amazon.com/general/latest/gr/ecs-service.html#service-quotas-fargate Fargate quotas>
--     in the /Amazon Web Services General Reference/.
--
--     For jobs that are running on Fargate resources, then @value@ must
--     match one of the supported values and the @MEMORY@ values must be
--     one of the values supported for that @VCPU@ value. The supported
--     values are 0.25, 0.5, 1, 2, 4, 8, and 16
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
--     [value = 8]
--         @MEMORY@ = 16384, 20480, 24576, 28672, 32768, 36864, 40960,
--         45056, 49152, 53248, 57344, or 61440
--
--     [value = 16]
--         @MEMORY@ = 32768, 40960, 49152, 57344, 65536, 73728, 81920,
--         90112, 98304, 106496, 114688, or 122880
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
--     The number of physical GPUs to reserve for the container. Make sure
--     that the number of GPUs reserved for all containers in a job
--     doesn\'t exceed the number of available GPUs on the compute resource
--     that the job is launched on.
--
--     GPUs aren\'t available for jobs that are running on Fargate
--     resources.
--
-- [type=\"MEMORY\"]
--     The memory hard limit (in MiB) present to the container. This
--     parameter is supported for jobs that are running on EC2 resources.
--     If your container attempts to exceed the memory specified, the
--     container is terminated. This parameter maps to @Memory@ in the
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
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
--     For jobs that are running on Fargate resources, then @value@ is the
--     hard limit (in MiB), and must match one of the supported values and
--     the @VCPU@ values must be one of the values supported for that
--     memory value.
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
--         @VCPU@ = 1, 2, 4, or 8
--
--     [value = 9216, 10240, 11264, 12288, 13312, 14336, or 15360]
--         @VCPU@ = 2 or 4
--
--     [value = 16384]
--         @VCPU@ = 2, 4, or 8
--
--     [value = 17408, 18432, 19456, 21504, 22528, 23552, 25600, 26624, 27648, 29696, or 30720]
--         @VCPU@ = 4
--
--     [value = 20480, 24576, or 28672]
--         @VCPU@ = 4 or 8
--
--     [value = 36864, 45056, 53248, or 61440]
--         @VCPU@ = 8
--
--     [value = 32768, 40960, 49152, or 57344]
--         @VCPU@ = 8 or 16
--
--     [value = 65536, 73728, 81920, 90112, 98304, 106496, 114688, or 122880]
--         @VCPU@ = 16
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
--     The default for the Fargate On-Demand vCPU resource count quota is 6
--     vCPUs. For more information about Fargate quotas, see
--     <https://docs.aws.amazon.com/general/latest/gr/ecs-service.html#service-quotas-fargate Fargate quotas>
--     in the /Amazon Web Services General Reference/.
--
--     For jobs that are running on Fargate resources, then @value@ must
--     match one of the supported values and the @MEMORY@ values must be
--     one of the values supported for that @VCPU@ value. The supported
--     values are 0.25, 0.5, 1, 2, 4, 8, and 16
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
--     [value = 8]
--         @MEMORY@ = 16384, 20480, 24576, 28672, 32768, 36864, 40960,
--         45056, 49152, 53248, 57344, or 61440
--
--     [value = 16]
--         @MEMORY@ = 32768, 40960, 49152, 57344, 65536, 73728, 81920,
--         90112, 98304, 106496, 114688, or 122880
resourceRequirement_value :: Lens.Lens' ResourceRequirement Prelude.Text
resourceRequirement_value = Lens.lens (\ResourceRequirement' {value} -> value) (\s@ResourceRequirement' {} a -> s {value = a} :: ResourceRequirement)

-- | The type of resource to assign to a container. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
resourceRequirement_type :: Lens.Lens' ResourceRequirement ResourceType
resourceRequirement_type = Lens.lens (\ResourceRequirement' {type'} -> type') (\s@ResourceRequirement' {} a -> s {type' = a} :: ResourceRequirement)

instance Data.FromJSON ResourceRequirement where
  parseJSON =
    Data.withObject
      "ResourceRequirement"
      ( \x ->
          ResourceRequirement'
            Prelude.<$> (x Data..: "value")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ResourceRequirement where
  hashWithSalt _salt ResourceRequirement' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ResourceRequirement where
  rnf ResourceRequirement' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ResourceRequirement where
  toJSON ResourceRequirement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("value" Data..= value),
            Prelude.Just ("type" Data..= type')
          ]
      )
