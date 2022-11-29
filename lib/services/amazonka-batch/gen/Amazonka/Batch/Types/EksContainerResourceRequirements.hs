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
-- Module      : Amazonka.Batch.Types.EksContainerResourceRequirements
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerResourceRequirements where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The type and amount of resources to assign to a container. The supported
-- resources include @memory@, @cpu@, and @nvidia.com\/gpu@. For more
-- information, see
-- <https://kubernetes.io/docs/concepts/configuration/manage-resources-containers/ Resource management for pods and containers>
-- in the /Kubernetes documentation/.
--
-- /See:/ 'newEksContainerResourceRequirements' smart constructor.
data EksContainerResourceRequirements = EksContainerResourceRequirements'
  { -- | The type and quantity of the resources to reserve for the container. The
    -- values vary based on the @name@ that\'s specified. Resources can be
    -- requested using either the @limits@ or the @requests@ objects.
    --
    -- [memory]
    --     The memory hard limit (in MiB) for the container, using whole
    --     integers, with a \"Mi\" suffix. If your container attempts to exceed
    --     the memory specified, the container is terminated. You must specify
    --     at least 4 MiB of memory for a job. @memory@ can be specified in
    --     @limits@, @requests@, or both. If @memory@ is specified in both
    --     places, then the value that\'s specified in @limits@ must be equal
    --     to the value that\'s specified in @requests@.
    --
    --     To maximize your resource utilization, provide your jobs with as
    --     much memory as possible for the specific instance type that you are
    --     using. To learn how, see
    --     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
    --     in the /Batch User Guide/.
    --
    -- [cpu]
    --     The number of CPUs that\'s reserved for the container. Values must
    --     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
    --     @requests@, or both. If @cpu@ is specified in both places, then the
    --     value that\'s specified in @limits@ must be at least as large as the
    --     value that\'s specified in @requests@.
    --
    -- [nvidia.com\/gpu]
    --     The number of GPUs that\'s reserved for the container. Values must
    --     be a whole integer. @memory@ can be specified in @limits@,
    --     @requests@, or both. If @memory@ is specified in both places, then
    --     the value that\'s specified in @limits@ must be equal to the value
    --     that\'s specified in @requests@.
    limits :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type and quantity of the resources to request for the container. The
    -- values vary based on the @name@ that\'s specified. Resources can be
    -- requested by using either the @limits@ or the @requests@ objects.
    --
    -- [memory]
    --     The memory hard limit (in MiB) for the container, using whole
    --     integers, with a \"Mi\" suffix. If your container attempts to exceed
    --     the memory specified, the container is terminated. You must specify
    --     at least 4 MiB of memory for a job. @memory@ can be specified in
    --     @limits@, @requests@, or both. If @memory@ is specified in both,
    --     then the value that\'s specified in @limits@ must be equal to the
    --     value that\'s specified in @requests@.
    --
    --     If you\'re trying to maximize your resource utilization by providing
    --     your jobs as much memory as possible for a particular instance type,
    --     see
    --     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
    --     in the /Batch User Guide/.
    --
    -- [cpu]
    --     The number of CPUs that are reserved for the container. Values must
    --     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
    --     @requests@, or both. If @cpu@ is specified in both, then the value
    --     that\'s specified in @limits@ must be at least as large as the value
    --     that\'s specified in @requests@.
    --
    -- [nvidia.com\/gpu]
    --     The number of GPUs that are reserved for the container. Values must
    --     be a whole integer. @nvidia.com\/gpu@ can be specified in @limits@,
    --     @requests@, or both. If @nvidia.com\/gpu@ is specified in both, then
    --     the value that\'s specified in @limits@ must be equal to the value
    --     that\'s specified in @requests@.
    requests :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerResourceRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limits', 'eksContainerResourceRequirements_limits' - The type and quantity of the resources to reserve for the container. The
-- values vary based on the @name@ that\'s specified. Resources can be
-- requested using either the @limits@ or the @requests@ objects.
--
-- [memory]
--     The memory hard limit (in MiB) for the container, using whole
--     integers, with a \"Mi\" suffix. If your container attempts to exceed
--     the memory specified, the container is terminated. You must specify
--     at least 4 MiB of memory for a job. @memory@ can be specified in
--     @limits@, @requests@, or both. If @memory@ is specified in both
--     places, then the value that\'s specified in @limits@ must be equal
--     to the value that\'s specified in @requests@.
--
--     To maximize your resource utilization, provide your jobs with as
--     much memory as possible for the specific instance type that you are
--     using. To learn how, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
-- [cpu]
--     The number of CPUs that\'s reserved for the container. Values must
--     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
--     @requests@, or both. If @cpu@ is specified in both places, then the
--     value that\'s specified in @limits@ must be at least as large as the
--     value that\'s specified in @requests@.
--
-- [nvidia.com\/gpu]
--     The number of GPUs that\'s reserved for the container. Values must
--     be a whole integer. @memory@ can be specified in @limits@,
--     @requests@, or both. If @memory@ is specified in both places, then
--     the value that\'s specified in @limits@ must be equal to the value
--     that\'s specified in @requests@.
--
-- 'requests', 'eksContainerResourceRequirements_requests' - The type and quantity of the resources to request for the container. The
-- values vary based on the @name@ that\'s specified. Resources can be
-- requested by using either the @limits@ or the @requests@ objects.
--
-- [memory]
--     The memory hard limit (in MiB) for the container, using whole
--     integers, with a \"Mi\" suffix. If your container attempts to exceed
--     the memory specified, the container is terminated. You must specify
--     at least 4 MiB of memory for a job. @memory@ can be specified in
--     @limits@, @requests@, or both. If @memory@ is specified in both,
--     then the value that\'s specified in @limits@ must be equal to the
--     value that\'s specified in @requests@.
--
--     If you\'re trying to maximize your resource utilization by providing
--     your jobs as much memory as possible for a particular instance type,
--     see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
-- [cpu]
--     The number of CPUs that are reserved for the container. Values must
--     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
--     @requests@, or both. If @cpu@ is specified in both, then the value
--     that\'s specified in @limits@ must be at least as large as the value
--     that\'s specified in @requests@.
--
-- [nvidia.com\/gpu]
--     The number of GPUs that are reserved for the container. Values must
--     be a whole integer. @nvidia.com\/gpu@ can be specified in @limits@,
--     @requests@, or both. If @nvidia.com\/gpu@ is specified in both, then
--     the value that\'s specified in @limits@ must be equal to the value
--     that\'s specified in @requests@.
newEksContainerResourceRequirements ::
  EksContainerResourceRequirements
newEksContainerResourceRequirements =
  EksContainerResourceRequirements'
    { limits =
        Prelude.Nothing,
      requests = Prelude.Nothing
    }

-- | The type and quantity of the resources to reserve for the container. The
-- values vary based on the @name@ that\'s specified. Resources can be
-- requested using either the @limits@ or the @requests@ objects.
--
-- [memory]
--     The memory hard limit (in MiB) for the container, using whole
--     integers, with a \"Mi\" suffix. If your container attempts to exceed
--     the memory specified, the container is terminated. You must specify
--     at least 4 MiB of memory for a job. @memory@ can be specified in
--     @limits@, @requests@, or both. If @memory@ is specified in both
--     places, then the value that\'s specified in @limits@ must be equal
--     to the value that\'s specified in @requests@.
--
--     To maximize your resource utilization, provide your jobs with as
--     much memory as possible for the specific instance type that you are
--     using. To learn how, see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
-- [cpu]
--     The number of CPUs that\'s reserved for the container. Values must
--     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
--     @requests@, or both. If @cpu@ is specified in both places, then the
--     value that\'s specified in @limits@ must be at least as large as the
--     value that\'s specified in @requests@.
--
-- [nvidia.com\/gpu]
--     The number of GPUs that\'s reserved for the container. Values must
--     be a whole integer. @memory@ can be specified in @limits@,
--     @requests@, or both. If @memory@ is specified in both places, then
--     the value that\'s specified in @limits@ must be equal to the value
--     that\'s specified in @requests@.
eksContainerResourceRequirements_limits :: Lens.Lens' EksContainerResourceRequirements (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eksContainerResourceRequirements_limits = Lens.lens (\EksContainerResourceRequirements' {limits} -> limits) (\s@EksContainerResourceRequirements' {} a -> s {limits = a} :: EksContainerResourceRequirements) Prelude.. Lens.mapping Lens.coerced

-- | The type and quantity of the resources to request for the container. The
-- values vary based on the @name@ that\'s specified. Resources can be
-- requested by using either the @limits@ or the @requests@ objects.
--
-- [memory]
--     The memory hard limit (in MiB) for the container, using whole
--     integers, with a \"Mi\" suffix. If your container attempts to exceed
--     the memory specified, the container is terminated. You must specify
--     at least 4 MiB of memory for a job. @memory@ can be specified in
--     @limits@, @requests@, or both. If @memory@ is specified in both,
--     then the value that\'s specified in @limits@ must be equal to the
--     value that\'s specified in @requests@.
--
--     If you\'re trying to maximize your resource utilization by providing
--     your jobs as much memory as possible for a particular instance type,
--     see
--     <https://docs.aws.amazon.com/batch/latest/userguide/memory-management.html Memory management>
--     in the /Batch User Guide/.
--
-- [cpu]
--     The number of CPUs that are reserved for the container. Values must
--     be an even multiple of @0.25@. @cpu@ can be specified in @limits@,
--     @requests@, or both. If @cpu@ is specified in both, then the value
--     that\'s specified in @limits@ must be at least as large as the value
--     that\'s specified in @requests@.
--
-- [nvidia.com\/gpu]
--     The number of GPUs that are reserved for the container. Values must
--     be a whole integer. @nvidia.com\/gpu@ can be specified in @limits@,
--     @requests@, or both. If @nvidia.com\/gpu@ is specified in both, then
--     the value that\'s specified in @limits@ must be equal to the value
--     that\'s specified in @requests@.
eksContainerResourceRequirements_requests :: Lens.Lens' EksContainerResourceRequirements (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eksContainerResourceRequirements_requests = Lens.lens (\EksContainerResourceRequirements' {requests} -> requests) (\s@EksContainerResourceRequirements' {} a -> s {requests = a} :: EksContainerResourceRequirements) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    EksContainerResourceRequirements
  where
  parseJSON =
    Core.withObject
      "EksContainerResourceRequirements"
      ( \x ->
          EksContainerResourceRequirements'
            Prelude.<$> (x Core..:? "limits" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "requests" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    EksContainerResourceRequirements
  where
  hashWithSalt
    _salt
    EksContainerResourceRequirements' {..} =
      _salt `Prelude.hashWithSalt` limits
        `Prelude.hashWithSalt` requests

instance
  Prelude.NFData
    EksContainerResourceRequirements
  where
  rnf EksContainerResourceRequirements' {..} =
    Prelude.rnf limits
      `Prelude.seq` Prelude.rnf requests

instance Core.ToJSON EksContainerResourceRequirements where
  toJSON EksContainerResourceRequirements' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("limits" Core..=) Prelude.<$> limits,
            ("requests" Core..=) Prelude.<$> requests
          ]
      )
