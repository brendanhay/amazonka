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
-- Module      : Amazonka.ECS.Types.ContainerInstanceHealthStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerInstanceHealthStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.InstanceHealthCheckResult
import Amazonka.ECS.Types.InstanceHealthCheckState
import qualified Amazonka.Prelude as Prelude

-- | An object representing the health status of the container instance.
--
-- /See:/ 'newContainerInstanceHealthStatus' smart constructor.
data ContainerInstanceHealthStatus = ContainerInstanceHealthStatus'
  { -- | An array of objects representing the details of the container instance
    -- health status.
    details :: Prelude.Maybe [InstanceHealthCheckResult],
    -- | The overall health status of the container instance. This is an
    -- aggregate status of all container instance health checks.
    overallStatus :: Prelude.Maybe InstanceHealthCheckState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerInstanceHealthStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'containerInstanceHealthStatus_details' - An array of objects representing the details of the container instance
-- health status.
--
-- 'overallStatus', 'containerInstanceHealthStatus_overallStatus' - The overall health status of the container instance. This is an
-- aggregate status of all container instance health checks.
newContainerInstanceHealthStatus ::
  ContainerInstanceHealthStatus
newContainerInstanceHealthStatus =
  ContainerInstanceHealthStatus'
    { details =
        Prelude.Nothing,
      overallStatus = Prelude.Nothing
    }

-- | An array of objects representing the details of the container instance
-- health status.
containerInstanceHealthStatus_details :: Lens.Lens' ContainerInstanceHealthStatus (Prelude.Maybe [InstanceHealthCheckResult])
containerInstanceHealthStatus_details = Lens.lens (\ContainerInstanceHealthStatus' {details} -> details) (\s@ContainerInstanceHealthStatus' {} a -> s {details = a} :: ContainerInstanceHealthStatus) Prelude.. Lens.mapping Lens.coerced

-- | The overall health status of the container instance. This is an
-- aggregate status of all container instance health checks.
containerInstanceHealthStatus_overallStatus :: Lens.Lens' ContainerInstanceHealthStatus (Prelude.Maybe InstanceHealthCheckState)
containerInstanceHealthStatus_overallStatus = Lens.lens (\ContainerInstanceHealthStatus' {overallStatus} -> overallStatus) (\s@ContainerInstanceHealthStatus' {} a -> s {overallStatus = a} :: ContainerInstanceHealthStatus)

instance Data.FromJSON ContainerInstanceHealthStatus where
  parseJSON =
    Data.withObject
      "ContainerInstanceHealthStatus"
      ( \x ->
          ContainerInstanceHealthStatus'
            Prelude.<$> (x Data..:? "details" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "overallStatus")
      )

instance
  Prelude.Hashable
    ContainerInstanceHealthStatus
  where
  hashWithSalt _salt ContainerInstanceHealthStatus' {..} =
    _salt
      `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` overallStatus

instance Prelude.NFData ContainerInstanceHealthStatus where
  rnf ContainerInstanceHealthStatus' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf overallStatus
