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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ContainerServiceHealthCheckConfig
import qualified Amazonka.Prelude as Prelude

-- | Describes the public endpoint configuration of a deployment of an Amazon
-- Lightsail container service.
--
-- /See:/ 'newContainerServiceEndpoint' smart constructor.
data ContainerServiceEndpoint = ContainerServiceEndpoint'
  { -- | The port of the specified container to which traffic is forwarded to.
    containerPort :: Prelude.Maybe Prelude.Int,
    -- | An object that describes the health check configuration of the
    -- container.
    healthCheck :: Prelude.Maybe ContainerServiceHealthCheckConfig,
    -- | The name of the container entry of the deployment that the endpoint
    -- configuration applies to.
    containerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPort', 'containerServiceEndpoint_containerPort' - The port of the specified container to which traffic is forwarded to.
--
-- 'healthCheck', 'containerServiceEndpoint_healthCheck' - An object that describes the health check configuration of the
-- container.
--
-- 'containerName', 'containerServiceEndpoint_containerName' - The name of the container entry of the deployment that the endpoint
-- configuration applies to.
newContainerServiceEndpoint ::
  ContainerServiceEndpoint
newContainerServiceEndpoint =
  ContainerServiceEndpoint'
    { containerPort =
        Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      containerName = Prelude.Nothing
    }

-- | The port of the specified container to which traffic is forwarded to.
containerServiceEndpoint_containerPort :: Lens.Lens' ContainerServiceEndpoint (Prelude.Maybe Prelude.Int)
containerServiceEndpoint_containerPort = Lens.lens (\ContainerServiceEndpoint' {containerPort} -> containerPort) (\s@ContainerServiceEndpoint' {} a -> s {containerPort = a} :: ContainerServiceEndpoint)

-- | An object that describes the health check configuration of the
-- container.
containerServiceEndpoint_healthCheck :: Lens.Lens' ContainerServiceEndpoint (Prelude.Maybe ContainerServiceHealthCheckConfig)
containerServiceEndpoint_healthCheck = Lens.lens (\ContainerServiceEndpoint' {healthCheck} -> healthCheck) (\s@ContainerServiceEndpoint' {} a -> s {healthCheck = a} :: ContainerServiceEndpoint)

-- | The name of the container entry of the deployment that the endpoint
-- configuration applies to.
containerServiceEndpoint_containerName :: Lens.Lens' ContainerServiceEndpoint (Prelude.Maybe Prelude.Text)
containerServiceEndpoint_containerName = Lens.lens (\ContainerServiceEndpoint' {containerName} -> containerName) (\s@ContainerServiceEndpoint' {} a -> s {containerName = a} :: ContainerServiceEndpoint)

instance Data.FromJSON ContainerServiceEndpoint where
  parseJSON =
    Data.withObject
      "ContainerServiceEndpoint"
      ( \x ->
          ContainerServiceEndpoint'
            Prelude.<$> (x Data..:? "containerPort")
            Prelude.<*> (x Data..:? "healthCheck")
            Prelude.<*> (x Data..:? "containerName")
      )

instance Prelude.Hashable ContainerServiceEndpoint where
  hashWithSalt _salt ContainerServiceEndpoint' {..} =
    _salt `Prelude.hashWithSalt` containerPort
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` containerName

instance Prelude.NFData ContainerServiceEndpoint where
  rnf ContainerServiceEndpoint' {..} =
    Prelude.rnf containerPort
      `Prelude.seq` Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf containerName
