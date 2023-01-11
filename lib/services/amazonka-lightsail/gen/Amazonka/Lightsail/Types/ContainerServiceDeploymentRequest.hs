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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceDeploymentRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceDeploymentRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.Container
import Amazonka.Lightsail.Types.EndpointRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail
-- container service.
--
-- A deployment specifies the settings, such as the ports and launch
-- command, of containers that are deployed to your container service.
--
-- /See:/ 'newContainerServiceDeploymentRequest' smart constructor.
data ContainerServiceDeploymentRequest = ContainerServiceDeploymentRequest'
  { -- | An object that describes the configuration for the containers of the
    -- deployment.
    containers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Container),
    -- | An object that describes the endpoint of the deployment.
    publicEndpoint :: Prelude.Maybe EndpointRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceDeploymentRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'containerServiceDeploymentRequest_containers' - An object that describes the configuration for the containers of the
-- deployment.
--
-- 'publicEndpoint', 'containerServiceDeploymentRequest_publicEndpoint' - An object that describes the endpoint of the deployment.
newContainerServiceDeploymentRequest ::
  ContainerServiceDeploymentRequest
newContainerServiceDeploymentRequest =
  ContainerServiceDeploymentRequest'
    { containers =
        Prelude.Nothing,
      publicEndpoint = Prelude.Nothing
    }

-- | An object that describes the configuration for the containers of the
-- deployment.
containerServiceDeploymentRequest_containers :: Lens.Lens' ContainerServiceDeploymentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Container))
containerServiceDeploymentRequest_containers = Lens.lens (\ContainerServiceDeploymentRequest' {containers} -> containers) (\s@ContainerServiceDeploymentRequest' {} a -> s {containers = a} :: ContainerServiceDeploymentRequest) Prelude.. Lens.mapping Lens.coerced

-- | An object that describes the endpoint of the deployment.
containerServiceDeploymentRequest_publicEndpoint :: Lens.Lens' ContainerServiceDeploymentRequest (Prelude.Maybe EndpointRequest)
containerServiceDeploymentRequest_publicEndpoint = Lens.lens (\ContainerServiceDeploymentRequest' {publicEndpoint} -> publicEndpoint) (\s@ContainerServiceDeploymentRequest' {} a -> s {publicEndpoint = a} :: ContainerServiceDeploymentRequest)

instance
  Prelude.Hashable
    ContainerServiceDeploymentRequest
  where
  hashWithSalt
    _salt
    ContainerServiceDeploymentRequest' {..} =
      _salt `Prelude.hashWithSalt` containers
        `Prelude.hashWithSalt` publicEndpoint

instance
  Prelude.NFData
    ContainerServiceDeploymentRequest
  where
  rnf ContainerServiceDeploymentRequest' {..} =
    Prelude.rnf containers
      `Prelude.seq` Prelude.rnf publicEndpoint

instance
  Data.ToJSON
    ContainerServiceDeploymentRequest
  where
  toJSON ContainerServiceDeploymentRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containers" Data..=) Prelude.<$> containers,
            ("publicEndpoint" Data..=)
              Prelude.<$> publicEndpoint
          ]
      )
