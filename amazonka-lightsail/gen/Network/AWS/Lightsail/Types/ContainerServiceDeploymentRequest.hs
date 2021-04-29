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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.EndpointRequest
import qualified Network.AWS.Prelude as Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail
-- container service.
--
-- A deployment specifies the settings, such as the ports and launch
-- command, of containers that are deployed to your container service.
--
-- /See:/ 'newContainerServiceDeploymentRequest' smart constructor.
data ContainerServiceDeploymentRequest = ContainerServiceDeploymentRequest'
  { -- | An object that describes the endpoint of the deployment.
    publicEndpoint :: Prelude.Maybe EndpointRequest,
    -- | An object that describes the configuration for the containers of the
    -- deployment.
    containers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Container)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceDeploymentRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicEndpoint', 'containerServiceDeploymentRequest_publicEndpoint' - An object that describes the endpoint of the deployment.
--
-- 'containers', 'containerServiceDeploymentRequest_containers' - An object that describes the configuration for the containers of the
-- deployment.
newContainerServiceDeploymentRequest ::
  ContainerServiceDeploymentRequest
newContainerServiceDeploymentRequest =
  ContainerServiceDeploymentRequest'
    { publicEndpoint =
        Prelude.Nothing,
      containers = Prelude.Nothing
    }

-- | An object that describes the endpoint of the deployment.
containerServiceDeploymentRequest_publicEndpoint :: Lens.Lens' ContainerServiceDeploymentRequest (Prelude.Maybe EndpointRequest)
containerServiceDeploymentRequest_publicEndpoint = Lens.lens (\ContainerServiceDeploymentRequest' {publicEndpoint} -> publicEndpoint) (\s@ContainerServiceDeploymentRequest' {} a -> s {publicEndpoint = a} :: ContainerServiceDeploymentRequest)

-- | An object that describes the configuration for the containers of the
-- deployment.
containerServiceDeploymentRequest_containers :: Lens.Lens' ContainerServiceDeploymentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Container))
containerServiceDeploymentRequest_containers = Lens.lens (\ContainerServiceDeploymentRequest' {containers} -> containers) (\s@ContainerServiceDeploymentRequest' {} a -> s {containers = a} :: ContainerServiceDeploymentRequest) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.Hashable
    ContainerServiceDeploymentRequest

instance
  Prelude.NFData
    ContainerServiceDeploymentRequest

instance
  Prelude.ToJSON
    ContainerServiceDeploymentRequest
  where
  toJSON ContainerServiceDeploymentRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("publicEndpoint" Prelude..=)
              Prelude.<$> publicEndpoint,
            ("containers" Prelude..=) Prelude.<$> containers
          ]
      )
