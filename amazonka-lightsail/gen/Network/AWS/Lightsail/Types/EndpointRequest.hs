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
-- Module      : Network.AWS.Lightsail.Types.EndpointRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.EndpointRequest where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import qualified Network.AWS.Prelude as Prelude

-- | Describes the settings of a public endpoint for an Amazon Lightsail
-- container service.
--
-- /See:/ 'newEndpointRequest' smart constructor.
data EndpointRequest = EndpointRequest'
  { -- | An object that describes the health check configuration of the
    -- container.
    healthCheck :: Prelude.Maybe ContainerServiceHealthCheckConfig,
    -- | The name of the container for the endpoint.
    containerName :: Prelude.Text,
    -- | The port of the container to which traffic is forwarded to.
    containerPort :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'endpointRequest_healthCheck' - An object that describes the health check configuration of the
-- container.
--
-- 'containerName', 'endpointRequest_containerName' - The name of the container for the endpoint.
--
-- 'containerPort', 'endpointRequest_containerPort' - The port of the container to which traffic is forwarded to.
newEndpointRequest ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'containerPort'
  Prelude.Int ->
  EndpointRequest
newEndpointRequest pContainerName_ pContainerPort_ =
  EndpointRequest'
    { healthCheck = Prelude.Nothing,
      containerName = pContainerName_,
      containerPort = pContainerPort_
    }

-- | An object that describes the health check configuration of the
-- container.
endpointRequest_healthCheck :: Lens.Lens' EndpointRequest (Prelude.Maybe ContainerServiceHealthCheckConfig)
endpointRequest_healthCheck = Lens.lens (\EndpointRequest' {healthCheck} -> healthCheck) (\s@EndpointRequest' {} a -> s {healthCheck = a} :: EndpointRequest)

-- | The name of the container for the endpoint.
endpointRequest_containerName :: Lens.Lens' EndpointRequest Prelude.Text
endpointRequest_containerName = Lens.lens (\EndpointRequest' {containerName} -> containerName) (\s@EndpointRequest' {} a -> s {containerName = a} :: EndpointRequest)

-- | The port of the container to which traffic is forwarded to.
endpointRequest_containerPort :: Lens.Lens' EndpointRequest Prelude.Int
endpointRequest_containerPort = Lens.lens (\EndpointRequest' {containerPort} -> containerPort) (\s@EndpointRequest' {} a -> s {containerPort = a} :: EndpointRequest)

instance Prelude.Hashable EndpointRequest

instance Prelude.NFData EndpointRequest

instance Prelude.ToJSON EndpointRequest where
  toJSON EndpointRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("healthCheck" Prelude..=) Prelude.<$> healthCheck,
            Prelude.Just
              ("containerName" Prelude..= containerName),
            Prelude.Just
              ("containerPort" Prelude..= containerPort)
          ]
      )
