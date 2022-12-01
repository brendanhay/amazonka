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
-- Module      : Amazonka.NetworkManager.Types.NetworkTelemetry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkTelemetry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types.ConnectionHealth
import qualified Amazonka.Prelude as Prelude

-- | Describes the telemetry information for a resource.
--
-- /See:/ 'newNetworkTelemetry' smart constructor.
data NetworkTelemetry = NetworkTelemetry'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The connection health.
    health :: Prelude.Maybe ConnectionHealth,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkTelemetry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'networkTelemetry_resourceId' - The ID of the resource.
--
-- 'resourceType', 'networkTelemetry_resourceType' - The resource type.
--
-- 'coreNetworkId', 'networkTelemetry_coreNetworkId' - The ID of a core network.
--
-- 'accountId', 'networkTelemetry_accountId' - The Amazon Web Services account ID.
--
-- 'address', 'networkTelemetry_address' - The address.
--
-- 'registeredGatewayArn', 'networkTelemetry_registeredGatewayArn' - The ARN of the gateway.
--
-- 'health', 'networkTelemetry_health' - The connection health.
--
-- 'awsRegion', 'networkTelemetry_awsRegion' - The Amazon Web Services Region.
--
-- 'resourceArn', 'networkTelemetry_resourceArn' - The ARN of the resource.
newNetworkTelemetry ::
  NetworkTelemetry
newNetworkTelemetry =
  NetworkTelemetry'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      address = Prelude.Nothing,
      registeredGatewayArn = Prelude.Nothing,
      health = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The ID of the resource.
networkTelemetry_resourceId :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_resourceId = Lens.lens (\NetworkTelemetry' {resourceId} -> resourceId) (\s@NetworkTelemetry' {} a -> s {resourceId = a} :: NetworkTelemetry)

-- | The resource type.
networkTelemetry_resourceType :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_resourceType = Lens.lens (\NetworkTelemetry' {resourceType} -> resourceType) (\s@NetworkTelemetry' {} a -> s {resourceType = a} :: NetworkTelemetry)

-- | The ID of a core network.
networkTelemetry_coreNetworkId :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_coreNetworkId = Lens.lens (\NetworkTelemetry' {coreNetworkId} -> coreNetworkId) (\s@NetworkTelemetry' {} a -> s {coreNetworkId = a} :: NetworkTelemetry)

-- | The Amazon Web Services account ID.
networkTelemetry_accountId :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_accountId = Lens.lens (\NetworkTelemetry' {accountId} -> accountId) (\s@NetworkTelemetry' {} a -> s {accountId = a} :: NetworkTelemetry)

-- | The address.
networkTelemetry_address :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_address = Lens.lens (\NetworkTelemetry' {address} -> address) (\s@NetworkTelemetry' {} a -> s {address = a} :: NetworkTelemetry)

-- | The ARN of the gateway.
networkTelemetry_registeredGatewayArn :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_registeredGatewayArn = Lens.lens (\NetworkTelemetry' {registeredGatewayArn} -> registeredGatewayArn) (\s@NetworkTelemetry' {} a -> s {registeredGatewayArn = a} :: NetworkTelemetry)

-- | The connection health.
networkTelemetry_health :: Lens.Lens' NetworkTelemetry (Prelude.Maybe ConnectionHealth)
networkTelemetry_health = Lens.lens (\NetworkTelemetry' {health} -> health) (\s@NetworkTelemetry' {} a -> s {health = a} :: NetworkTelemetry)

-- | The Amazon Web Services Region.
networkTelemetry_awsRegion :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_awsRegion = Lens.lens (\NetworkTelemetry' {awsRegion} -> awsRegion) (\s@NetworkTelemetry' {} a -> s {awsRegion = a} :: NetworkTelemetry)

-- | The ARN of the resource.
networkTelemetry_resourceArn :: Lens.Lens' NetworkTelemetry (Prelude.Maybe Prelude.Text)
networkTelemetry_resourceArn = Lens.lens (\NetworkTelemetry' {resourceArn} -> resourceArn) (\s@NetworkTelemetry' {} a -> s {resourceArn = a} :: NetworkTelemetry)

instance Core.FromJSON NetworkTelemetry where
  parseJSON =
    Core.withObject
      "NetworkTelemetry"
      ( \x ->
          NetworkTelemetry'
            Prelude.<$> (x Core..:? "ResourceId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "CoreNetworkId")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "RegisteredGatewayArn")
            Prelude.<*> (x Core..:? "Health")
            Prelude.<*> (x Core..:? "AwsRegion")
            Prelude.<*> (x Core..:? "ResourceArn")
      )

instance Prelude.Hashable NetworkTelemetry where
  hashWithSalt _salt NetworkTelemetry' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` registeredGatewayArn
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData NetworkTelemetry where
  rnf NetworkTelemetry' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf resourceArn
