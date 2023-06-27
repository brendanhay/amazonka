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
-- Module      : Amazonka.BackupGateway.Types.Gateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupGateway.Types.Gateway where

import Amazonka.BackupGateway.Types.GatewayType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A gateway is an Backup Gateway appliance that runs on the customer\'s
-- network to provide seamless connectivity to backup storage in the Amazon
-- Web Services Cloud.
--
-- /See:/ 'newGateway' smart constructor.
data Gateway = Gateway'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the gateway.
    gatewayDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The type of the gateway.
    gatewayType :: Prelude.Maybe GatewayType,
    -- | The hypervisor ID of the gateway.
    hypervisorId :: Prelude.Maybe Prelude.Text,
    -- | The last time Backup gateway communicated with the gateway, in Unix
    -- format and UTC time.
    lastSeenTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Gateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'gateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'gatewayDisplayName', 'gateway_gatewayDisplayName' - The display name of the gateway.
--
-- 'gatewayType', 'gateway_gatewayType' - The type of the gateway.
--
-- 'hypervisorId', 'gateway_hypervisorId' - The hypervisor ID of the gateway.
--
-- 'lastSeenTime', 'gateway_lastSeenTime' - The last time Backup gateway communicated with the gateway, in Unix
-- format and UTC time.
newGateway ::
  Gateway
newGateway =
  Gateway'
    { gatewayArn = Prelude.Nothing,
      gatewayDisplayName = Prelude.Nothing,
      gatewayType = Prelude.Nothing,
      hypervisorId = Prelude.Nothing,
      lastSeenTime = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
gateway_gatewayArn :: Lens.Lens' Gateway (Prelude.Maybe Prelude.Text)
gateway_gatewayArn = Lens.lens (\Gateway' {gatewayArn} -> gatewayArn) (\s@Gateway' {} a -> s {gatewayArn = a} :: Gateway)

-- | The display name of the gateway.
gateway_gatewayDisplayName :: Lens.Lens' Gateway (Prelude.Maybe Prelude.Text)
gateway_gatewayDisplayName = Lens.lens (\Gateway' {gatewayDisplayName} -> gatewayDisplayName) (\s@Gateway' {} a -> s {gatewayDisplayName = a} :: Gateway)

-- | The type of the gateway.
gateway_gatewayType :: Lens.Lens' Gateway (Prelude.Maybe GatewayType)
gateway_gatewayType = Lens.lens (\Gateway' {gatewayType} -> gatewayType) (\s@Gateway' {} a -> s {gatewayType = a} :: Gateway)

-- | The hypervisor ID of the gateway.
gateway_hypervisorId :: Lens.Lens' Gateway (Prelude.Maybe Prelude.Text)
gateway_hypervisorId = Lens.lens (\Gateway' {hypervisorId} -> hypervisorId) (\s@Gateway' {} a -> s {hypervisorId = a} :: Gateway)

-- | The last time Backup gateway communicated with the gateway, in Unix
-- format and UTC time.
gateway_lastSeenTime :: Lens.Lens' Gateway (Prelude.Maybe Prelude.UTCTime)
gateway_lastSeenTime = Lens.lens (\Gateway' {lastSeenTime} -> lastSeenTime) (\s@Gateway' {} a -> s {lastSeenTime = a} :: Gateway) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Gateway where
  parseJSON =
    Data.withObject
      "Gateway"
      ( \x ->
          Gateway'
            Prelude.<$> (x Data..:? "GatewayArn")
            Prelude.<*> (x Data..:? "GatewayDisplayName")
            Prelude.<*> (x Data..:? "GatewayType")
            Prelude.<*> (x Data..:? "HypervisorId")
            Prelude.<*> (x Data..:? "LastSeenTime")
      )

instance Prelude.Hashable Gateway where
  hashWithSalt _salt Gateway' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` gatewayDisplayName
      `Prelude.hashWithSalt` gatewayType
      `Prelude.hashWithSalt` hypervisorId
      `Prelude.hashWithSalt` lastSeenTime

instance Prelude.NFData Gateway where
  rnf Gateway' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf gatewayDisplayName
      `Prelude.seq` Prelude.rnf gatewayType
      `Prelude.seq` Prelude.rnf hypervisorId
      `Prelude.seq` Prelude.rnf lastSeenTime
