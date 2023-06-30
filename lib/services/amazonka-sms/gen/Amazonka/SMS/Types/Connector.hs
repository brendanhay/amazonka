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
-- Module      : Amazonka.SMS.Types.Connector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.Connector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.ConnectorCapability
import Amazonka.SMS.Types.ConnectorStatus
import Amazonka.SMS.Types.VmManagerType

-- | Represents a connector.
--
-- /See:/ 'newConnector' smart constructor.
data Connector = Connector'
  { -- | The time the connector was associated.
    associatedOn :: Prelude.Maybe Data.POSIX,
    -- | The capabilities of the connector.
    capabilityList :: Prelude.Maybe [ConnectorCapability],
    -- | The ID of the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the connector.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The MAC address of the connector.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The status of the connector.
    status :: Prelude.Maybe ConnectorStatus,
    -- | The connector version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VM manager.
    vmManagerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the VM manager.
    vmManagerName :: Prelude.Maybe Prelude.Text,
    -- | The VM management product.
    vmManagerType :: Prelude.Maybe VmManagerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedOn', 'connector_associatedOn' - The time the connector was associated.
--
-- 'capabilityList', 'connector_capabilityList' - The capabilities of the connector.
--
-- 'connectorId', 'connector_connectorId' - The ID of the connector.
--
-- 'ipAddress', 'connector_ipAddress' - The IP address of the connector.
--
-- 'macAddress', 'connector_macAddress' - The MAC address of the connector.
--
-- 'status', 'connector_status' - The status of the connector.
--
-- 'version', 'connector_version' - The connector version.
--
-- 'vmManagerId', 'connector_vmManagerId' - The ID of the VM manager.
--
-- 'vmManagerName', 'connector_vmManagerName' - The name of the VM manager.
--
-- 'vmManagerType', 'connector_vmManagerType' - The VM management product.
newConnector ::
  Connector
newConnector =
  Connector'
    { associatedOn = Prelude.Nothing,
      capabilityList = Prelude.Nothing,
      connectorId = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing,
      vmManagerId = Prelude.Nothing,
      vmManagerName = Prelude.Nothing,
      vmManagerType = Prelude.Nothing
    }

-- | The time the connector was associated.
connector_associatedOn :: Lens.Lens' Connector (Prelude.Maybe Prelude.UTCTime)
connector_associatedOn = Lens.lens (\Connector' {associatedOn} -> associatedOn) (\s@Connector' {} a -> s {associatedOn = a} :: Connector) Prelude.. Lens.mapping Data._Time

-- | The capabilities of the connector.
connector_capabilityList :: Lens.Lens' Connector (Prelude.Maybe [ConnectorCapability])
connector_capabilityList = Lens.lens (\Connector' {capabilityList} -> capabilityList) (\s@Connector' {} a -> s {capabilityList = a} :: Connector) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the connector.
connector_connectorId :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_connectorId = Lens.lens (\Connector' {connectorId} -> connectorId) (\s@Connector' {} a -> s {connectorId = a} :: Connector)

-- | The IP address of the connector.
connector_ipAddress :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_ipAddress = Lens.lens (\Connector' {ipAddress} -> ipAddress) (\s@Connector' {} a -> s {ipAddress = a} :: Connector)

-- | The MAC address of the connector.
connector_macAddress :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_macAddress = Lens.lens (\Connector' {macAddress} -> macAddress) (\s@Connector' {} a -> s {macAddress = a} :: Connector)

-- | The status of the connector.
connector_status :: Lens.Lens' Connector (Prelude.Maybe ConnectorStatus)
connector_status = Lens.lens (\Connector' {status} -> status) (\s@Connector' {} a -> s {status = a} :: Connector)

-- | The connector version.
connector_version :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_version = Lens.lens (\Connector' {version} -> version) (\s@Connector' {} a -> s {version = a} :: Connector)

-- | The ID of the VM manager.
connector_vmManagerId :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_vmManagerId = Lens.lens (\Connector' {vmManagerId} -> vmManagerId) (\s@Connector' {} a -> s {vmManagerId = a} :: Connector)

-- | The name of the VM manager.
connector_vmManagerName :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_vmManagerName = Lens.lens (\Connector' {vmManagerName} -> vmManagerName) (\s@Connector' {} a -> s {vmManagerName = a} :: Connector)

-- | The VM management product.
connector_vmManagerType :: Lens.Lens' Connector (Prelude.Maybe VmManagerType)
connector_vmManagerType = Lens.lens (\Connector' {vmManagerType} -> vmManagerType) (\s@Connector' {} a -> s {vmManagerType = a} :: Connector)

instance Data.FromJSON Connector where
  parseJSON =
    Data.withObject
      "Connector"
      ( \x ->
          Connector'
            Prelude.<$> (x Data..:? "associatedOn")
            Prelude.<*> (x Data..:? "capabilityList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "connectorId")
            Prelude.<*> (x Data..:? "ipAddress")
            Prelude.<*> (x Data..:? "macAddress")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
            Prelude.<*> (x Data..:? "vmManagerId")
            Prelude.<*> (x Data..:? "vmManagerName")
            Prelude.<*> (x Data..:? "vmManagerType")
      )

instance Prelude.Hashable Connector where
  hashWithSalt _salt Connector' {..} =
    _salt
      `Prelude.hashWithSalt` associatedOn
      `Prelude.hashWithSalt` capabilityList
      `Prelude.hashWithSalt` connectorId
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` vmManagerId
      `Prelude.hashWithSalt` vmManagerName
      `Prelude.hashWithSalt` vmManagerType

instance Prelude.NFData Connector where
  rnf Connector' {..} =
    Prelude.rnf associatedOn
      `Prelude.seq` Prelude.rnf capabilityList
      `Prelude.seq` Prelude.rnf connectorId
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf vmManagerId
      `Prelude.seq` Prelude.rnf vmManagerName
      `Prelude.seq` Prelude.rnf vmManagerType
