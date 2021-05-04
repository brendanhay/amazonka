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
-- Module      : Network.AWS.SMS.Types.Connector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Connector where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.ConnectorCapability
import Network.AWS.SMS.Types.ConnectorStatus
import Network.AWS.SMS.Types.VmManagerType

-- | Represents a connector.
--
-- /See:/ 'newConnector' smart constructor.
data Connector = Connector'
  { -- | The status of the connector.
    status :: Prelude.Maybe ConnectorStatus,
    -- | The MAC address of the connector.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The time the connector was associated.
    associatedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the connector.
    connectorId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VM manager.
    vmManagerId :: Prelude.Maybe Prelude.Text,
    -- | The connector version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the VM manager.
    vmManagerName :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the connector.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The VM management product.
    vmManagerType :: Prelude.Maybe VmManagerType,
    -- | The capabilities of the connector.
    capabilityList :: Prelude.Maybe [ConnectorCapability]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Connector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'connector_status' - The status of the connector.
--
-- 'macAddress', 'connector_macAddress' - The MAC address of the connector.
--
-- 'associatedOn', 'connector_associatedOn' - The time the connector was associated.
--
-- 'connectorId', 'connector_connectorId' - The ID of the connector.
--
-- 'vmManagerId', 'connector_vmManagerId' - The ID of the VM manager.
--
-- 'version', 'connector_version' - The connector version.
--
-- 'vmManagerName', 'connector_vmManagerName' - The name of the VM manager.
--
-- 'ipAddress', 'connector_ipAddress' - The IP address of the connector.
--
-- 'vmManagerType', 'connector_vmManagerType' - The VM management product.
--
-- 'capabilityList', 'connector_capabilityList' - The capabilities of the connector.
newConnector ::
  Connector
newConnector =
  Connector'
    { status = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      associatedOn = Prelude.Nothing,
      connectorId = Prelude.Nothing,
      vmManagerId = Prelude.Nothing,
      version = Prelude.Nothing,
      vmManagerName = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      vmManagerType = Prelude.Nothing,
      capabilityList = Prelude.Nothing
    }

-- | The status of the connector.
connector_status :: Lens.Lens' Connector (Prelude.Maybe ConnectorStatus)
connector_status = Lens.lens (\Connector' {status} -> status) (\s@Connector' {} a -> s {status = a} :: Connector)

-- | The MAC address of the connector.
connector_macAddress :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_macAddress = Lens.lens (\Connector' {macAddress} -> macAddress) (\s@Connector' {} a -> s {macAddress = a} :: Connector)

-- | The time the connector was associated.
connector_associatedOn :: Lens.Lens' Connector (Prelude.Maybe Prelude.UTCTime)
connector_associatedOn = Lens.lens (\Connector' {associatedOn} -> associatedOn) (\s@Connector' {} a -> s {associatedOn = a} :: Connector) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the connector.
connector_connectorId :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_connectorId = Lens.lens (\Connector' {connectorId} -> connectorId) (\s@Connector' {} a -> s {connectorId = a} :: Connector)

-- | The ID of the VM manager.
connector_vmManagerId :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_vmManagerId = Lens.lens (\Connector' {vmManagerId} -> vmManagerId) (\s@Connector' {} a -> s {vmManagerId = a} :: Connector)

-- | The connector version.
connector_version :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_version = Lens.lens (\Connector' {version} -> version) (\s@Connector' {} a -> s {version = a} :: Connector)

-- | The name of the VM manager.
connector_vmManagerName :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_vmManagerName = Lens.lens (\Connector' {vmManagerName} -> vmManagerName) (\s@Connector' {} a -> s {vmManagerName = a} :: Connector)

-- | The IP address of the connector.
connector_ipAddress :: Lens.Lens' Connector (Prelude.Maybe Prelude.Text)
connector_ipAddress = Lens.lens (\Connector' {ipAddress} -> ipAddress) (\s@Connector' {} a -> s {ipAddress = a} :: Connector)

-- | The VM management product.
connector_vmManagerType :: Lens.Lens' Connector (Prelude.Maybe VmManagerType)
connector_vmManagerType = Lens.lens (\Connector' {vmManagerType} -> vmManagerType) (\s@Connector' {} a -> s {vmManagerType = a} :: Connector)

-- | The capabilities of the connector.
connector_capabilityList :: Lens.Lens' Connector (Prelude.Maybe [ConnectorCapability])
connector_capabilityList = Lens.lens (\Connector' {capabilityList} -> capabilityList) (\s@Connector' {} a -> s {capabilityList = a} :: Connector) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Connector where
  parseJSON =
    Prelude.withObject
      "Connector"
      ( \x ->
          Connector'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "macAddress")
            Prelude.<*> (x Prelude..:? "associatedOn")
            Prelude.<*> (x Prelude..:? "connectorId")
            Prelude.<*> (x Prelude..:? "vmManagerId")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "vmManagerName")
            Prelude.<*> (x Prelude..:? "ipAddress")
            Prelude.<*> (x Prelude..:? "vmManagerType")
            Prelude.<*> ( x Prelude..:? "capabilityList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Connector

instance Prelude.NFData Connector
