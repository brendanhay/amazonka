-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Connector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Connector
  ( Connector (..),

    -- * Smart constructor
    mkConnector,

    -- * Lenses
    cStatus,
    cVmManagerName,
    cIpAddress,
    cVmManagerId,
    cVmManagerType,
    cConnectorId,
    cAssociatedOn,
    cMacAddress,
    cVersion,
    cCapabilityList,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ConnectorCapability
import Network.AWS.SMS.Types.ConnectorStatus
import Network.AWS.SMS.Types.VMManagerType

-- | Represents a connector.
--
-- /See:/ 'mkConnector' smart constructor.
data Connector = Connector'
  { status :: Lude.Maybe ConnectorStatus,
    vmManagerName :: Lude.Maybe Lude.Text,
    ipAddress :: Lude.Maybe Lude.Text,
    vmManagerId :: Lude.Maybe Lude.Text,
    vmManagerType :: Lude.Maybe VMManagerType,
    connectorId :: Lude.Maybe Lude.Text,
    associatedOn :: Lude.Maybe Lude.Timestamp,
    macAddress :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    capabilityList :: Lude.Maybe [ConnectorCapability]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- * 'associatedOn' - The time the connector was associated.
-- * 'capabilityList' - The capabilities of the connector.
-- * 'connectorId' - The ID of the connector.
-- * 'ipAddress' - The IP address of the connector.
-- * 'macAddress' - The MAC address of the connector.
-- * 'status' - The status of the connector.
-- * 'version' - The connector version.
-- * 'vmManagerId' - The ID of the VM manager.
-- * 'vmManagerName' - The name of the VM manager.
-- * 'vmManagerType' - The VM management product.
mkConnector ::
  Connector
mkConnector =
  Connector'
    { status = Lude.Nothing,
      vmManagerName = Lude.Nothing,
      ipAddress = Lude.Nothing,
      vmManagerId = Lude.Nothing,
      vmManagerType = Lude.Nothing,
      connectorId = Lude.Nothing,
      associatedOn = Lude.Nothing,
      macAddress = Lude.Nothing,
      version = Lude.Nothing,
      capabilityList = Lude.Nothing
    }

-- | The status of the connector.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Connector (Lude.Maybe ConnectorStatus)
cStatus = Lens.lens (status :: Connector -> Lude.Maybe ConnectorStatus) (\s a -> s {status = a} :: Connector)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the VM manager.
--
-- /Note:/ Consider using 'vmManagerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerName :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cVmManagerName = Lens.lens (vmManagerName :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {vmManagerName = a} :: Connector)
{-# DEPRECATED cVmManagerName "Use generic-lens or generic-optics with 'vmManagerName' instead." #-}

-- | The IP address of the connector.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIpAddress :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cIpAddress = Lens.lens (ipAddress :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: Connector)
{-# DEPRECATED cIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | The ID of the VM manager.
--
-- /Note:/ Consider using 'vmManagerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerId :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cVmManagerId = Lens.lens (vmManagerId :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {vmManagerId = a} :: Connector)
{-# DEPRECATED cVmManagerId "Use generic-lens or generic-optics with 'vmManagerId' instead." #-}

-- | The VM management product.
--
-- /Note:/ Consider using 'vmManagerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerType :: Lens.Lens' Connector (Lude.Maybe VMManagerType)
cVmManagerType = Lens.lens (vmManagerType :: Connector -> Lude.Maybe VMManagerType) (\s a -> s {vmManagerType = a} :: Connector)
{-# DEPRECATED cVmManagerType "Use generic-lens or generic-optics with 'vmManagerType' instead." #-}

-- | The ID of the connector.
--
-- /Note:/ Consider using 'connectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectorId :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cConnectorId = Lens.lens (connectorId :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {connectorId = a} :: Connector)
{-# DEPRECATED cConnectorId "Use generic-lens or generic-optics with 'connectorId' instead." #-}

-- | The time the connector was associated.
--
-- /Note:/ Consider using 'associatedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAssociatedOn :: Lens.Lens' Connector (Lude.Maybe Lude.Timestamp)
cAssociatedOn = Lens.lens (associatedOn :: Connector -> Lude.Maybe Lude.Timestamp) (\s a -> s {associatedOn = a} :: Connector)
{-# DEPRECATED cAssociatedOn "Use generic-lens or generic-optics with 'associatedOn' instead." #-}

-- | The MAC address of the connector.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMacAddress :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cMacAddress = Lens.lens (macAddress :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {macAddress = a} :: Connector)
{-# DEPRECATED cMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

-- | The connector version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersion :: Lens.Lens' Connector (Lude.Maybe Lude.Text)
cVersion = Lens.lens (version :: Connector -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Connector)
{-# DEPRECATED cVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The capabilities of the connector.
--
-- /Note:/ Consider using 'capabilityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapabilityList :: Lens.Lens' Connector (Lude.Maybe [ConnectorCapability])
cCapabilityList = Lens.lens (capabilityList :: Connector -> Lude.Maybe [ConnectorCapability]) (\s a -> s {capabilityList = a} :: Connector)
{-# DEPRECATED cCapabilityList "Use generic-lens or generic-optics with 'capabilityList' instead." #-}

instance Lude.FromJSON Connector where
  parseJSON =
    Lude.withObject
      "Connector"
      ( \x ->
          Connector'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "vmManagerName")
            Lude.<*> (x Lude..:? "ipAddress")
            Lude.<*> (x Lude..:? "vmManagerId")
            Lude.<*> (x Lude..:? "vmManagerType")
            Lude.<*> (x Lude..:? "connectorId")
            Lude.<*> (x Lude..:? "associatedOn")
            Lude.<*> (x Lude..:? "macAddress")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "capabilityList" Lude..!= Lude.mempty)
      )
