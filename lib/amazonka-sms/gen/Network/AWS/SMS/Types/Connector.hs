{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Connector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.Connector
  ( Connector (..)
  -- * Smart constructor
  , mkConnector
  -- * Lenses
  , cAssociatedOn
  , cCapabilityList
  , cConnectorId
  , cIpAddress
  , cMacAddress
  , cStatus
  , cVersion
  , cVmManagerId
  , cVmManagerName
  , cVmManagerType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ConnectorCapability as Types
import qualified Network.AWS.SMS.Types.ConnectorId as Types
import qualified Network.AWS.SMS.Types.ConnectorStatus as Types
import qualified Network.AWS.SMS.Types.IpAddress as Types
import qualified Network.AWS.SMS.Types.MacAddress as Types
import qualified Network.AWS.SMS.Types.Version as Types
import qualified Network.AWS.SMS.Types.VmManagerId as Types
import qualified Network.AWS.SMS.Types.VmManagerName as Types
import qualified Network.AWS.SMS.Types.VmManagerType as Types

-- | Represents a connector.
--
-- /See:/ 'mkConnector' smart constructor.
data Connector = Connector'
  { associatedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the connector was associated.
  , capabilityList :: Core.Maybe [Types.ConnectorCapability]
    -- ^ The capabilities of the connector.
  , connectorId :: Core.Maybe Types.ConnectorId
    -- ^ The ID of the connector.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The IP address of the connector.
  , macAddress :: Core.Maybe Types.MacAddress
    -- ^ The MAC address of the connector.
  , status :: Core.Maybe Types.ConnectorStatus
    -- ^ The status of the connector.
  , version :: Core.Maybe Types.Version
    -- ^ The connector version.
  , vmManagerId :: Core.Maybe Types.VmManagerId
    -- ^ The ID of the VM manager.
  , vmManagerName :: Core.Maybe Types.VmManagerName
    -- ^ The name of the VM manager.
  , vmManagerType :: Core.Maybe Types.VmManagerType
    -- ^ The VM management product.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Connector' value with any optional fields omitted.
mkConnector
    :: Connector
mkConnector
  = Connector'{associatedOn = Core.Nothing,
               capabilityList = Core.Nothing, connectorId = Core.Nothing,
               ipAddress = Core.Nothing, macAddress = Core.Nothing,
               status = Core.Nothing, version = Core.Nothing,
               vmManagerId = Core.Nothing, vmManagerName = Core.Nothing,
               vmManagerType = Core.Nothing}

-- | The time the connector was associated.
--
-- /Note:/ Consider using 'associatedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAssociatedOn :: Lens.Lens' Connector (Core.Maybe Core.NominalDiffTime)
cAssociatedOn = Lens.field @"associatedOn"
{-# INLINEABLE cAssociatedOn #-}
{-# DEPRECATED associatedOn "Use generic-lens or generic-optics with 'associatedOn' instead"  #-}

-- | The capabilities of the connector.
--
-- /Note:/ Consider using 'capabilityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapabilityList :: Lens.Lens' Connector (Core.Maybe [Types.ConnectorCapability])
cCapabilityList = Lens.field @"capabilityList"
{-# INLINEABLE cCapabilityList #-}
{-# DEPRECATED capabilityList "Use generic-lens or generic-optics with 'capabilityList' instead"  #-}

-- | The ID of the connector.
--
-- /Note:/ Consider using 'connectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectorId :: Lens.Lens' Connector (Core.Maybe Types.ConnectorId)
cConnectorId = Lens.field @"connectorId"
{-# INLINEABLE cConnectorId #-}
{-# DEPRECATED connectorId "Use generic-lens or generic-optics with 'connectorId' instead"  #-}

-- | The IP address of the connector.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIpAddress :: Lens.Lens' Connector (Core.Maybe Types.IpAddress)
cIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE cIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The MAC address of the connector.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMacAddress :: Lens.Lens' Connector (Core.Maybe Types.MacAddress)
cMacAddress = Lens.field @"macAddress"
{-# INLINEABLE cMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | The status of the connector.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Connector (Core.Maybe Types.ConnectorStatus)
cStatus = Lens.field @"status"
{-# INLINEABLE cStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The connector version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVersion :: Lens.Lens' Connector (Core.Maybe Types.Version)
cVersion = Lens.field @"version"
{-# INLINEABLE cVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The ID of the VM manager.
--
-- /Note:/ Consider using 'vmManagerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerId :: Lens.Lens' Connector (Core.Maybe Types.VmManagerId)
cVmManagerId = Lens.field @"vmManagerId"
{-# INLINEABLE cVmManagerId #-}
{-# DEPRECATED vmManagerId "Use generic-lens or generic-optics with 'vmManagerId' instead"  #-}

-- | The name of the VM manager.
--
-- /Note:/ Consider using 'vmManagerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerName :: Lens.Lens' Connector (Core.Maybe Types.VmManagerName)
cVmManagerName = Lens.field @"vmManagerName"
{-# INLINEABLE cVmManagerName #-}
{-# DEPRECATED vmManagerName "Use generic-lens or generic-optics with 'vmManagerName' instead"  #-}

-- | The VM management product.
--
-- /Note:/ Consider using 'vmManagerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVmManagerType :: Lens.Lens' Connector (Core.Maybe Types.VmManagerType)
cVmManagerType = Lens.field @"vmManagerType"
{-# INLINEABLE cVmManagerType #-}
{-# DEPRECATED vmManagerType "Use generic-lens or generic-optics with 'vmManagerType' instead"  #-}

instance Core.FromJSON Connector where
        parseJSON
          = Core.withObject "Connector" Core.$
              \ x ->
                Connector' Core.<$>
                  (x Core..:? "associatedOn") Core.<*> x Core..:? "capabilityList"
                    Core.<*> x Core..:? "connectorId"
                    Core.<*> x Core..:? "ipAddress"
                    Core.<*> x Core..:? "macAddress"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "version"
                    Core.<*> x Core..:? "vmManagerId"
                    Core.<*> x Core..:? "vmManagerName"
                    Core.<*> x Core..:? "vmManagerType"
