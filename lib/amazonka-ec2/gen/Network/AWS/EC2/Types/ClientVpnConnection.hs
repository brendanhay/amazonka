{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnConnection
  ( ClientVpnConnection (..)
  -- * Smart constructor
  , mkClientVpnConnection
  -- * Lenses
  , cvcClientIp
  , cvcClientVpnEndpointId
  , cvcCommonName
  , cvcConnectionEndTime
  , cvcConnectionEstablishedTime
  , cvcConnectionId
  , cvcEgressBytes
  , cvcEgressPackets
  , cvcIngressBytes
  , cvcIngressPackets
  , cvcPostureComplianceStatuses
  , cvcStatus
  , cvcTimestamp
  , cvcUsername
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnConnectionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a client connection.
--
-- /See:/ 'mkClientVpnConnection' smart constructor.
data ClientVpnConnection = ClientVpnConnection'
  { clientIp :: Core.Maybe Core.Text
    -- ^ The IP address of the client.
  , clientVpnEndpointId :: Core.Maybe Core.Text
    -- ^ The ID of the Client VPN endpoint to which the client is connected.
  , commonName :: Core.Maybe Core.Text
    -- ^ The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
  , connectionEndTime :: Core.Maybe Core.Text
    -- ^ The date and time the client connection was terminated.
  , connectionEstablishedTime :: Core.Maybe Core.Text
    -- ^ The date and time the client connection was established.
  , connectionId :: Core.Maybe Core.Text
    -- ^ The ID of the client connection.
  , egressBytes :: Core.Maybe Core.Text
    -- ^ The number of bytes received by the client.
  , egressPackets :: Core.Maybe Core.Text
    -- ^ The number of packets received by the client.
  , ingressBytes :: Core.Maybe Core.Text
    -- ^ The number of bytes sent by the client.
  , ingressPackets :: Core.Maybe Core.Text
    -- ^ The number of packets sent by the client.
  , postureComplianceStatuses :: Core.Maybe [Core.Text]
    -- ^ The statuses returned by the client connect handler for posture compliance, if applicable.
  , status :: Core.Maybe Types.ClientVpnConnectionStatus
    -- ^ The current state of the client connection.
  , timestamp :: Core.Maybe Core.Text
    -- ^ The current date and time.
  , username :: Core.Maybe Core.Text
    -- ^ The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnConnection' value with any optional fields omitted.
mkClientVpnConnection
    :: ClientVpnConnection
mkClientVpnConnection
  = ClientVpnConnection'{clientIp = Core.Nothing,
                         clientVpnEndpointId = Core.Nothing, commonName = Core.Nothing,
                         connectionEndTime = Core.Nothing,
                         connectionEstablishedTime = Core.Nothing,
                         connectionId = Core.Nothing, egressBytes = Core.Nothing,
                         egressPackets = Core.Nothing, ingressBytes = Core.Nothing,
                         ingressPackets = Core.Nothing,
                         postureComplianceStatuses = Core.Nothing, status = Core.Nothing,
                         timestamp = Core.Nothing, username = Core.Nothing}

-- | The IP address of the client.
--
-- /Note:/ Consider using 'clientIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcClientIp :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcClientIp = Lens.field @"clientIp"
{-# INLINEABLE cvcClientIp #-}
{-# DEPRECATED clientIp "Use generic-lens or generic-optics with 'clientIp' instead"  #-}

-- | The ID of the Client VPN endpoint to which the client is connected.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcClientVpnEndpointId :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE cvcClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The common name associated with the client. This is either the name of the client certificate, or the Active Directory user name.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcCommonName :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcCommonName = Lens.field @"commonName"
{-# INLINEABLE cvcCommonName #-}
{-# DEPRECATED commonName "Use generic-lens or generic-optics with 'commonName' instead"  #-}

-- | The date and time the client connection was terminated.
--
-- /Note:/ Consider using 'connectionEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionEndTime :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcConnectionEndTime = Lens.field @"connectionEndTime"
{-# INLINEABLE cvcConnectionEndTime #-}
{-# DEPRECATED connectionEndTime "Use generic-lens or generic-optics with 'connectionEndTime' instead"  #-}

-- | The date and time the client connection was established.
--
-- /Note:/ Consider using 'connectionEstablishedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionEstablishedTime :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcConnectionEstablishedTime = Lens.field @"connectionEstablishedTime"
{-# INLINEABLE cvcConnectionEstablishedTime #-}
{-# DEPRECATED connectionEstablishedTime "Use generic-lens or generic-optics with 'connectionEstablishedTime' instead"  #-}

-- | The ID of the client connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcConnectionId :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcConnectionId = Lens.field @"connectionId"
{-# INLINEABLE cvcConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The number of bytes received by the client.
--
-- /Note:/ Consider using 'egressBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcEgressBytes :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcEgressBytes = Lens.field @"egressBytes"
{-# INLINEABLE cvcEgressBytes #-}
{-# DEPRECATED egressBytes "Use generic-lens or generic-optics with 'egressBytes' instead"  #-}

-- | The number of packets received by the client.
--
-- /Note:/ Consider using 'egressPackets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcEgressPackets :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcEgressPackets = Lens.field @"egressPackets"
{-# INLINEABLE cvcEgressPackets #-}
{-# DEPRECATED egressPackets "Use generic-lens or generic-optics with 'egressPackets' instead"  #-}

-- | The number of bytes sent by the client.
--
-- /Note:/ Consider using 'ingressBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcIngressBytes :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcIngressBytes = Lens.field @"ingressBytes"
{-# INLINEABLE cvcIngressBytes #-}
{-# DEPRECATED ingressBytes "Use generic-lens or generic-optics with 'ingressBytes' instead"  #-}

-- | The number of packets sent by the client.
--
-- /Note:/ Consider using 'ingressPackets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcIngressPackets :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcIngressPackets = Lens.field @"ingressPackets"
{-# INLINEABLE cvcIngressPackets #-}
{-# DEPRECATED ingressPackets "Use generic-lens or generic-optics with 'ingressPackets' instead"  #-}

-- | The statuses returned by the client connect handler for posture compliance, if applicable.
--
-- /Note:/ Consider using 'postureComplianceStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcPostureComplianceStatuses :: Lens.Lens' ClientVpnConnection (Core.Maybe [Core.Text])
cvcPostureComplianceStatuses = Lens.field @"postureComplianceStatuses"
{-# INLINEABLE cvcPostureComplianceStatuses #-}
{-# DEPRECATED postureComplianceStatuses "Use generic-lens or generic-optics with 'postureComplianceStatuses' instead"  #-}

-- | The current state of the client connection.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcStatus :: Lens.Lens' ClientVpnConnection (Core.Maybe Types.ClientVpnConnectionStatus)
cvcStatus = Lens.field @"status"
{-# INLINEABLE cvcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The current date and time.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcTimestamp :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcTimestamp = Lens.field @"timestamp"
{-# INLINEABLE cvcTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

-- | The username of the client who established the client connection. This information is only provided if Active Directory client authentication is used.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcUsername :: Lens.Lens' ClientVpnConnection (Core.Maybe Core.Text)
cvcUsername = Lens.field @"username"
{-# INLINEABLE cvcUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromXML ClientVpnConnection where
        parseXML x
          = ClientVpnConnection' Core.<$>
              (x Core..@? "clientIp") Core.<*> x Core..@? "clientVpnEndpointId"
                Core.<*> x Core..@? "commonName"
                Core.<*> x Core..@? "connectionEndTime"
                Core.<*> x Core..@? "connectionEstablishedTime"
                Core.<*> x Core..@? "connectionId"
                Core.<*> x Core..@? "egressBytes"
                Core.<*> x Core..@? "egressPackets"
                Core.<*> x Core..@? "ingressBytes"
                Core.<*> x Core..@? "ingressPackets"
                Core.<*>
                x Core..@? "postureComplianceStatusSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "timestamp"
                Core.<*> x Core..@? "username"
