{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Lag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Lag
  ( Lag (..),

    -- * Smart constructor
    mkLag,

    -- * Lenses
    lfAllowsHostedConnections,
    lfAwsDevice,
    lfAwsDeviceV2,
    lfConnections,
    lfConnectionsBandwidth,
    lfHasLogicalRedundancy,
    lfJumboFrameCapable,
    lfLagId,
    lfLagName,
    lfLagState,
    lfLocation,
    lfMinimumLinks,
    lfNumberOfConnections,
    lfOwnerAccount,
    lfProviderName,
    lfRegion,
    lfTags,
  )
where

import qualified Network.AWS.DirectConnect.Types.AwsDevice as Types
import qualified Network.AWS.DirectConnect.Types.AwsDeviceV2 as Types
import qualified Network.AWS.DirectConnect.Types.Bandwidth as Types
import qualified Network.AWS.DirectConnect.Types.Connection as Types
import qualified Network.AWS.DirectConnect.Types.HasLogicalRedundancy as Types
import qualified Network.AWS.DirectConnect.Types.LagId as Types
import qualified Network.AWS.DirectConnect.Types.LagName as Types
import qualified Network.AWS.DirectConnect.Types.LagState as Types
import qualified Network.AWS.DirectConnect.Types.LocationCode as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.ProviderName as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a link aggregation group (LAG).
--
-- /See:/ 'mkLag' smart constructor.
data Lag = Lag'
  { -- | Indicates whether the LAG can host other connections.
    allowsHostedConnections :: Core.Maybe Core.Bool,
    -- | The AWS Direct Connect endpoint that hosts the LAG.
    awsDevice :: Core.Maybe Types.AwsDevice,
    -- | The AWS Direct Connect endpoint that hosts the LAG.
    awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2,
    -- | The connections bundled by the LAG.
    connections :: Core.Maybe [Types.Connection],
    -- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Core.Maybe Types.Bandwidth,
    -- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
    hasLogicalRedundancy :: Core.Maybe Types.HasLogicalRedundancy,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Core.Maybe Core.Bool,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Types.LagId,
    -- | The name of the LAG.
    lagName :: Core.Maybe Types.LagName,
    -- | The state of the LAG. The following are the possible values:
    --
    --
    --     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.
    --
    --
    --     * @pending@ : The LAG has been approved and is being initialized.
    --
    --
    --     * @available@ : The network link is established and the LAG is ready for use.
    --
    --
    --     * @down@ : The network link is down.
    --
    --
    --     * @deleting@ : The LAG is being deleted.
    --
    --
    --     * @deleted@ : The LAG is deleted.
    --
    --
    --     * @unknown@ : The state of the LAG is not available.
    lagState :: Core.Maybe Types.LagState,
    -- | The location of the LAG.
    location :: Core.Maybe Types.LocationCode,
    -- | The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
    minimumLinks :: Core.Maybe Core.Int,
    -- | The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
    numberOfConnections :: Core.Maybe Core.Int,
    -- | The ID of the AWS account that owns the LAG.
    ownerAccount :: Core.Maybe Types.OwnerAccount,
    -- | The name of the service provider associated with the LAG.
    providerName :: Core.Maybe Types.ProviderName,
    -- | The AWS Region where the connection is located.
    region :: Core.Maybe Types.Region,
    -- | The tags associated with the LAG.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Lag' value with any optional fields omitted.
mkLag ::
  Lag
mkLag =
  Lag'
    { allowsHostedConnections = Core.Nothing,
      awsDevice = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      connections = Core.Nothing,
      connectionsBandwidth = Core.Nothing,
      hasLogicalRedundancy = Core.Nothing,
      jumboFrameCapable = Core.Nothing,
      lagId = Core.Nothing,
      lagName = Core.Nothing,
      lagState = Core.Nothing,
      location = Core.Nothing,
      minimumLinks = Core.Nothing,
      numberOfConnections = Core.Nothing,
      ownerAccount = Core.Nothing,
      providerName = Core.Nothing,
      region = Core.Nothing,
      tags = Core.Nothing
    }

-- | Indicates whether the LAG can host other connections.
--
-- /Note:/ Consider using 'allowsHostedConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAllowsHostedConnections :: Lens.Lens' Lag (Core.Maybe Core.Bool)
lfAllowsHostedConnections = Lens.field @"allowsHostedConnections"
{-# DEPRECATED lfAllowsHostedConnections "Use generic-lens or generic-optics with 'allowsHostedConnections' instead." #-}

-- | The AWS Direct Connect endpoint that hosts the LAG.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAwsDevice :: Lens.Lens' Lag (Core.Maybe Types.AwsDevice)
lfAwsDevice = Lens.field @"awsDevice"
{-# DEPRECATED lfAwsDevice "Use generic-lens or generic-optics with 'awsDevice' instead." #-}

-- | The AWS Direct Connect endpoint that hosts the LAG.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAwsDeviceV2 :: Lens.Lens' Lag (Core.Maybe Types.AwsDeviceV2)
lfAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# DEPRECATED lfAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The connections bundled by the LAG.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfConnections :: Lens.Lens' Lag (Core.Maybe [Types.Connection])
lfConnections = Lens.field @"connections"
{-# DEPRECATED lfConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- /Note:/ Consider using 'connectionsBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfConnectionsBandwidth :: Lens.Lens' Lag (Core.Maybe Types.Bandwidth)
lfConnectionsBandwidth = Lens.field @"connectionsBandwidth"
{-# DEPRECATED lfConnectionsBandwidth "Use generic-lens or generic-optics with 'connectionsBandwidth' instead." #-}

-- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfHasLogicalRedundancy :: Lens.Lens' Lag (Core.Maybe Types.HasLogicalRedundancy)
lfHasLogicalRedundancy = Lens.field @"hasLogicalRedundancy"
{-# DEPRECATED lfHasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfJumboFrameCapable :: Lens.Lens' Lag (Core.Maybe Core.Bool)
lfJumboFrameCapable = Lens.field @"jumboFrameCapable"
{-# DEPRECATED lfJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLagId :: Lens.Lens' Lag (Core.Maybe Types.LagId)
lfLagId = Lens.field @"lagId"
{-# DEPRECATED lfLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLagName :: Lens.Lens' Lag (Core.Maybe Types.LagName)
lfLagName = Lens.field @"lagName"
{-# DEPRECATED lfLagName "Use generic-lens or generic-optics with 'lagName' instead." #-}

-- | The state of the LAG. The following are the possible values:
--
--
--     * @requested@ : The initial state of a LAG. The LAG stays in the requested state until the Letter of Authorization (LOA) is available.
--
--
--     * @pending@ : The LAG has been approved and is being initialized.
--
--
--     * @available@ : The network link is established and the LAG is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The LAG is being deleted.
--
--
--     * @deleted@ : The LAG is deleted.
--
--
--     * @unknown@ : The state of the LAG is not available.
--
--
--
-- /Note:/ Consider using 'lagState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLagState :: Lens.Lens' Lag (Core.Maybe Types.LagState)
lfLagState = Lens.field @"lagState"
{-# DEPRECATED lfLagState "Use generic-lens or generic-optics with 'lagState' instead." #-}

-- | The location of the LAG.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLocation :: Lens.Lens' Lag (Core.Maybe Types.LocationCode)
lfLocation = Lens.field @"location"
{-# DEPRECATED lfLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
--
-- /Note:/ Consider using 'minimumLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMinimumLinks :: Lens.Lens' Lag (Core.Maybe Core.Int)
lfMinimumLinks = Lens.field @"minimumLinks"
{-# DEPRECATED lfMinimumLinks "Use generic-lens or generic-optics with 'minimumLinks' instead." #-}

-- | The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
--
-- /Note:/ Consider using 'numberOfConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNumberOfConnections :: Lens.Lens' Lag (Core.Maybe Core.Int)
lfNumberOfConnections = Lens.field @"numberOfConnections"
{-# DEPRECATED lfNumberOfConnections "Use generic-lens or generic-optics with 'numberOfConnections' instead." #-}

-- | The ID of the AWS account that owns the LAG.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfOwnerAccount :: Lens.Lens' Lag (Core.Maybe Types.OwnerAccount)
lfOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED lfOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The name of the service provider associated with the LAG.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfProviderName :: Lens.Lens' Lag (Core.Maybe Types.ProviderName)
lfProviderName = Lens.field @"providerName"
{-# DEPRECATED lfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfRegion :: Lens.Lens' Lag (Core.Maybe Types.Region)
lfRegion = Lens.field @"region"
{-# DEPRECATED lfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The tags associated with the LAG.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfTags :: Lens.Lens' Lag (Core.Maybe (Core.NonEmpty Types.Tag))
lfTags = Lens.field @"tags"
{-# DEPRECATED lfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Lag where
  parseJSON =
    Core.withObject "Lag" Core.$
      \x ->
        Lag'
          Core.<$> (x Core..:? "allowsHostedConnections")
          Core.<*> (x Core..:? "awsDevice")
          Core.<*> (x Core..:? "awsDeviceV2")
          Core.<*> (x Core..:? "connections")
          Core.<*> (x Core..:? "connectionsBandwidth")
          Core.<*> (x Core..:? "hasLogicalRedundancy")
          Core.<*> (x Core..:? "jumboFrameCapable")
          Core.<*> (x Core..:? "lagId")
          Core.<*> (x Core..:? "lagName")
          Core.<*> (x Core..:? "lagState")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "minimumLinks")
          Core.<*> (x Core..:? "numberOfConnections")
          Core.<*> (x Core..:? "ownerAccount")
          Core.<*> (x Core..:? "providerName")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "tags")
