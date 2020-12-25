{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Connection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connection
  ( Connection (..),

    -- * Smart constructor
    mkConnection,

    -- * Lenses
    cAwsDevice,
    cAwsDeviceV2,
    cBandwidth,
    cConnectionId,
    cConnectionName,
    cConnectionState,
    cHasLogicalRedundancy,
    cJumboFrameCapable,
    cLagId,
    cLoaIssueTime,
    cLocation,
    cOwnerAccount,
    cPartnerName,
    cProviderName,
    cRegion,
    cTags,
    cVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types.AwsDevice as Types
import qualified Network.AWS.DirectConnect.Types.AwsDeviceV2 as Types
import qualified Network.AWS.DirectConnect.Types.Bandwidth as Types
import qualified Network.AWS.DirectConnect.Types.ConnectionId as Types
import qualified Network.AWS.DirectConnect.Types.ConnectionName as Types
import qualified Network.AWS.DirectConnect.Types.ConnectionState as Types
import qualified Network.AWS.DirectConnect.Types.HasLogicalRedundancy as Types
import qualified Network.AWS.DirectConnect.Types.LagId as Types
import qualified Network.AWS.DirectConnect.Types.LocationCode as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.PartnerName as Types
import qualified Network.AWS.DirectConnect.Types.ProviderName as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an AWS Direct Connect connection.
--
-- /See:/ 'mkConnection' smart constructor.
data Connection = Connection'
  { -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Core.Maybe Types.AwsDevice,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2,
    -- | The bandwidth of the connection.
    bandwidth :: Core.Maybe Types.Bandwidth,
    -- | The ID of the connection.
    connectionId :: Core.Maybe Types.ConnectionId,
    -- | The name of the connection.
    connectionName :: Core.Maybe Types.ConnectionName,
    -- | The state of the connection. The following are the possible values:
    --
    --
    --     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
    --
    --
    --     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
    --
    --
    --     * @pending@ : The connection has been approved and is being initialized.
    --
    --
    --     * @available@ : The network link is up and the connection is ready for use.
    --
    --
    --     * @down@ : The network link is down.
    --
    --
    --     * @deleting@ : The connection is being deleted.
    --
    --
    --     * @deleted@ : The connection has been deleted.
    --
    --
    --     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
    --
    --
    --     * @unknown@ : The state of the connection is not available.
    connectionState :: Core.Maybe Types.ConnectionState,
    -- | Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
    hasLogicalRedundancy :: Core.Maybe Types.HasLogicalRedundancy,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Core.Maybe Core.Bool,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Types.LagId,
    -- | The time of the most recent call to 'DescribeLoa' for this connection.
    loaIssueTime :: Core.Maybe Core.NominalDiffTime,
    -- | The location of the connection.
    location :: Core.Maybe Types.LocationCode,
    -- | The ID of the AWS account that owns the connection.
    ownerAccount :: Core.Maybe Types.OwnerAccount,
    -- | The name of the AWS Direct Connect service provider associated with the connection.
    partnerName :: Core.Maybe Types.PartnerName,
    -- | The name of the service provider associated with the connection.
    providerName :: Core.Maybe Types.ProviderName,
    -- | The AWS Region where the connection is located.
    region :: Core.Maybe Types.Region,
    -- | The tags associated with the connection.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | The ID of the VLAN.
    vlan :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Connection' value with any optional fields omitted.
mkConnection ::
  Connection
mkConnection =
  Connection'
    { awsDevice = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      bandwidth = Core.Nothing,
      connectionId = Core.Nothing,
      connectionName = Core.Nothing,
      connectionState = Core.Nothing,
      hasLogicalRedundancy = Core.Nothing,
      jumboFrameCapable = Core.Nothing,
      lagId = Core.Nothing,
      loaIssueTime = Core.Nothing,
      location = Core.Nothing,
      ownerAccount = Core.Nothing,
      partnerName = Core.Nothing,
      providerName = Core.Nothing,
      region = Core.Nothing,
      tags = Core.Nothing,
      vlan = Core.Nothing
    }

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsDevice :: Lens.Lens' Connection (Core.Maybe Types.AwsDevice)
cAwsDevice = Lens.field @"awsDevice"
{-# DEPRECATED cAwsDevice "Use generic-lens or generic-optics with 'awsDevice' instead." #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsDeviceV2 :: Lens.Lens' Connection (Core.Maybe Types.AwsDeviceV2)
cAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# DEPRECATED cAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The bandwidth of the connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBandwidth :: Lens.Lens' Connection (Core.Maybe Types.Bandwidth)
cBandwidth = Lens.field @"bandwidth"
{-# DEPRECATED cBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectionId :: Lens.Lens' Connection (Core.Maybe Types.ConnectionId)
cConnectionId = Lens.field @"connectionId"
{-# DEPRECATED cConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The name of the connection.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectionName :: Lens.Lens' Connection (Core.Maybe Types.ConnectionName)
cConnectionName = Lens.field @"connectionName"
{-# DEPRECATED cConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | The state of the connection. The following are the possible values:
--
--
--     * @ordering@ : The initial state of a hosted connection provisioned on an interconnect. The connection stays in the ordering state until the owner of the hosted connection confirms or declines the connection order.
--
--
--     * @requested@ : The initial state of a standard connection. The connection stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The connection has been approved and is being initialized.
--
--
--     * @available@ : The network link is up and the connection is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The connection is being deleted.
--
--
--     * @deleted@ : The connection has been deleted.
--
--
--     * @rejected@ : A hosted connection in the @ordering@ state enters the @rejected@ state if it is deleted by the customer.
--
--
--     * @unknown@ : The state of the connection is not available.
--
--
--
-- /Note:/ Consider using 'connectionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectionState :: Lens.Lens' Connection (Core.Maybe Types.ConnectionState)
cConnectionState = Lens.field @"connectionState"
{-# DEPRECATED cConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHasLogicalRedundancy :: Lens.Lens' Connection (Core.Maybe Types.HasLogicalRedundancy)
cHasLogicalRedundancy = Lens.field @"hasLogicalRedundancy"
{-# DEPRECATED cHasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJumboFrameCapable :: Lens.Lens' Connection (Core.Maybe Core.Bool)
cJumboFrameCapable = Lens.field @"jumboFrameCapable"
{-# DEPRECATED cJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLagId :: Lens.Lens' Connection (Core.Maybe Types.LagId)
cLagId = Lens.field @"lagId"
{-# DEPRECATED cLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The time of the most recent call to 'DescribeLoa' for this connection.
--
-- /Note:/ Consider using 'loaIssueTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLoaIssueTime :: Lens.Lens' Connection (Core.Maybe Core.NominalDiffTime)
cLoaIssueTime = Lens.field @"loaIssueTime"
{-# DEPRECATED cLoaIssueTime "Use generic-lens or generic-optics with 'loaIssueTime' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLocation :: Lens.Lens' Connection (Core.Maybe Types.LocationCode)
cLocation = Lens.field @"location"
{-# DEPRECATED cLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The ID of the AWS account that owns the connection.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOwnerAccount :: Lens.Lens' Connection (Core.Maybe Types.OwnerAccount)
cOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED cOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The name of the AWS Direct Connect service provider associated with the connection.
--
-- /Note:/ Consider using 'partnerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPartnerName :: Lens.Lens' Connection (Core.Maybe Types.PartnerName)
cPartnerName = Lens.field @"partnerName"
{-# DEPRECATED cPartnerName "Use generic-lens or generic-optics with 'partnerName' instead." #-}

-- | The name of the service provider associated with the connection.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cProviderName :: Lens.Lens' Connection (Core.Maybe Types.ProviderName)
cProviderName = Lens.field @"providerName"
{-# DEPRECATED cProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegion :: Lens.Lens' Connection (Core.Maybe Types.Region)
cRegion = Lens.field @"region"
{-# DEPRECATED cRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The tags associated with the connection.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Connection (Core.Maybe (Core.NonEmpty Types.Tag))
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVlan :: Lens.Lens' Connection (Core.Maybe Core.Int)
cVlan = Lens.field @"vlan"
{-# DEPRECATED cVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject "Connection" Core.$
      \x ->
        Connection'
          Core.<$> (x Core..:? "awsDevice")
          Core.<*> (x Core..:? "awsDeviceV2")
          Core.<*> (x Core..:? "bandwidth")
          Core.<*> (x Core..:? "connectionId")
          Core.<*> (x Core..:? "connectionName")
          Core.<*> (x Core..:? "connectionState")
          Core.<*> (x Core..:? "hasLogicalRedundancy")
          Core.<*> (x Core..:? "jumboFrameCapable")
          Core.<*> (x Core..:? "lagId")
          Core.<*> (x Core..:? "loaIssueTime")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "ownerAccount")
          Core.<*> (x Core..:? "partnerName")
          Core.<*> (x Core..:? "providerName")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "vlan")
