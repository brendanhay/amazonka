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
    cLagId,
    cVlan,
    cLocation,
    cAwsDevice,
    cHasLogicalRedundancy,
    cConnectionId,
    cLoaIssueTime,
    cPartnerName,
    cConnectionName,
    cBandwidth,
    cJumboFrameCapable,
    cOwnerAccount,
    cRegion,
    cProviderName,
    cAwsDeviceV2,
    cConnectionState,
    cTags,
  )
where

import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an AWS Direct Connect connection.
--
-- /See:/ 'mkConnection' smart constructor.
data Connection = Connection'
  { lagId :: Lude.Maybe Lude.Text,
    vlan :: Lude.Maybe Lude.Int,
    location :: Lude.Maybe Lude.Text,
    awsDevice :: Lude.Maybe Lude.Text,
    hasLogicalRedundancy :: Lude.Maybe HasLogicalRedundancy,
    connectionId :: Lude.Maybe Lude.Text,
    loaIssueTime :: Lude.Maybe Lude.Timestamp,
    partnerName :: Lude.Maybe Lude.Text,
    connectionName :: Lude.Maybe Lude.Text,
    bandwidth :: Lude.Maybe Lude.Text,
    jumboFrameCapable :: Lude.Maybe Lude.Bool,
    ownerAccount :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    providerName :: Lude.Maybe Lude.Text,
    awsDeviceV2 :: Lude.Maybe Lude.Text,
    connectionState :: Lude.Maybe ConnectionState,
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- * 'awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
-- * 'awsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
-- * 'bandwidth' - The bandwidth of the connection.
-- * 'connectionId' - The ID of the connection.
-- * 'connectionName' - The name of the connection.
-- * 'connectionState' - The state of the connection. The following are the possible values:
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
-- * 'hasLogicalRedundancy' - Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
-- * 'jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
-- * 'lagId' - The ID of the LAG.
-- * 'loaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
-- * 'location' - The location of the connection.
-- * 'ownerAccount' - The ID of the AWS account that owns the connection.
-- * 'partnerName' - The name of the AWS Direct Connect service provider associated with the connection.
-- * 'providerName' - The name of the service provider associated with the connection.
-- * 'region' - The AWS Region where the connection is located.
-- * 'tags' - The tags associated with the connection.
-- * 'vlan' - The ID of the VLAN.
mkConnection ::
  Connection
mkConnection =
  Connection'
    { lagId = Lude.Nothing,
      vlan = Lude.Nothing,
      location = Lude.Nothing,
      awsDevice = Lude.Nothing,
      hasLogicalRedundancy = Lude.Nothing,
      connectionId = Lude.Nothing,
      loaIssueTime = Lude.Nothing,
      partnerName = Lude.Nothing,
      connectionName = Lude.Nothing,
      bandwidth = Lude.Nothing,
      jumboFrameCapable = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      region = Lude.Nothing,
      providerName = Lude.Nothing,
      awsDeviceV2 = Lude.Nothing,
      connectionState = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLagId :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cLagId = Lens.lens (lagId :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: Connection)
{-# DEPRECATED cLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVlan :: Lens.Lens' Connection (Lude.Maybe Lude.Int)
cVlan = Lens.lens (vlan :: Connection -> Lude.Maybe Lude.Int) (\s a -> s {vlan = a} :: Connection)
{-# DEPRECATED cVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLocation :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cLocation = Lens.lens (location :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: Connection)
{-# DEPRECATED cLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsDevice :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cAwsDevice = Lens.lens (awsDevice :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {awsDevice = a} :: Connection)
{-# DEPRECATED cAwsDevice "Use generic-lens or generic-optics with 'awsDevice' instead." #-}

-- | Indicates whether the connection supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHasLogicalRedundancy :: Lens.Lens' Connection (Lude.Maybe HasLogicalRedundancy)
cHasLogicalRedundancy = Lens.lens (hasLogicalRedundancy :: Connection -> Lude.Maybe HasLogicalRedundancy) (\s a -> s {hasLogicalRedundancy = a} :: Connection)
{-# DEPRECATED cHasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead." #-}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectionId :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cConnectionId = Lens.lens (connectionId :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: Connection)
{-# DEPRECATED cConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The time of the most recent call to 'DescribeLoa' for this connection.
--
-- /Note:/ Consider using 'loaIssueTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLoaIssueTime :: Lens.Lens' Connection (Lude.Maybe Lude.Timestamp)
cLoaIssueTime = Lens.lens (loaIssueTime :: Connection -> Lude.Maybe Lude.Timestamp) (\s a -> s {loaIssueTime = a} :: Connection)
{-# DEPRECATED cLoaIssueTime "Use generic-lens or generic-optics with 'loaIssueTime' instead." #-}

-- | The name of the AWS Direct Connect service provider associated with the connection.
--
-- /Note:/ Consider using 'partnerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPartnerName :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cPartnerName = Lens.lens (partnerName :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {partnerName = a} :: Connection)
{-# DEPRECATED cPartnerName "Use generic-lens or generic-optics with 'partnerName' instead." #-}

-- | The name of the connection.
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnectionName :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cConnectionName = Lens.lens (connectionName :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {connectionName = a} :: Connection)
{-# DEPRECATED cConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | The bandwidth of the connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBandwidth :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cBandwidth = Lens.lens (bandwidth :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {bandwidth = a} :: Connection)
{-# DEPRECATED cBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJumboFrameCapable :: Lens.Lens' Connection (Lude.Maybe Lude.Bool)
cJumboFrameCapable = Lens.lens (jumboFrameCapable :: Connection -> Lude.Maybe Lude.Bool) (\s a -> s {jumboFrameCapable = a} :: Connection)
{-# DEPRECATED cJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

-- | The ID of the AWS account that owns the connection.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cOwnerAccount :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cOwnerAccount = Lens.lens (ownerAccount :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: Connection)
{-# DEPRECATED cOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegion :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cRegion = Lens.lens (region :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Connection)
{-# DEPRECATED cRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the service provider associated with the connection.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cProviderName :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cProviderName = Lens.lens (providerName :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: Connection)
{-# DEPRECATED cProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAwsDeviceV2 :: Lens.Lens' Connection (Lude.Maybe Lude.Text)
cAwsDeviceV2 = Lens.lens (awsDeviceV2 :: Connection -> Lude.Maybe Lude.Text) (\s a -> s {awsDeviceV2 = a} :: Connection)
{-# DEPRECATED cAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

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
cConnectionState :: Lens.Lens' Connection (Lude.Maybe ConnectionState)
cConnectionState = Lens.lens (connectionState :: Connection -> Lude.Maybe ConnectionState) (\s a -> s {connectionState = a} :: Connection)
{-# DEPRECATED cConnectionState "Use generic-lens or generic-optics with 'connectionState' instead." #-}

-- | The tags associated with the connection.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Connection (Lude.Maybe (Lude.NonEmpty Tag))
cTags = Lens.lens (tags :: Connection -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: Connection)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Connection where
  parseJSON =
    Lude.withObject
      "Connection"
      ( \x ->
          Connection'
            Lude.<$> (x Lude..:? "lagId")
            Lude.<*> (x Lude..:? "vlan")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "awsDevice")
            Lude.<*> (x Lude..:? "hasLogicalRedundancy")
            Lude.<*> (x Lude..:? "connectionId")
            Lude.<*> (x Lude..:? "loaIssueTime")
            Lude.<*> (x Lude..:? "partnerName")
            Lude.<*> (x Lude..:? "connectionName")
            Lude.<*> (x Lude..:? "bandwidth")
            Lude.<*> (x Lude..:? "jumboFrameCapable")
            Lude.<*> (x Lude..:? "ownerAccount")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "providerName")
            Lude.<*> (x Lude..:? "awsDeviceV2")
            Lude.<*> (x Lude..:? "connectionState")
            Lude.<*> (x Lude..:? "tags")
      )
