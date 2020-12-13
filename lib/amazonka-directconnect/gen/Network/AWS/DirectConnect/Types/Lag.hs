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
    lfLagId,
    lfConnectionsBandwidth,
    lfMinimumLinks,
    lfLagName,
    lfLocation,
    lfConnections,
    lfAwsDevice,
    lfHasLogicalRedundancy,
    lfAllowsHostedConnections,
    lfNumberOfConnections,
    lfJumboFrameCapable,
    lfLagState,
    lfOwnerAccount,
    lfRegion,
    lfProviderName,
    lfAwsDeviceV2,
    lfTags,
  )
where

import Network.AWS.DirectConnect.Types.Connection
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.LagState
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a link aggregation group (LAG).
--
-- /See:/ 'mkLag' smart constructor.
data Lag = Lag'
  { -- | The ID of the LAG.
    lagId :: Lude.Maybe Lude.Text,
    -- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Lude.Maybe Lude.Text,
    -- | The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
    minimumLinks :: Lude.Maybe Lude.Int,
    -- | The name of the LAG.
    lagName :: Lude.Maybe Lude.Text,
    -- | The location of the LAG.
    location :: Lude.Maybe Lude.Text,
    -- | The connections bundled by the LAG.
    connections :: Lude.Maybe [Connection],
    -- | The AWS Direct Connect endpoint that hosts the LAG.
    awsDevice :: Lude.Maybe Lude.Text,
    -- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
    hasLogicalRedundancy :: Lude.Maybe HasLogicalRedundancy,
    -- | Indicates whether the LAG can host other connections.
    allowsHostedConnections :: Lude.Maybe Lude.Bool,
    -- | The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
    numberOfConnections :: Lude.Maybe Lude.Int,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Lude.Maybe Lude.Bool,
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
    lagState :: Lude.Maybe LagState,
    -- | The ID of the AWS account that owns the LAG.
    ownerAccount :: Lude.Maybe Lude.Text,
    -- | The AWS Region where the connection is located.
    region :: Lude.Maybe Lude.Text,
    -- | The name of the service provider associated with the LAG.
    providerName :: Lude.Maybe Lude.Text,
    -- | The AWS Direct Connect endpoint that hosts the LAG.
    awsDeviceV2 :: Lude.Maybe Lude.Text,
    -- | The tags associated with the LAG.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Lag' with the minimum fields required to make a request.
--
-- * 'lagId' - The ID of the LAG.
-- * 'connectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
-- * 'minimumLinks' - The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
-- * 'lagName' - The name of the LAG.
-- * 'location' - The location of the LAG.
-- * 'connections' - The connections bundled by the LAG.
-- * 'awsDevice' - The AWS Direct Connect endpoint that hosts the LAG.
-- * 'hasLogicalRedundancy' - Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
-- * 'allowsHostedConnections' - Indicates whether the LAG can host other connections.
-- * 'numberOfConnections' - The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
-- * 'jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
-- * 'lagState' - The state of the LAG. The following are the possible values:
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
-- * 'ownerAccount' - The ID of the AWS account that owns the LAG.
-- * 'region' - The AWS Region where the connection is located.
-- * 'providerName' - The name of the service provider associated with the LAG.
-- * 'awsDeviceV2' - The AWS Direct Connect endpoint that hosts the LAG.
-- * 'tags' - The tags associated with the LAG.
mkLag ::
  Lag
mkLag =
  Lag'
    { lagId = Lude.Nothing,
      connectionsBandwidth = Lude.Nothing,
      minimumLinks = Lude.Nothing,
      lagName = Lude.Nothing,
      location = Lude.Nothing,
      connections = Lude.Nothing,
      awsDevice = Lude.Nothing,
      hasLogicalRedundancy = Lude.Nothing,
      allowsHostedConnections = Lude.Nothing,
      numberOfConnections = Lude.Nothing,
      jumboFrameCapable = Lude.Nothing,
      lagState = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      region = Lude.Nothing,
      providerName = Lude.Nothing,
      awsDeviceV2 = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLagId :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfLagId = Lens.lens (lagId :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: Lag)
{-# DEPRECATED lfLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The individual bandwidth of the physical connections bundled by the LAG. The possible values are 1Gbps and 10Gbps.
--
-- /Note:/ Consider using 'connectionsBandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfConnectionsBandwidth :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfConnectionsBandwidth = Lens.lens (connectionsBandwidth :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {connectionsBandwidth = a} :: Lag)
{-# DEPRECATED lfConnectionsBandwidth "Use generic-lens or generic-optics with 'connectionsBandwidth' instead." #-}

-- | The minimum number of physical dedicated connections that must be operational for the LAG itself to be operational.
--
-- /Note:/ Consider using 'minimumLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfMinimumLinks :: Lens.Lens' Lag (Lude.Maybe Lude.Int)
lfMinimumLinks = Lens.lens (minimumLinks :: Lag -> Lude.Maybe Lude.Int) (\s a -> s {minimumLinks = a} :: Lag)
{-# DEPRECATED lfMinimumLinks "Use generic-lens or generic-optics with 'minimumLinks' instead." #-}

-- | The name of the LAG.
--
-- /Note:/ Consider using 'lagName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLagName :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfLagName = Lens.lens (lagName :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {lagName = a} :: Lag)
{-# DEPRECATED lfLagName "Use generic-lens or generic-optics with 'lagName' instead." #-}

-- | The location of the LAG.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLocation :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfLocation = Lens.lens (location :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: Lag)
{-# DEPRECATED lfLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The connections bundled by the LAG.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfConnections :: Lens.Lens' Lag (Lude.Maybe [Connection])
lfConnections = Lens.lens (connections :: Lag -> Lude.Maybe [Connection]) (\s a -> s {connections = a} :: Lag)
{-# DEPRECATED lfConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | The AWS Direct Connect endpoint that hosts the LAG.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAwsDevice :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfAwsDevice = Lens.lens (awsDevice :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {awsDevice = a} :: Lag)
{-# DEPRECATED lfAwsDevice "Use generic-lens or generic-optics with 'awsDevice' instead." #-}

-- | Indicates whether the LAG supports a secondary BGP peer in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfHasLogicalRedundancy :: Lens.Lens' Lag (Lude.Maybe HasLogicalRedundancy)
lfHasLogicalRedundancy = Lens.lens (hasLogicalRedundancy :: Lag -> Lude.Maybe HasLogicalRedundancy) (\s a -> s {hasLogicalRedundancy = a} :: Lag)
{-# DEPRECATED lfHasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead." #-}

-- | Indicates whether the LAG can host other connections.
--
-- /Note:/ Consider using 'allowsHostedConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAllowsHostedConnections :: Lens.Lens' Lag (Lude.Maybe Lude.Bool)
lfAllowsHostedConnections = Lens.lens (allowsHostedConnections :: Lag -> Lude.Maybe Lude.Bool) (\s a -> s {allowsHostedConnections = a} :: Lag)
{-# DEPRECATED lfAllowsHostedConnections "Use generic-lens or generic-optics with 'allowsHostedConnections' instead." #-}

-- | The number of physical dedicated connections bundled by the LAG, up to a maximum of 10.
--
-- /Note:/ Consider using 'numberOfConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNumberOfConnections :: Lens.Lens' Lag (Lude.Maybe Lude.Int)
lfNumberOfConnections = Lens.lens (numberOfConnections :: Lag -> Lude.Maybe Lude.Int) (\s a -> s {numberOfConnections = a} :: Lag)
{-# DEPRECATED lfNumberOfConnections "Use generic-lens or generic-optics with 'numberOfConnections' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfJumboFrameCapable :: Lens.Lens' Lag (Lude.Maybe Lude.Bool)
lfJumboFrameCapable = Lens.lens (jumboFrameCapable :: Lag -> Lude.Maybe Lude.Bool) (\s a -> s {jumboFrameCapable = a} :: Lag)
{-# DEPRECATED lfJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

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
lfLagState :: Lens.Lens' Lag (Lude.Maybe LagState)
lfLagState = Lens.lens (lagState :: Lag -> Lude.Maybe LagState) (\s a -> s {lagState = a} :: Lag)
{-# DEPRECATED lfLagState "Use generic-lens or generic-optics with 'lagState' instead." #-}

-- | The ID of the AWS account that owns the LAG.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfOwnerAccount :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfOwnerAccount = Lens.lens (ownerAccount :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: Lag)
{-# DEPRECATED lfOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfRegion :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfRegion = Lens.lens (region :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Lag)
{-# DEPRECATED lfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the service provider associated with the LAG.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfProviderName :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfProviderName = Lens.lens (providerName :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: Lag)
{-# DEPRECATED lfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The AWS Direct Connect endpoint that hosts the LAG.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfAwsDeviceV2 :: Lens.Lens' Lag (Lude.Maybe Lude.Text)
lfAwsDeviceV2 = Lens.lens (awsDeviceV2 :: Lag -> Lude.Maybe Lude.Text) (\s a -> s {awsDeviceV2 = a} :: Lag)
{-# DEPRECATED lfAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The tags associated with the LAG.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfTags :: Lens.Lens' Lag (Lude.Maybe (Lude.NonEmpty Tag))
lfTags = Lens.lens (tags :: Lag -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: Lag)
{-# DEPRECATED lfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Lag where
  parseJSON =
    Lude.withObject
      "Lag"
      ( \x ->
          Lag'
            Lude.<$> (x Lude..:? "lagId")
            Lude.<*> (x Lude..:? "connectionsBandwidth")
            Lude.<*> (x Lude..:? "minimumLinks")
            Lude.<*> (x Lude..:? "lagName")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "connections" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "awsDevice")
            Lude.<*> (x Lude..:? "hasLogicalRedundancy")
            Lude.<*> (x Lude..:? "allowsHostedConnections")
            Lude.<*> (x Lude..:? "numberOfConnections")
            Lude.<*> (x Lude..:? "jumboFrameCapable")
            Lude.<*> (x Lude..:? "lagState")
            Lude.<*> (x Lude..:? "ownerAccount")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "providerName")
            Lude.<*> (x Lude..:? "awsDeviceV2")
            Lude.<*> (x Lude..:? "tags")
      )
