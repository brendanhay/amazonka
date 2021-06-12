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
-- Module      : Network.AWS.DirectConnect.Types.BGPPeer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPPeer where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.BGPPeerState
import Network.AWS.DirectConnect.Types.BGPStatus
import qualified Network.AWS.Lens as Lens

-- | Information about a BGP peer.
--
-- /See:/ 'newBGPPeer' smart constructor.
data BGPPeer = BGPPeer'
  { -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Core.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Core.Maybe Core.Int,
    -- | The Direct Connect endpoint on which the BGP peer terminates.
    awsDeviceV2 :: Core.Maybe Core.Text,
    -- | The ID of the BGP peer.
    bgpPeerId :: Core.Maybe Core.Text,
    -- | The status of the BGP peer. The following are the possible values:
    --
    -- -   @up@: The BGP peer is established. This state does not indicate the
    --     state of the routing function. Ensure that you are receiving routes
    --     over the BGP session.
    --
    -- -   @down@: The BGP peer is down.
    --
    -- -   @unknown@: The BGP peer status is not available.
    bgpStatus :: Core.Maybe BGPStatus,
    -- | The state of the BGP peer. The following are the possible values:
    --
    -- -   @verifying@: The BGP peering addresses or ASN require validation
    --     before the BGP peer can be created. This state applies only to
    --     public virtual interfaces.
    --
    -- -   @pending@: The BGP peer is created, and remains in this state until
    --     it is ready to be established.
    --
    -- -   @available@: The BGP peer is ready to be established.
    --
    -- -   @deleting@: The BGP peer is being deleted.
    --
    -- -   @deleted@: The BGP peer is deleted and cannot be established.
    bgpPeerState :: Core.Maybe BGPPeerState,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Core.Text,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authKey', 'bGPPeer_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'asn', 'bGPPeer_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- 'awsDeviceV2', 'bGPPeer_awsDeviceV2' - The Direct Connect endpoint on which the BGP peer terminates.
--
-- 'bgpPeerId', 'bGPPeer_bgpPeerId' - The ID of the BGP peer.
--
-- 'bgpStatus', 'bGPPeer_bgpStatus' - The status of the BGP peer. The following are the possible values:
--
-- -   @up@: The BGP peer is established. This state does not indicate the
--     state of the routing function. Ensure that you are receiving routes
--     over the BGP session.
--
-- -   @down@: The BGP peer is down.
--
-- -   @unknown@: The BGP peer status is not available.
--
-- 'bgpPeerState', 'bGPPeer_bgpPeerState' - The state of the BGP peer. The following are the possible values:
--
-- -   @verifying@: The BGP peering addresses or ASN require validation
--     before the BGP peer can be created. This state applies only to
--     public virtual interfaces.
--
-- -   @pending@: The BGP peer is created, and remains in this state until
--     it is ready to be established.
--
-- -   @available@: The BGP peer is ready to be established.
--
-- -   @deleting@: The BGP peer is being deleted.
--
-- -   @deleted@: The BGP peer is deleted and cannot be established.
--
-- 'addressFamily', 'bGPPeer_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'bGPPeer_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'customerAddress', 'bGPPeer_customerAddress' - The IP address assigned to the customer interface.
newBGPPeer ::
  BGPPeer
newBGPPeer =
  BGPPeer'
    { authKey = Core.Nothing,
      asn = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      bgpPeerId = Core.Nothing,
      bgpStatus = Core.Nothing,
      bgpPeerState = Core.Nothing,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      customerAddress = Core.Nothing
    }

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
bGPPeer_authKey :: Lens.Lens' BGPPeer (Core.Maybe Core.Text)
bGPPeer_authKey = Lens.lens (\BGPPeer' {authKey} -> authKey) (\s@BGPPeer' {} a -> s {authKey = a} :: BGPPeer)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
bGPPeer_asn :: Lens.Lens' BGPPeer (Core.Maybe Core.Int)
bGPPeer_asn = Lens.lens (\BGPPeer' {asn} -> asn) (\s@BGPPeer' {} a -> s {asn = a} :: BGPPeer)

-- | The Direct Connect endpoint on which the BGP peer terminates.
bGPPeer_awsDeviceV2 :: Lens.Lens' BGPPeer (Core.Maybe Core.Text)
bGPPeer_awsDeviceV2 = Lens.lens (\BGPPeer' {awsDeviceV2} -> awsDeviceV2) (\s@BGPPeer' {} a -> s {awsDeviceV2 = a} :: BGPPeer)

-- | The ID of the BGP peer.
bGPPeer_bgpPeerId :: Lens.Lens' BGPPeer (Core.Maybe Core.Text)
bGPPeer_bgpPeerId = Lens.lens (\BGPPeer' {bgpPeerId} -> bgpPeerId) (\s@BGPPeer' {} a -> s {bgpPeerId = a} :: BGPPeer)

-- | The status of the BGP peer. The following are the possible values:
--
-- -   @up@: The BGP peer is established. This state does not indicate the
--     state of the routing function. Ensure that you are receiving routes
--     over the BGP session.
--
-- -   @down@: The BGP peer is down.
--
-- -   @unknown@: The BGP peer status is not available.
bGPPeer_bgpStatus :: Lens.Lens' BGPPeer (Core.Maybe BGPStatus)
bGPPeer_bgpStatus = Lens.lens (\BGPPeer' {bgpStatus} -> bgpStatus) (\s@BGPPeer' {} a -> s {bgpStatus = a} :: BGPPeer)

-- | The state of the BGP peer. The following are the possible values:
--
-- -   @verifying@: The BGP peering addresses or ASN require validation
--     before the BGP peer can be created. This state applies only to
--     public virtual interfaces.
--
-- -   @pending@: The BGP peer is created, and remains in this state until
--     it is ready to be established.
--
-- -   @available@: The BGP peer is ready to be established.
--
-- -   @deleting@: The BGP peer is being deleted.
--
-- -   @deleted@: The BGP peer is deleted and cannot be established.
bGPPeer_bgpPeerState :: Lens.Lens' BGPPeer (Core.Maybe BGPPeerState)
bGPPeer_bgpPeerState = Lens.lens (\BGPPeer' {bgpPeerState} -> bgpPeerState) (\s@BGPPeer' {} a -> s {bgpPeerState = a} :: BGPPeer)

-- | The address family for the BGP peer.
bGPPeer_addressFamily :: Lens.Lens' BGPPeer (Core.Maybe AddressFamily)
bGPPeer_addressFamily = Lens.lens (\BGPPeer' {addressFamily} -> addressFamily) (\s@BGPPeer' {} a -> s {addressFamily = a} :: BGPPeer)

-- | The IP address assigned to the Amazon interface.
bGPPeer_amazonAddress :: Lens.Lens' BGPPeer (Core.Maybe Core.Text)
bGPPeer_amazonAddress = Lens.lens (\BGPPeer' {amazonAddress} -> amazonAddress) (\s@BGPPeer' {} a -> s {amazonAddress = a} :: BGPPeer)

-- | The IP address assigned to the customer interface.
bGPPeer_customerAddress :: Lens.Lens' BGPPeer (Core.Maybe Core.Text)
bGPPeer_customerAddress = Lens.lens (\BGPPeer' {customerAddress} -> customerAddress) (\s@BGPPeer' {} a -> s {customerAddress = a} :: BGPPeer)

instance Core.FromJSON BGPPeer where
  parseJSON =
    Core.withObject
      "BGPPeer"
      ( \x ->
          BGPPeer'
            Core.<$> (x Core..:? "authKey")
            Core.<*> (x Core..:? "asn")
            Core.<*> (x Core..:? "awsDeviceV2")
            Core.<*> (x Core..:? "bgpPeerId")
            Core.<*> (x Core..:? "bgpStatus")
            Core.<*> (x Core..:? "bgpPeerState")
            Core.<*> (x Core..:? "addressFamily")
            Core.<*> (x Core..:? "amazonAddress")
            Core.<*> (x Core..:? "customerAddress")
      )

instance Core.Hashable BGPPeer

instance Core.NFData BGPPeer
