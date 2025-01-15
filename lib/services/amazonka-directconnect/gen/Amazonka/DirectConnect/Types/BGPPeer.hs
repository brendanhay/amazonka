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
-- Module      : Amazonka.DirectConnect.Types.BGPPeer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.BGPPeer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.AddressFamily
import Amazonka.DirectConnect.Types.BGPPeerState
import Amazonka.DirectConnect.Types.BGPStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a BGP peer.
--
-- /See:/ 'newBGPPeer' smart constructor.
data BGPPeer = BGPPeer'
  { -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the BGP peer.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the BGP peer.
    bgpPeerId :: Prelude.Maybe Prelude.Text,
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
    bgpPeerState :: Prelude.Maybe BGPPeerState,
    -- | The status of the BGP peer. The following are the possible values:
    --
    -- -   @up@: The BGP peer is established. This state does not indicate the
    --     state of the routing function. Ensure that you are receiving routes
    --     over the BGP session.
    --
    -- -   @down@: The BGP peer is down.
    --
    -- -   @unknown@: The BGP peer status is not available.
    bgpStatus :: Prelude.Maybe BGPStatus,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BGPPeer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressFamily', 'bGPPeer_addressFamily' - The address family for the BGP peer.
--
-- 'amazonAddress', 'bGPPeer_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'asn', 'bGPPeer_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- 'authKey', 'bGPPeer_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'awsDeviceV2', 'bGPPeer_awsDeviceV2' - The Direct Connect endpoint that terminates the BGP peer.
--
-- 'awsLogicalDeviceId', 'bGPPeer_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'bgpPeerId', 'bGPPeer_bgpPeerId' - The ID of the BGP peer.
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
-- 'customerAddress', 'bGPPeer_customerAddress' - The IP address assigned to the customer interface.
newBGPPeer ::
  BGPPeer
newBGPPeer =
  BGPPeer'
    { addressFamily = Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      asn = Prelude.Nothing,
      authKey = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      bgpPeerId = Prelude.Nothing,
      bgpPeerState = Prelude.Nothing,
      bgpStatus = Prelude.Nothing,
      customerAddress = Prelude.Nothing
    }

-- | The address family for the BGP peer.
bGPPeer_addressFamily :: Lens.Lens' BGPPeer (Prelude.Maybe AddressFamily)
bGPPeer_addressFamily = Lens.lens (\BGPPeer' {addressFamily} -> addressFamily) (\s@BGPPeer' {} a -> s {addressFamily = a} :: BGPPeer)

-- | The IP address assigned to the Amazon interface.
bGPPeer_amazonAddress :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_amazonAddress = Lens.lens (\BGPPeer' {amazonAddress} -> amazonAddress) (\s@BGPPeer' {} a -> s {amazonAddress = a} :: BGPPeer)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
bGPPeer_asn :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Int)
bGPPeer_asn = Lens.lens (\BGPPeer' {asn} -> asn) (\s@BGPPeer' {} a -> s {asn = a} :: BGPPeer)

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
bGPPeer_authKey :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_authKey = Lens.lens (\BGPPeer' {authKey} -> authKey) (\s@BGPPeer' {} a -> s {authKey = a} :: BGPPeer)

-- | The Direct Connect endpoint that terminates the BGP peer.
bGPPeer_awsDeviceV2 :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_awsDeviceV2 = Lens.lens (\BGPPeer' {awsDeviceV2} -> awsDeviceV2) (\s@BGPPeer' {} a -> s {awsDeviceV2 = a} :: BGPPeer)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
bGPPeer_awsLogicalDeviceId :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_awsLogicalDeviceId = Lens.lens (\BGPPeer' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@BGPPeer' {} a -> s {awsLogicalDeviceId = a} :: BGPPeer)

-- | The ID of the BGP peer.
bGPPeer_bgpPeerId :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_bgpPeerId = Lens.lens (\BGPPeer' {bgpPeerId} -> bgpPeerId) (\s@BGPPeer' {} a -> s {bgpPeerId = a} :: BGPPeer)

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
bGPPeer_bgpPeerState :: Lens.Lens' BGPPeer (Prelude.Maybe BGPPeerState)
bGPPeer_bgpPeerState = Lens.lens (\BGPPeer' {bgpPeerState} -> bgpPeerState) (\s@BGPPeer' {} a -> s {bgpPeerState = a} :: BGPPeer)

-- | The status of the BGP peer. The following are the possible values:
--
-- -   @up@: The BGP peer is established. This state does not indicate the
--     state of the routing function. Ensure that you are receiving routes
--     over the BGP session.
--
-- -   @down@: The BGP peer is down.
--
-- -   @unknown@: The BGP peer status is not available.
bGPPeer_bgpStatus :: Lens.Lens' BGPPeer (Prelude.Maybe BGPStatus)
bGPPeer_bgpStatus = Lens.lens (\BGPPeer' {bgpStatus} -> bgpStatus) (\s@BGPPeer' {} a -> s {bgpStatus = a} :: BGPPeer)

-- | The IP address assigned to the customer interface.
bGPPeer_customerAddress :: Lens.Lens' BGPPeer (Prelude.Maybe Prelude.Text)
bGPPeer_customerAddress = Lens.lens (\BGPPeer' {customerAddress} -> customerAddress) (\s@BGPPeer' {} a -> s {customerAddress = a} :: BGPPeer)

instance Data.FromJSON BGPPeer where
  parseJSON =
    Data.withObject
      "BGPPeer"
      ( \x ->
          BGPPeer'
            Prelude.<$> (x Data..:? "addressFamily")
            Prelude.<*> (x Data..:? "amazonAddress")
            Prelude.<*> (x Data..:? "asn")
            Prelude.<*> (x Data..:? "authKey")
            Prelude.<*> (x Data..:? "awsDeviceV2")
            Prelude.<*> (x Data..:? "awsLogicalDeviceId")
            Prelude.<*> (x Data..:? "bgpPeerId")
            Prelude.<*> (x Data..:? "bgpPeerState")
            Prelude.<*> (x Data..:? "bgpStatus")
            Prelude.<*> (x Data..:? "customerAddress")
      )

instance Prelude.Hashable BGPPeer where
  hashWithSalt _salt BGPPeer' {..} =
    _salt
      `Prelude.hashWithSalt` addressFamily
      `Prelude.hashWithSalt` amazonAddress
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` authKey
      `Prelude.hashWithSalt` awsDeviceV2
      `Prelude.hashWithSalt` awsLogicalDeviceId
      `Prelude.hashWithSalt` bgpPeerId
      `Prelude.hashWithSalt` bgpPeerState
      `Prelude.hashWithSalt` bgpStatus
      `Prelude.hashWithSalt` customerAddress

instance Prelude.NFData BGPPeer where
  rnf BGPPeer' {..} =
    Prelude.rnf addressFamily `Prelude.seq`
      Prelude.rnf amazonAddress `Prelude.seq`
        Prelude.rnf asn `Prelude.seq`
          Prelude.rnf authKey `Prelude.seq`
            Prelude.rnf awsDeviceV2 `Prelude.seq`
              Prelude.rnf awsLogicalDeviceId `Prelude.seq`
                Prelude.rnf bgpPeerId `Prelude.seq`
                  Prelude.rnf bgpPeerState `Prelude.seq`
                    Prelude.rnf bgpStatus `Prelude.seq`
                      Prelude.rnf customerAddress
