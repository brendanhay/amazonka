-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPPeer
  ( BGPPeer (..),

    -- * Smart constructor
    mkBGPPeer,

    -- * Lenses
    bpCustomerAddress,
    bpAmazonAddress,
    bpAddressFamily,
    bpBgpStatus,
    bpAsn,
    bpAuthKey,
    bpBgpPeerId,
    bpBgpPeerState,
    bpAwsDeviceV2,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.BGPPeerState
import Network.AWS.DirectConnect.Types.BGPStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a BGP peer.
--
-- /See:/ 'mkBGPPeer' smart constructor.
data BGPPeer = BGPPeer'
  { customerAddress :: Lude.Maybe Lude.Text,
    amazonAddress :: Lude.Maybe Lude.Text,
    addressFamily :: Lude.Maybe AddressFamily,
    bgpStatus :: Lude.Maybe BGPStatus,
    asn :: Lude.Maybe Lude.Int,
    authKey :: Lude.Maybe Lude.Text,
    bgpPeerId :: Lude.Maybe Lude.Text,
    bgpPeerState :: Lude.Maybe BGPPeerState,
    awsDeviceV2 :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BGPPeer' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'awsDeviceV2' - The Direct Connect endpoint on which the BGP peer terminates.
-- * 'bgpPeerId' - The ID of the BGP peer.
-- * 'bgpPeerState' - The state of the BGP peer. The following are the possible values:
--
--
--     * @verifying@ : The BGP peering addresses or ASN require validation before the BGP peer can be created. This state applies only to public virtual interfaces.
--
--
--     * @pending@ : The BGP peer is created, and remains in this state until it is ready to be established.
--
--
--     * @available@ : The BGP peer is ready to be established.
--
--
--     * @deleting@ : The BGP peer is being deleted.
--
--
--     * @deleted@ : The BGP peer is deleted and cannot be established.
--
--
-- * 'bgpStatus' - The status of the BGP peer. The following are the possible values:
--
--
--     * @up@ : The BGP peer is established. This state does not indicate the state of the routing function. Ensure that you are receiving routes over the BGP session.
--
--
--     * @down@ : The BGP peer is down.
--
--
--     * @unknown@ : The BGP peer status is not available.
--
--
-- * 'customerAddress' - The IP address assigned to the customer interface.
mkBGPPeer ::
  BGPPeer
mkBGPPeer =
  BGPPeer'
    { customerAddress = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      bgpStatus = Lude.Nothing,
      asn = Lude.Nothing,
      authKey = Lude.Nothing,
      bgpPeerId = Lude.Nothing,
      bgpPeerState = Lude.Nothing,
      awsDeviceV2 = Lude.Nothing
    }

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpCustomerAddress :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Text)
bpCustomerAddress = Lens.lens (customerAddress :: BGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: BGPPeer)
{-# DEPRECATED bpCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAmazonAddress :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Text)
bpAmazonAddress = Lens.lens (amazonAddress :: BGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: BGPPeer)
{-# DEPRECATED bpAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAddressFamily :: Lens.Lens' BGPPeer (Lude.Maybe AddressFamily)
bpAddressFamily = Lens.lens (addressFamily :: BGPPeer -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: BGPPeer)
{-# DEPRECATED bpAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The status of the BGP peer. The following are the possible values:
--
--
--     * @up@ : The BGP peer is established. This state does not indicate the state of the routing function. Ensure that you are receiving routes over the BGP session.
--
--
--     * @down@ : The BGP peer is down.
--
--
--     * @unknown@ : The BGP peer status is not available.
--
--
--
-- /Note:/ Consider using 'bgpStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBgpStatus :: Lens.Lens' BGPPeer (Lude.Maybe BGPStatus)
bpBgpStatus = Lens.lens (bgpStatus :: BGPPeer -> Lude.Maybe BGPStatus) (\s a -> s {bgpStatus = a} :: BGPPeer)
{-# DEPRECATED bpBgpStatus "Use generic-lens or generic-optics with 'bgpStatus' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAsn :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Int)
bpAsn = Lens.lens (asn :: BGPPeer -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: BGPPeer)
{-# DEPRECATED bpAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAuthKey :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Text)
bpAuthKey = Lens.lens (authKey :: BGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: BGPPeer)
{-# DEPRECATED bpAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The ID of the BGP peer.
--
-- /Note:/ Consider using 'bgpPeerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBgpPeerId :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Text)
bpBgpPeerId = Lens.lens (bgpPeerId :: BGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {bgpPeerId = a} :: BGPPeer)
{-# DEPRECATED bpBgpPeerId "Use generic-lens or generic-optics with 'bgpPeerId' instead." #-}

-- | The state of the BGP peer. The following are the possible values:
--
--
--     * @verifying@ : The BGP peering addresses or ASN require validation before the BGP peer can be created. This state applies only to public virtual interfaces.
--
--
--     * @pending@ : The BGP peer is created, and remains in this state until it is ready to be established.
--
--
--     * @available@ : The BGP peer is ready to be established.
--
--
--     * @deleting@ : The BGP peer is being deleted.
--
--
--     * @deleted@ : The BGP peer is deleted and cannot be established.
--
--
--
-- /Note:/ Consider using 'bgpPeerState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBgpPeerState :: Lens.Lens' BGPPeer (Lude.Maybe BGPPeerState)
bpBgpPeerState = Lens.lens (bgpPeerState :: BGPPeer -> Lude.Maybe BGPPeerState) (\s a -> s {bgpPeerState = a} :: BGPPeer)
{-# DEPRECATED bpBgpPeerState "Use generic-lens or generic-optics with 'bgpPeerState' instead." #-}

-- | The Direct Connect endpoint on which the BGP peer terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAwsDeviceV2 :: Lens.Lens' BGPPeer (Lude.Maybe Lude.Text)
bpAwsDeviceV2 = Lens.lens (awsDeviceV2 :: BGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {awsDeviceV2 = a} :: BGPPeer)
{-# DEPRECATED bpAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

instance Lude.FromJSON BGPPeer where
  parseJSON =
    Lude.withObject
      "BGPPeer"
      ( \x ->
          BGPPeer'
            Lude.<$> (x Lude..:? "customerAddress")
            Lude.<*> (x Lude..:? "amazonAddress")
            Lude.<*> (x Lude..:? "addressFamily")
            Lude.<*> (x Lude..:? "bgpStatus")
            Lude.<*> (x Lude..:? "asn")
            Lude.<*> (x Lude..:? "authKey")
            Lude.<*> (x Lude..:? "bgpPeerId")
            Lude.<*> (x Lude..:? "bgpPeerState")
            Lude.<*> (x Lude..:? "awsDeviceV2")
      )
