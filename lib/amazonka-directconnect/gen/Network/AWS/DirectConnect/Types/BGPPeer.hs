{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.BGPPeer
  ( BGPPeer (..)
  -- * Smart constructor
  , mkBGPPeer
  -- * Lenses
  , bgppAddressFamily
  , bgppAmazonAddress
  , bgppAsn
  , bgppAuthKey
  , bgppAwsDeviceV2
  , bgppBgpPeerId
  , bgppBgpPeerState
  , bgppBgpStatus
  , bgppCustomerAddress
  ) where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.AuthKey as Types
import qualified Network.AWS.DirectConnect.Types.AwsDeviceV2 as Types
import qualified Network.AWS.DirectConnect.Types.BGPPeerState as Types
import qualified Network.AWS.DirectConnect.Types.BGPStatus as Types
import qualified Network.AWS.DirectConnect.Types.BgpPeerId as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a BGP peer.
--
-- /See:/ 'mkBGPPeer' smart constructor.
data BGPPeer = BGPPeer'
  { addressFamily :: Core.Maybe Types.AddressFamily
    -- ^ The address family for the BGP peer.
  , amazonAddress :: Core.Maybe Types.AmazonAddress
    -- ^ The IP address assigned to the Amazon interface.
  , asn :: Core.Maybe Core.Int
    -- ^ The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
  , authKey :: Core.Maybe Types.AuthKey
    -- ^ The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
  , awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2
    -- ^ The Direct Connect endpoint on which the BGP peer terminates.
  , bgpPeerId :: Core.Maybe Types.BgpPeerId
    -- ^ The ID of the BGP peer.
  , bgpPeerState :: Core.Maybe Types.BGPPeerState
    -- ^ The state of the BGP peer. The following are the possible values:
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
  , bgpStatus :: Core.Maybe Types.BGPStatus
    -- ^ The status of the BGP peer. The following are the possible values:
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
  , customerAddress :: Core.Maybe Types.CustomerAddress
    -- ^ The IP address assigned to the customer interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BGPPeer' value with any optional fields omitted.
mkBGPPeer
    :: BGPPeer
mkBGPPeer
  = BGPPeer'{addressFamily = Core.Nothing,
             amazonAddress = Core.Nothing, asn = Core.Nothing,
             authKey = Core.Nothing, awsDeviceV2 = Core.Nothing,
             bgpPeerId = Core.Nothing, bgpPeerState = Core.Nothing,
             bgpStatus = Core.Nothing, customerAddress = Core.Nothing}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppAddressFamily :: Lens.Lens' BGPPeer (Core.Maybe Types.AddressFamily)
bgppAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE bgppAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppAmazonAddress :: Lens.Lens' BGPPeer (Core.Maybe Types.AmazonAddress)
bgppAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE bgppAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppAsn :: Lens.Lens' BGPPeer (Core.Maybe Core.Int)
bgppAsn = Lens.field @"asn"
{-# INLINEABLE bgppAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppAuthKey :: Lens.Lens' BGPPeer (Core.Maybe Types.AuthKey)
bgppAuthKey = Lens.field @"authKey"
{-# INLINEABLE bgppAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The Direct Connect endpoint on which the BGP peer terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppAwsDeviceV2 :: Lens.Lens' BGPPeer (Core.Maybe Types.AwsDeviceV2)
bgppAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# INLINEABLE bgppAwsDeviceV2 #-}
{-# DEPRECATED awsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead"  #-}

-- | The ID of the BGP peer.
--
-- /Note:/ Consider using 'bgpPeerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppBgpPeerId :: Lens.Lens' BGPPeer (Core.Maybe Types.BgpPeerId)
bgppBgpPeerId = Lens.field @"bgpPeerId"
{-# INLINEABLE bgppBgpPeerId #-}
{-# DEPRECATED bgpPeerId "Use generic-lens or generic-optics with 'bgpPeerId' instead"  #-}

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
bgppBgpPeerState :: Lens.Lens' BGPPeer (Core.Maybe Types.BGPPeerState)
bgppBgpPeerState = Lens.field @"bgpPeerState"
{-# INLINEABLE bgppBgpPeerState #-}
{-# DEPRECATED bgpPeerState "Use generic-lens or generic-optics with 'bgpPeerState' instead"  #-}

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
bgppBgpStatus :: Lens.Lens' BGPPeer (Core.Maybe Types.BGPStatus)
bgppBgpStatus = Lens.field @"bgpStatus"
{-# INLINEABLE bgppBgpStatus #-}
{-# DEPRECATED bgpStatus "Use generic-lens or generic-optics with 'bgpStatus' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgppCustomerAddress :: Lens.Lens' BGPPeer (Core.Maybe Types.CustomerAddress)
bgppCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE bgppCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

instance Core.FromJSON BGPPeer where
        parseJSON
          = Core.withObject "BGPPeer" Core.$
              \ x ->
                BGPPeer' Core.<$>
                  (x Core..:? "addressFamily") Core.<*> x Core..:? "amazonAddress"
                    Core.<*> x Core..:? "asn"
                    Core.<*> x Core..:? "authKey"
                    Core.<*> x Core..:? "awsDeviceV2"
                    Core.<*> x Core..:? "bgpPeerId"
                    Core.<*> x Core..:? "bgpPeerState"
                    Core.<*> x Core..:? "bgpStatus"
                    Core.<*> x Core..:? "customerAddress"
