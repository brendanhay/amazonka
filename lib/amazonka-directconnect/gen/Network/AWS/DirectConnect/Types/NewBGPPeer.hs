{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewBGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.NewBGPPeer
  ( NewBGPPeer (..)
  -- * Smart constructor
  , mkNewBGPPeer
  -- * Lenses
  , nbgppAddressFamily
  , nbgppAmazonAddress
  , nbgppAsn
  , nbgppAuthKey
  , nbgppCustomerAddress
  ) where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a new BGP peer.
--
-- /See:/ 'mkNewBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { addressFamily :: Core.Maybe Types.AddressFamily
    -- ^ The address family for the BGP peer.
  , amazonAddress :: Core.Maybe Types.AmazonAddress
    -- ^ The IP address assigned to the Amazon interface.
  , asn :: Core.Maybe Core.Int
    -- ^ The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
  , authKey :: Core.Maybe Types.BGPAuthKey
    -- ^ The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
  , customerAddress :: Core.Maybe Types.CustomerAddress
    -- ^ The IP address assigned to the customer interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewBGPPeer' value with any optional fields omitted.
mkNewBGPPeer
    :: NewBGPPeer
mkNewBGPPeer
  = NewBGPPeer'{addressFamily = Core.Nothing,
                amazonAddress = Core.Nothing, asn = Core.Nothing,
                authKey = Core.Nothing, customerAddress = Core.Nothing}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbgppAddressFamily :: Lens.Lens' NewBGPPeer (Core.Maybe Types.AddressFamily)
nbgppAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE nbgppAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbgppAmazonAddress :: Lens.Lens' NewBGPPeer (Core.Maybe Types.AmazonAddress)
nbgppAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE nbgppAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbgppAsn :: Lens.Lens' NewBGPPeer (Core.Maybe Core.Int)
nbgppAsn = Lens.field @"asn"
{-# INLINEABLE nbgppAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbgppAuthKey :: Lens.Lens' NewBGPPeer (Core.Maybe Types.BGPAuthKey)
nbgppAuthKey = Lens.field @"authKey"
{-# INLINEABLE nbgppAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbgppCustomerAddress :: Lens.Lens' NewBGPPeer (Core.Maybe Types.CustomerAddress)
nbgppCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE nbgppCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

instance Core.FromJSON NewBGPPeer where
        toJSON NewBGPPeer{..}
          = Core.object
              (Core.catMaybes
                 [("addressFamily" Core..=) Core.<$> addressFamily,
                  ("amazonAddress" Core..=) Core.<$> amazonAddress,
                  ("asn" Core..=) Core.<$> asn, ("authKey" Core..=) Core.<$> authKey,
                  ("customerAddress" Core..=) Core.<$> customerAddress])
