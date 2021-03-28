{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
  ( PrivateIpAddressDetails (..)
  -- * Smart constructor
  , mkPrivateIpAddressDetails
  -- * Lenses
  , piadPrivateDnsName
  , piadPrivateIpAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains other private IP address information of the EC2 instance.
--
-- /See:/ 'mkPrivateIpAddressDetails' smart constructor.
data PrivateIpAddressDetails = PrivateIpAddressDetails'
  { privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name of the EC2 instance.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IP address of the EC2 instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrivateIpAddressDetails' value with any optional fields omitted.
mkPrivateIpAddressDetails
    :: PrivateIpAddressDetails
mkPrivateIpAddressDetails
  = PrivateIpAddressDetails'{privateDnsName = Core.Nothing,
                             privateIpAddress = Core.Nothing}

-- | The private DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piadPrivateDnsName :: Lens.Lens' PrivateIpAddressDetails (Core.Maybe Core.Text)
piadPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE piadPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The private IP address of the EC2 instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piadPrivateIpAddress :: Lens.Lens' PrivateIpAddressDetails (Core.Maybe Core.Text)
piadPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE piadPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.FromJSON PrivateIpAddressDetails where
        parseJSON
          = Core.withObject "PrivateIpAddressDetails" Core.$
              \ x ->
                PrivateIpAddressDetails' Core.<$>
                  (x Core..:? "privateDnsName") Core.<*>
                    x Core..:? "privateIpAddress"
