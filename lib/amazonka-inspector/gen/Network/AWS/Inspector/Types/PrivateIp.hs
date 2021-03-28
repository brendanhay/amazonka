{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PrivateIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.PrivateIp
  ( PrivateIp (..)
  -- * Smart constructor
  , mkPrivateIp
  -- * Lenses
  , piPrivateDnsName
  , piPrivateIpAddress
  ) where

import qualified Network.AWS.Inspector.Types.PrivateDnsName as Types
import qualified Network.AWS.Inspector.Types.PrivateIpAddress as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a private IP address associated with a network interface. This data type is used as a response element in the 'DescribeFindings' action.
--
-- /See:/ 'mkPrivateIp' smart constructor.
data PrivateIp = PrivateIp'
  { privateDnsName :: Core.Maybe Types.PrivateDnsName
    -- ^ The DNS name of the private IP address.
  , privateIpAddress :: Core.Maybe Types.PrivateIpAddress
    -- ^ The full IP address of the network inteface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PrivateIp' value with any optional fields omitted.
mkPrivateIp
    :: PrivateIp
mkPrivateIp
  = PrivateIp'{privateDnsName = Core.Nothing,
               privateIpAddress = Core.Nothing}

-- | The DNS name of the private IP address.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPrivateDnsName :: Lens.Lens' PrivateIp (Core.Maybe Types.PrivateDnsName)
piPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE piPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The full IP address of the network inteface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPrivateIpAddress :: Lens.Lens' PrivateIp (Core.Maybe Types.PrivateIpAddress)
piPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE piPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.FromJSON PrivateIp where
        parseJSON
          = Core.withObject "PrivateIp" Core.$
              \ x ->
                PrivateIp' Core.<$>
                  (x Core..:? "privateDnsName") Core.<*>
                    x Core..:? "privateIpAddress"
