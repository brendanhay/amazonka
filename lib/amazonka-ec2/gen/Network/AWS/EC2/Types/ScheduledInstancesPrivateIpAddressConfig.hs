{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
  ( ScheduledInstancesPrivateIpAddressConfig (..)
  -- * Smart constructor
  , mkScheduledInstancesPrivateIpAddressConfig
  -- * Lenses
  , sipiacPrimary
  , sipiacPrivateIpAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a private IPv4 address for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesPrivateIpAddressConfig' smart constructor.
data ScheduledInstancesPrivateIpAddressConfig = ScheduledInstancesPrivateIpAddressConfig'
  { primary :: Core.Maybe Core.Bool
    -- ^ Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The IPv4 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesPrivateIpAddressConfig' value with any optional fields omitted.
mkScheduledInstancesPrivateIpAddressConfig
    :: ScheduledInstancesPrivateIpAddressConfig
mkScheduledInstancesPrivateIpAddressConfig
  = ScheduledInstancesPrivateIpAddressConfig'{primary = Core.Nothing,
                                              privateIpAddress = Core.Nothing}

-- | Indicates whether this is a primary IPv4 address. Otherwise, this is a secondary IPv4 address.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipiacPrimary :: Lens.Lens' ScheduledInstancesPrivateIpAddressConfig (Core.Maybe Core.Bool)
sipiacPrimary = Lens.field @"primary"
{-# INLINEABLE sipiacPrimary #-}
{-# DEPRECATED primary "Use generic-lens or generic-optics with 'primary' instead"  #-}

-- | The IPv4 address.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipiacPrivateIpAddress :: Lens.Lens' ScheduledInstancesPrivateIpAddressConfig (Core.Maybe Core.Text)
sipiacPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE sipiacPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.ToQuery ScheduledInstancesPrivateIpAddressConfig
         where
        toQuery ScheduledInstancesPrivateIpAddressConfig{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Primary") primary
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress
