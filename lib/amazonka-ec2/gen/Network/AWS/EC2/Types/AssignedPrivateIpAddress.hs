{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssignedPrivateIpAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AssignedPrivateIpAddress
  ( AssignedPrivateIpAddress (..)
  -- * Smart constructor
  , mkAssignedPrivateIpAddress
  -- * Lenses
  , apiaPrivateIpAddress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the private IP addresses assigned to a network interface.
--
-- /See:/ 'mkAssignedPrivateIpAddress' smart constructor.
newtype AssignedPrivateIpAddress = AssignedPrivateIpAddress'
  { privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IP address assigned to the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssignedPrivateIpAddress' value with any optional fields omitted.
mkAssignedPrivateIpAddress
    :: AssignedPrivateIpAddress
mkAssignedPrivateIpAddress
  = AssignedPrivateIpAddress'{privateIpAddress = Core.Nothing}

-- | The private IP address assigned to the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apiaPrivateIpAddress :: Lens.Lens' AssignedPrivateIpAddress (Core.Maybe Core.Text)
apiaPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE apiaPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.FromXML AssignedPrivateIpAddress where
        parseXML x
          = AssignedPrivateIpAddress' Core.<$>
              (x Core..@? "privateIpAddress")
