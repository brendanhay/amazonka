{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfacePermissionState
  ( NetworkInterfacePermissionState (..)
  -- * Smart constructor
  , mkNetworkInterfacePermissionState
  -- * Lenses
  , nipsState
  , nipsStatusMessage
  ) where

import qualified Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of a network interface permission.
--
-- /See:/ 'mkNetworkInterfacePermissionState' smart constructor.
data NetworkInterfacePermissionState = NetworkInterfacePermissionState'
  { state :: Core.Maybe Types.NetworkInterfacePermissionStateCode
    -- ^ The state of the permission.
  , statusMessage :: Core.Maybe Core.Text
    -- ^ A status message, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfacePermissionState' value with any optional fields omitted.
mkNetworkInterfacePermissionState
    :: NetworkInterfacePermissionState
mkNetworkInterfacePermissionState
  = NetworkInterfacePermissionState'{state = Core.Nothing,
                                     statusMessage = Core.Nothing}

-- | The state of the permission.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipsState :: Lens.Lens' NetworkInterfacePermissionState (Core.Maybe Types.NetworkInterfacePermissionStateCode)
nipsState = Lens.field @"state"
{-# INLINEABLE nipsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A status message, if applicable.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipsStatusMessage :: Lens.Lens' NetworkInterfacePermissionState (Core.Maybe Core.Text)
nipsStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE nipsStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

instance Core.FromXML NetworkInterfacePermissionState where
        parseXML x
          = NetworkInterfacePermissionState' Core.<$>
              (x Core..@? "state") Core.<*> x Core..@? "statusMessage"
