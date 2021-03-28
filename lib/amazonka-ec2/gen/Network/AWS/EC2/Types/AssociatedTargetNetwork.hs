{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AssociatedTargetNetwork
  ( AssociatedTargetNetwork (..)
  -- * Smart constructor
  , mkAssociatedTargetNetwork
  -- * Lenses
  , atnNetworkId
  , atnNetworkType
  ) where

import qualified Network.AWS.EC2.Types.AssociatedNetworkType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target network that is associated with a Client VPN endpoint. A target network is a subnet in a VPC.
--
-- /See:/ 'mkAssociatedTargetNetwork' smart constructor.
data AssociatedTargetNetwork = AssociatedTargetNetwork'
  { networkId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet.
  , networkType :: Core.Maybe Types.AssociatedNetworkType
    -- ^ The target network type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatedTargetNetwork' value with any optional fields omitted.
mkAssociatedTargetNetwork
    :: AssociatedTargetNetwork
mkAssociatedTargetNetwork
  = AssociatedTargetNetwork'{networkId = Core.Nothing,
                             networkType = Core.Nothing}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atnNetworkId :: Lens.Lens' AssociatedTargetNetwork (Core.Maybe Core.Text)
atnNetworkId = Lens.field @"networkId"
{-# INLINEABLE atnNetworkId #-}
{-# DEPRECATED networkId "Use generic-lens or generic-optics with 'networkId' instead"  #-}

-- | The target network type.
--
-- /Note:/ Consider using 'networkType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atnNetworkType :: Lens.Lens' AssociatedTargetNetwork (Core.Maybe Types.AssociatedNetworkType)
atnNetworkType = Lens.field @"networkType"
{-# INLINEABLE atnNetworkType #-}
{-# DEPRECATED networkType "Use generic-lens or generic-optics with 'networkType' instead"  #-}

instance Core.FromXML AssociatedTargetNetwork where
        parseXML x
          = AssociatedTargetNetwork' Core.<$>
              (x Core..@? "networkId") Core.<*> x Core..@? "networkType"
