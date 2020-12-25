{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkCardInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkCardInfo
  ( NetworkCardInfo (..),

    -- * Smart constructor
    mkNetworkCardInfo,

    -- * Lenses
    nciMaximumNetworkInterfaces,
    nciNetworkCardIndex,
    nciNetworkPerformance,
  )
where

import qualified Network.AWS.EC2.Types.NetworkPerformance as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the network card support of the instance type.
--
-- /See:/ 'mkNetworkCardInfo' smart constructor.
data NetworkCardInfo = NetworkCardInfo'
  { -- | The maximum number of network interfaces for the network card.
    maximumNetworkInterfaces :: Core.Maybe Core.Int,
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | The network performance of the network card.
    networkPerformance :: Core.Maybe Types.NetworkPerformance
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkCardInfo' value with any optional fields omitted.
mkNetworkCardInfo ::
  NetworkCardInfo
mkNetworkCardInfo =
  NetworkCardInfo'
    { maximumNetworkInterfaces = Core.Nothing,
      networkCardIndex = Core.Nothing,
      networkPerformance = Core.Nothing
    }

-- | The maximum number of network interfaces for the network card.
--
-- /Note:/ Consider using 'maximumNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciMaximumNetworkInterfaces :: Lens.Lens' NetworkCardInfo (Core.Maybe Core.Int)
nciMaximumNetworkInterfaces = Lens.field @"maximumNetworkInterfaces"
{-# DEPRECATED nciMaximumNetworkInterfaces "Use generic-lens or generic-optics with 'maximumNetworkInterfaces' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciNetworkCardIndex :: Lens.Lens' NetworkCardInfo (Core.Maybe Core.Int)
nciNetworkCardIndex = Lens.field @"networkCardIndex"
{-# DEPRECATED nciNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The network performance of the network card.
--
-- /Note:/ Consider using 'networkPerformance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nciNetworkPerformance :: Lens.Lens' NetworkCardInfo (Core.Maybe Types.NetworkPerformance)
nciNetworkPerformance = Lens.field @"networkPerformance"
{-# DEPRECATED nciNetworkPerformance "Use generic-lens or generic-optics with 'networkPerformance' instead." #-}

instance Core.FromXML NetworkCardInfo where
  parseXML x =
    NetworkCardInfo'
      Core.<$> (x Core..@? "maximumNetworkInterfaces")
      Core.<*> (x Core..@? "networkCardIndex")
      Core.<*> (x Core..@? "networkPerformance")
