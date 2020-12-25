{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRange
  ( TrafficMirrorPortRange (..),

    -- * Smart constructor
    mkTrafficMirrorPortRange,

    -- * Lenses
    tmprFromPort,
    tmprToPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Traffic Mirror port range.
--
-- /See:/ 'mkTrafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
  { -- | The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
    fromPort :: Core.Maybe Core.Int,
    -- | The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorPortRange' value with any optional fields omitted.
mkTrafficMirrorPortRange ::
  TrafficMirrorPortRange
mkTrafficMirrorPortRange =
  TrafficMirrorPortRange'
    { fromPort = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprFromPort :: Lens.Lens' TrafficMirrorPortRange (Core.Maybe Core.Int)
tmprFromPort = Lens.field @"fromPort"
{-# DEPRECATED tmprFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprToPort :: Lens.Lens' TrafficMirrorPortRange (Core.Maybe Core.Int)
tmprToPort = Lens.field @"toPort"
{-# DEPRECATED tmprToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Core.FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      Core.<$> (x Core..@? "fromPort") Core.<*> (x Core..@? "toPort")
