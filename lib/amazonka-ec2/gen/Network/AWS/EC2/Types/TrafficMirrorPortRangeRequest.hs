{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorPortRangeRequest
  ( TrafficMirrorPortRangeRequest (..),

    -- * Smart constructor
    mkTrafficMirrorPortRangeRequest,

    -- * Lenses
    tmprrFromPort,
    tmprrToPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Traffic Mirror filter rule port range.
--
-- /See:/ 'mkTrafficMirrorPortRangeRequest' smart constructor.
data TrafficMirrorPortRangeRequest = TrafficMirrorPortRangeRequest'
  { -- | The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
    fromPort :: Core.Maybe Core.Int,
    -- | The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
    toPort :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorPortRangeRequest' value with any optional fields omitted.
mkTrafficMirrorPortRangeRequest ::
  TrafficMirrorPortRangeRequest
mkTrafficMirrorPortRangeRequest =
  TrafficMirrorPortRangeRequest'
    { fromPort = Core.Nothing,
      toPort = Core.Nothing
    }

-- | The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprrFromPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Core.Maybe Core.Int)
tmprrFromPort = Lens.field @"fromPort"
{-# DEPRECATED tmprrFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprrToPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Core.Maybe Core.Int)
tmprrToPort = Lens.field @"toPort"
{-# DEPRECATED tmprrToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}
