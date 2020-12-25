{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoute
  ( TrafficRoute (..),

    -- * Smart constructor
    mkTrafficRoute,

    -- * Lenses
    trListenerArns,
  )
where

import qualified Network.AWS.CodeDeploy.Types.ListenerArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a listener. The listener contains the path used to route traffic that is received from the load balancer to a target group.
--
-- /See:/ 'mkTrafficRoute' smart constructor.
newtype TrafficRoute = TrafficRoute'
  { -- | The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
    listenerArns :: Core.Maybe [Types.ListenerArn]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficRoute' value with any optional fields omitted.
mkTrafficRoute ::
  TrafficRoute
mkTrafficRoute = TrafficRoute' {listenerArns = Core.Nothing}

-- | The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
--
-- /Note:/ Consider using 'listenerArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trListenerArns :: Lens.Lens' TrafficRoute (Core.Maybe [Types.ListenerArn])
trListenerArns = Lens.field @"listenerArns"
{-# DEPRECATED trListenerArns "Use generic-lens or generic-optics with 'listenerArns' instead." #-}

instance Core.FromJSON TrafficRoute where
  toJSON TrafficRoute {..} =
    Core.object
      (Core.catMaybes [("listenerArns" Core..=) Core.<$> listenerArns])

instance Core.FromJSON TrafficRoute where
  parseJSON =
    Core.withObject "TrafficRoute" Core.$
      \x -> TrafficRoute' Core.<$> (x Core..:? "listenerArns")
