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
    trListenerARNs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a listener. The listener contains the path used to route traffic that is received from the load balancer to a target group.
--
-- /See:/ 'mkTrafficRoute' smart constructor.
newtype TrafficRoute = TrafficRoute'
  { listenerARNs ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficRoute' with the minimum fields required to make a request.
--
-- * 'listenerARNs' - The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
mkTrafficRoute ::
  TrafficRoute
mkTrafficRoute = TrafficRoute' {listenerARNs = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of one listener. The listener identifies the route between a target group and a load balancer. This is an array of strings with a maximum size of one.
--
-- /Note:/ Consider using 'listenerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trListenerARNs :: Lens.Lens' TrafficRoute (Lude.Maybe [Lude.Text])
trListenerARNs = Lens.lens (listenerARNs :: TrafficRoute -> Lude.Maybe [Lude.Text]) (\s a -> s {listenerARNs = a} :: TrafficRoute)
{-# DEPRECATED trListenerARNs "Use generic-lens or generic-optics with 'listenerARNs' instead." #-}

instance Lude.FromJSON TrafficRoute where
  parseJSON =
    Lude.withObject
      "TrafficRoute"
      ( \x ->
          TrafficRoute'
            Lude.<$> (x Lude..:? "listenerArns" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TrafficRoute where
  toJSON TrafficRoute' {..} =
    Lude.object
      (Lude.catMaybes [("listenerArns" Lude..=) Lude.<$> listenerARNs])
