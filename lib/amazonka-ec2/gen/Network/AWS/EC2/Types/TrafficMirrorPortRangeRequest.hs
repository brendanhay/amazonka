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
import qualified Network.AWS.Prelude as Lude

-- | Information about the Traffic Mirror filter rule port range.
--
-- /See:/ 'mkTrafficMirrorPortRangeRequest' smart constructor.
data TrafficMirrorPortRangeRequest = TrafficMirrorPortRangeRequest'
  { fromPort ::
      Lude.Maybe Lude.Int,
    toPort :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficMirrorPortRangeRequest' with the minimum fields required to make a request.
--
-- * 'fromPort' - The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
-- * 'toPort' - The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
mkTrafficMirrorPortRangeRequest ::
  TrafficMirrorPortRangeRequest
mkTrafficMirrorPortRangeRequest =
  TrafficMirrorPortRangeRequest'
    { fromPort = Lude.Nothing,
      toPort = Lude.Nothing
    }

-- | The first port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprrFromPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Lude.Maybe Lude.Int)
tmprrFromPort = Lens.lens (fromPort :: TrafficMirrorPortRangeRequest -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: TrafficMirrorPortRangeRequest)
{-# DEPRECATED tmprrFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The last port in the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprrToPort :: Lens.Lens' TrafficMirrorPortRangeRequest (Lude.Maybe Lude.Int)
tmprrToPort = Lens.lens (toPort :: TrafficMirrorPortRangeRequest -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: TrafficMirrorPortRangeRequest)
{-# DEPRECATED tmprrToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Lude.ToQuery TrafficMirrorPortRangeRequest where
  toQuery TrafficMirrorPortRangeRequest' {..} =
    Lude.mconcat
      ["FromPort" Lude.=: fromPort, "ToPort" Lude.=: toPort]
