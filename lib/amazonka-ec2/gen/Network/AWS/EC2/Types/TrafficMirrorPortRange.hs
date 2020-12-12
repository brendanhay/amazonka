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
import qualified Network.AWS.Prelude as Lude

-- | Describes the Traffic Mirror port range.
--
-- /See:/ 'mkTrafficMirrorPortRange' smart constructor.
data TrafficMirrorPortRange = TrafficMirrorPortRange'
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

-- | Creates a value of 'TrafficMirrorPortRange' with the minimum fields required to make a request.
--
-- * 'fromPort' - The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
-- * 'toPort' - The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
mkTrafficMirrorPortRange ::
  TrafficMirrorPortRange
mkTrafficMirrorPortRange =
  TrafficMirrorPortRange'
    { fromPort = Lude.Nothing,
      toPort = Lude.Nothing
    }

-- | The start of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprFromPort :: Lens.Lens' TrafficMirrorPortRange (Lude.Maybe Lude.Int)
tmprFromPort = Lens.lens (fromPort :: TrafficMirrorPortRange -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: TrafficMirrorPortRange)
{-# DEPRECATED tmprFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The end of the Traffic Mirror port range. This applies to the TCP and UDP protocols.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmprToPort :: Lens.Lens' TrafficMirrorPortRange (Lude.Maybe Lude.Int)
tmprToPort = Lens.lens (toPort :: TrafficMirrorPortRange -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: TrafficMirrorPortRange)
{-# DEPRECATED tmprToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

instance Lude.FromXML TrafficMirrorPortRange where
  parseXML x =
    TrafficMirrorPortRange'
      Lude.<$> (x Lude..@? "fromPort") Lude.<*> (x Lude..@? "toPort")
