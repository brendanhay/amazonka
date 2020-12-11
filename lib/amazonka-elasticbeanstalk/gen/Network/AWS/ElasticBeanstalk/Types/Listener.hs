-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Listener
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Listener
  ( Listener (..),

    -- * Smart constructor
    mkListener,

    -- * Lenses
    lProtocol,
    lPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of a Listener for the LoadBalancer.
--
-- /See:/ 'mkListener' smart constructor.
data Listener = Listener'
  { protocol :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- * 'port' - The port that is used by the Listener.
-- * 'protocol' - The protocol that is used by the Listener.
mkListener ::
  Listener
mkListener =
  Listener' {protocol = Lude.Nothing, port = Lude.Nothing}

-- | The protocol that is used by the Listener.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lProtocol :: Lens.Lens' Listener (Lude.Maybe Lude.Text)
lProtocol = Lens.lens (protocol :: Listener -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: Listener)
{-# DEPRECATED lProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The port that is used by the Listener.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPort :: Lens.Lens' Listener (Lude.Maybe Lude.Int)
lPort = Lens.lens (port :: Listener -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Listener)
{-# DEPRECATED lPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML Listener where
  parseXML x =
    Listener'
      Lude.<$> (x Lude..@? "Protocol") Lude.<*> (x Lude..@? "Port")
