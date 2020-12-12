{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
  ( LoadBalancerDescription (..),

    -- * Smart constructor
    mkLoadBalancerDescription,

    -- * Lenses
    lbdLoadBalancerName,
    lbdDomain,
    lbdListeners,
  )
where

import Network.AWS.ElasticBeanstalk.Types.Listener
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'mkLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { loadBalancerName ::
      Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    listeners :: Lude.Maybe [Listener]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- * 'domain' - The domain name of the LoadBalancer.
-- * 'listeners' - A list of Listeners used by the LoadBalancer.
-- * 'loadBalancerName' - The name of the LoadBalancer.
mkLoadBalancerDescription ::
  LoadBalancerDescription
mkLoadBalancerDescription =
  LoadBalancerDescription'
    { loadBalancerName = Lude.Nothing,
      domain = Lude.Nothing,
      listeners = Lude.Nothing
    }

-- | The name of the LoadBalancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdLoadBalancerName :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The domain name of the LoadBalancer.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdDomain :: Lens.Lens' LoadBalancerDescription (Lude.Maybe Lude.Text)
lbdDomain = Lens.lens (domain :: LoadBalancerDescription -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | A list of Listeners used by the LoadBalancer.
--
-- /Note:/ Consider using 'listeners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbdListeners :: Lens.Lens' LoadBalancerDescription (Lude.Maybe [Listener])
lbdListeners = Lens.lens (listeners :: LoadBalancerDescription -> Lude.Maybe [Listener]) (\s a -> s {listeners = a} :: LoadBalancerDescription)
{-# DEPRECATED lbdListeners "Use generic-lens or generic-optics with 'listeners' instead." #-}

instance Lude.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Lude.<$> (x Lude..@? "LoadBalancerName")
      Lude.<*> (x Lude..@? "Domain")
      Lude.<*> ( x Lude..@? "Listeners" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
