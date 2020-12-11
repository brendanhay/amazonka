-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancer
  ( ClassicLoadBalancer (..),

    -- * Smart constructor
    mkClassicLoadBalancer,

    -- * Lenses
    clbName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Classic Load Balancer.
--
-- /See:/ 'mkClassicLoadBalancer' smart constructor.
newtype ClassicLoadBalancer = ClassicLoadBalancer'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassicLoadBalancer' with the minimum fields required to make a request.
--
-- * 'name' - The name of the load balancer.
mkClassicLoadBalancer ::
  ClassicLoadBalancer
mkClassicLoadBalancer = ClassicLoadBalancer' {name = Lude.Nothing}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbName :: Lens.Lens' ClassicLoadBalancer (Lude.Maybe Lude.Text)
clbName = Lens.lens (name :: ClassicLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ClassicLoadBalancer)
{-# DEPRECATED clbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML ClassicLoadBalancer where
  parseXML x = ClassicLoadBalancer' Lude.<$> (x Lude..@? "name")

instance Lude.ToQuery ClassicLoadBalancer where
  toQuery ClassicLoadBalancer' {..} =
    Lude.mconcat ["Name" Lude.=: name]
