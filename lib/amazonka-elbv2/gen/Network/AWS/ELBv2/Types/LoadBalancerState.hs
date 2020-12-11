-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancerState
  ( LoadBalancerState (..),

    -- * Smart constructor
    mkLoadBalancerState,

    -- * Lenses
    lbsReason,
    lbsCode,
  )
where

import Network.AWS.ELBv2.Types.LoadBalancerStateEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the state of the load balancer.
--
-- /See:/ 'mkLoadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { reason ::
      Lude.Maybe Lude.Text,
    code :: Lude.Maybe LoadBalancerStateEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- * 'code' - The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
-- * 'reason' - A description of the state.
mkLoadBalancerState ::
  LoadBalancerState
mkLoadBalancerState =
  LoadBalancerState' {reason = Lude.Nothing, code = Lude.Nothing}

-- | A description of the state.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsReason :: Lens.Lens' LoadBalancerState (Lude.Maybe Lude.Text)
lbsReason = Lens.lens (reason :: LoadBalancerState -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: LoadBalancerState)
{-# DEPRECATED lbsReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbsCode :: Lens.Lens' LoadBalancerState (Lude.Maybe LoadBalancerStateEnum)
lbsCode = Lens.lens (code :: LoadBalancerState -> Lude.Maybe LoadBalancerStateEnum) (\s a -> s {code = a} :: LoadBalancerState)
{-# DEPRECATED lbsCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromXML LoadBalancerState where
  parseXML x =
    LoadBalancerState'
      Lude.<$> (x Lude..@? "Reason") Lude.<*> (x Lude..@? "Code")
