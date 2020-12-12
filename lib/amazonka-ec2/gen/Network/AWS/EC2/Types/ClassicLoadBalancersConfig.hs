{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancersConfig
  ( ClassicLoadBalancersConfig (..),

    -- * Smart constructor
    mkClassicLoadBalancersConfig,

    -- * Lenses
    clbcClassicLoadBalancers,
  )
where

import Network.AWS.EC2.Types.ClassicLoadBalancer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these Classic Load Balancers.
--
-- /See:/ 'mkClassicLoadBalancersConfig' smart constructor.
newtype ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { classicLoadBalancers ::
      Lude.Maybe
        ( Lude.NonEmpty
            ClassicLoadBalancer
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassicLoadBalancersConfig' with the minimum fields required to make a request.
--
-- * 'classicLoadBalancers' - One or more Classic Load Balancers.
mkClassicLoadBalancersConfig ::
  ClassicLoadBalancersConfig
mkClassicLoadBalancersConfig =
  ClassicLoadBalancersConfig' {classicLoadBalancers = Lude.Nothing}

-- | One or more Classic Load Balancers.
--
-- /Note:/ Consider using 'classicLoadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcClassicLoadBalancers :: Lens.Lens' ClassicLoadBalancersConfig (Lude.Maybe (Lude.NonEmpty ClassicLoadBalancer))
clbcClassicLoadBalancers = Lens.lens (classicLoadBalancers :: ClassicLoadBalancersConfig -> Lude.Maybe (Lude.NonEmpty ClassicLoadBalancer)) (\s a -> s {classicLoadBalancers = a} :: ClassicLoadBalancersConfig)
{-# DEPRECATED clbcClassicLoadBalancers "Use generic-lens or generic-optics with 'classicLoadBalancers' instead." #-}

instance Lude.FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      Lude.<$> ( x Lude..@? "classicLoadBalancers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "item")
               )

instance Lude.ToQuery ClassicLoadBalancersConfig where
  toQuery ClassicLoadBalancersConfig' {..} =
    Lude.mconcat
      [ Lude.toQuery
          ( Lude.toQueryList "ClassicLoadBalancers"
              Lude.<$> classicLoadBalancers
          )
      ]
