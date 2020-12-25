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

import qualified Network.AWS.EC2.Types.ClassicLoadBalancer as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot Fleet registers the running Spot Instances with these Classic Load Balancers.
--
-- /See:/ 'mkClassicLoadBalancersConfig' smart constructor.
newtype ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { -- | One or more Classic Load Balancers.
    classicLoadBalancers :: Core.Maybe (Core.NonEmpty Types.ClassicLoadBalancer)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ClassicLoadBalancersConfig' value with any optional fields omitted.
mkClassicLoadBalancersConfig ::
  ClassicLoadBalancersConfig
mkClassicLoadBalancersConfig =
  ClassicLoadBalancersConfig' {classicLoadBalancers = Core.Nothing}

-- | One or more Classic Load Balancers.
--
-- /Note:/ Consider using 'classicLoadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbcClassicLoadBalancers :: Lens.Lens' ClassicLoadBalancersConfig (Core.Maybe (Core.NonEmpty Types.ClassicLoadBalancer))
clbcClassicLoadBalancers = Lens.field @"classicLoadBalancers"
{-# DEPRECATED clbcClassicLoadBalancers "Use generic-lens or generic-optics with 'classicLoadBalancers' instead." #-}

instance Core.FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      Core.<$> ( x Core..@? "classicLoadBalancers"
                   Core..<@> Core.parseXMLNonEmpty "item"
               )
