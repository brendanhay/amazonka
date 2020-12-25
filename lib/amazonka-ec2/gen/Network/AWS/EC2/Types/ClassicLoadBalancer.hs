{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Classic Load Balancer.
--
-- /See:/ 'mkClassicLoadBalancer' smart constructor.
newtype ClassicLoadBalancer = ClassicLoadBalancer'
  { -- | The name of the load balancer.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ClassicLoadBalancer' value with any optional fields omitted.
mkClassicLoadBalancer ::
  ClassicLoadBalancer
mkClassicLoadBalancer = ClassicLoadBalancer' {name = Core.Nothing}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbName :: Lens.Lens' ClassicLoadBalancer (Core.Maybe Types.String)
clbName = Lens.field @"name"
{-# DEPRECATED clbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML ClassicLoadBalancer where
  parseXML x = ClassicLoadBalancer' Core.<$> (x Core..@? "name")
