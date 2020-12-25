{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Denied
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Denied
  ( Denied (..),

    -- * Smart constructor
    mkDenied,

    -- * Lenses
    dExplicitDeny,
    dImplicitDeny,
  )
where

import qualified Network.AWS.IoT.Types.ExplicitDeny as Types
import qualified Network.AWS.IoT.Types.ImplicitDeny as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information that denied the authorization.
--
-- /See:/ 'mkDenied' smart constructor.
data Denied = Denied'
  { -- | Information that explicitly denies the authorization.
    explicitDeny :: Core.Maybe Types.ExplicitDeny,
    -- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
    implicitDeny :: Core.Maybe Types.ImplicitDeny
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Denied' value with any optional fields omitted.
mkDenied ::
  Denied
mkDenied =
  Denied' {explicitDeny = Core.Nothing, implicitDeny = Core.Nothing}

-- | Information that explicitly denies the authorization.
--
-- /Note:/ Consider using 'explicitDeny' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExplicitDeny :: Lens.Lens' Denied (Core.Maybe Types.ExplicitDeny)
dExplicitDeny = Lens.field @"explicitDeny"
{-# DEPRECATED dExplicitDeny "Use generic-lens or generic-optics with 'explicitDeny' instead." #-}

-- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- /Note:/ Consider using 'implicitDeny' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImplicitDeny :: Lens.Lens' Denied (Core.Maybe Types.ImplicitDeny)
dImplicitDeny = Lens.field @"implicitDeny"
{-# DEPRECATED dImplicitDeny "Use generic-lens or generic-optics with 'implicitDeny' instead." #-}

instance Core.FromJSON Denied where
  parseJSON =
    Core.withObject "Denied" Core.$
      \x ->
        Denied'
          Core.<$> (x Core..:? "explicitDeny") Core.<*> (x Core..:? "implicitDeny")
