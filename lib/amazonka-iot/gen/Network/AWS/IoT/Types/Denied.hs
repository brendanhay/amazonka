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
    dImplicitDeny,
    dExplicitDeny,
  )
where

import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ImplicitDeny
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information that denied the authorization.
--
-- /See:/ 'mkDenied' smart constructor.
data Denied = Denied'
  { -- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
    implicitDeny :: Lude.Maybe ImplicitDeny,
    -- | Information that explicitly denies the authorization.
    explicitDeny :: Lude.Maybe ExplicitDeny
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Denied' with the minimum fields required to make a request.
--
-- * 'implicitDeny' - Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
-- * 'explicitDeny' - Information that explicitly denies the authorization.
mkDenied ::
  Denied
mkDenied =
  Denied' {implicitDeny = Lude.Nothing, explicitDeny = Lude.Nothing}

-- | Information that implicitly denies the authorization. When a policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- /Note:/ Consider using 'implicitDeny' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dImplicitDeny :: Lens.Lens' Denied (Lude.Maybe ImplicitDeny)
dImplicitDeny = Lens.lens (implicitDeny :: Denied -> Lude.Maybe ImplicitDeny) (\s a -> s {implicitDeny = a} :: Denied)
{-# DEPRECATED dImplicitDeny "Use generic-lens or generic-optics with 'implicitDeny' instead." #-}

-- | Information that explicitly denies the authorization.
--
-- /Note:/ Consider using 'explicitDeny' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dExplicitDeny :: Lens.Lens' Denied (Lude.Maybe ExplicitDeny)
dExplicitDeny = Lens.lens (explicitDeny :: Denied -> Lude.Maybe ExplicitDeny) (\s a -> s {explicitDeny = a} :: Denied)
{-# DEPRECATED dExplicitDeny "Use generic-lens or generic-optics with 'explicitDeny' instead." #-}

instance Lude.FromJSON Denied where
  parseJSON =
    Lude.withObject
      "Denied"
      ( \x ->
          Denied'
            Lude.<$> (x Lude..:? "implicitDeny") Lude.<*> (x Lude..:? "explicitDeny")
      )
