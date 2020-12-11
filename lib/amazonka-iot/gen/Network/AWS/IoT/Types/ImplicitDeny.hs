-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ImplicitDeny
  ( ImplicitDeny (..),

    -- * Smart constructor
    mkImplicitDeny,

    -- * Lenses
    idPolicies,
  )
where

import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that implicitly denies authorization. When policy doesn't explicitly deny or allow an action on a resource it is considered an implicit deny.
--
-- /See:/ 'mkImplicitDeny' smart constructor.
newtype ImplicitDeny = ImplicitDeny'
  { policies ::
      Lude.Maybe [Policy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImplicitDeny' with the minimum fields required to make a request.
--
-- * 'policies' - Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
mkImplicitDeny ::
  ImplicitDeny
mkImplicitDeny = ImplicitDeny' {policies = Lude.Nothing}

-- | Policies that don't contain a matching allow or deny statement for the specified action on the specified resource.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPolicies :: Lens.Lens' ImplicitDeny (Lude.Maybe [Policy])
idPolicies = Lens.lens (policies :: ImplicitDeny -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: ImplicitDeny)
{-# DEPRECATED idPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON ImplicitDeny where
  parseJSON =
    Lude.withObject
      "ImplicitDeny"
      ( \x ->
          ImplicitDeny'
            Lude.<$> (x Lude..:? "policies" Lude..!= Lude.mempty)
      )
