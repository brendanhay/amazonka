-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExplicitDeny
  ( ExplicitDeny (..),

    -- * Smart constructor
    mkExplicitDeny,

    -- * Lenses
    edPolicies,
  )
where

import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information that explicitly denies authorization.
--
-- /See:/ 'mkExplicitDeny' smart constructor.
newtype ExplicitDeny = ExplicitDeny'
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

-- | Creates a value of 'ExplicitDeny' with the minimum fields required to make a request.
--
-- * 'policies' - The policies that denied the authorization.
mkExplicitDeny ::
  ExplicitDeny
mkExplicitDeny = ExplicitDeny' {policies = Lude.Nothing}

-- | The policies that denied the authorization.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPolicies :: Lens.Lens' ExplicitDeny (Lude.Maybe [Policy])
edPolicies = Lens.lens (policies :: ExplicitDeny -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: ExplicitDeny)
{-# DEPRECATED edPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON ExplicitDeny where
  parseJSON =
    Lude.withObject
      "ExplicitDeny"
      ( \x ->
          ExplicitDeny'
            Lude.<$> (x Lude..:? "policies" Lude..!= Lude.mempty)
      )
