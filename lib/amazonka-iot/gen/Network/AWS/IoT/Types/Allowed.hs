{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Allowed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Allowed
  ( Allowed (..),

    -- * Smart constructor
    mkAllowed,

    -- * Lenses
    aPolicies,
  )
where

import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information that allowed the authorization.
--
-- /See:/ 'mkAllowed' smart constructor.
newtype Allowed = Allowed'
  { -- | A list of policies that allowed the authentication.
    policies :: Lude.Maybe [Policy]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Allowed' with the minimum fields required to make a request.
--
-- * 'policies' - A list of policies that allowed the authentication.
mkAllowed ::
  Allowed
mkAllowed = Allowed' {policies = Lude.Nothing}

-- | A list of policies that allowed the authentication.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPolicies :: Lens.Lens' Allowed (Lude.Maybe [Policy])
aPolicies = Lens.lens (policies :: Allowed -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: Allowed)
{-# DEPRECATED aPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromJSON Allowed where
  parseJSON =
    Lude.withObject
      "Allowed"
      ( \x ->
          Allowed' Lude.<$> (x Lude..:? "policies" Lude..!= Lude.mempty)
      )
