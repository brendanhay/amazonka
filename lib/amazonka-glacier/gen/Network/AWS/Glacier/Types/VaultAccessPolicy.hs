{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultAccessPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultAccessPolicy
  ( VaultAccessPolicy (..),

    -- * Smart constructor
    mkVaultAccessPolicy,

    -- * Lenses
    vapPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the vault access policy.
--
-- /See:/ 'mkVaultAccessPolicy' smart constructor.
newtype VaultAccessPolicy = VaultAccessPolicy'
  { policy ::
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

-- | Creates a value of 'VaultAccessPolicy' with the minimum fields required to make a request.
--
-- * 'policy' - The vault access policy.
mkVaultAccessPolicy ::
  VaultAccessPolicy
mkVaultAccessPolicy = VaultAccessPolicy' {policy = Lude.Nothing}

-- | The vault access policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vapPolicy :: Lens.Lens' VaultAccessPolicy (Lude.Maybe Lude.Text)
vapPolicy = Lens.lens (policy :: VaultAccessPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: VaultAccessPolicy)
{-# DEPRECATED vapPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.FromJSON VaultAccessPolicy where
  parseJSON =
    Lude.withObject
      "VaultAccessPolicy"
      (\x -> VaultAccessPolicy' Lude.<$> (x Lude..:? "Policy"))

instance Lude.ToJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy' {..} =
    Lude.object (Lude.catMaybes [("Policy" Lude..=) Lude.<$> policy])
