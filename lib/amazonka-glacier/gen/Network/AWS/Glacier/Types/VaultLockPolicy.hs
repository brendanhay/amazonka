{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.VaultLockPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultLockPolicy
  ( VaultLockPolicy (..),

    -- * Smart constructor
    mkVaultLockPolicy,

    -- * Lenses
    vlpPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the vault lock policy.
--
-- /See:/ 'mkVaultLockPolicy' smart constructor.
newtype VaultLockPolicy = VaultLockPolicy'
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

-- | Creates a value of 'VaultLockPolicy' with the minimum fields required to make a request.
--
-- * 'policy' - The vault lock policy.
mkVaultLockPolicy ::
  VaultLockPolicy
mkVaultLockPolicy = VaultLockPolicy' {policy = Lude.Nothing}

-- | The vault lock policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vlpPolicy :: Lens.Lens' VaultLockPolicy (Lude.Maybe Lude.Text)
vlpPolicy = Lens.lens (policy :: VaultLockPolicy -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: VaultLockPolicy)
{-# DEPRECATED vlpPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.ToJSON VaultLockPolicy where
  toJSON VaultLockPolicy' {..} =
    Lude.object (Lude.catMaybes [("Policy" Lude..=) Lude.<$> policy])
