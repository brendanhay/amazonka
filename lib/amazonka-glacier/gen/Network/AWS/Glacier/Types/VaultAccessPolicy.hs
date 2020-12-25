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

import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the vault access policy.
--
-- /See:/ 'mkVaultAccessPolicy' smart constructor.
newtype VaultAccessPolicy = VaultAccessPolicy'
  { -- | The vault access policy.
    policy :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VaultAccessPolicy' value with any optional fields omitted.
mkVaultAccessPolicy ::
  VaultAccessPolicy
mkVaultAccessPolicy = VaultAccessPolicy' {policy = Core.Nothing}

-- | The vault access policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vapPolicy :: Lens.Lens' VaultAccessPolicy (Core.Maybe Types.String)
vapPolicy = Lens.field @"policy"
{-# DEPRECATED vapPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Core.FromJSON VaultAccessPolicy where
  toJSON VaultAccessPolicy {..} =
    Core.object (Core.catMaybes [("Policy" Core..=) Core.<$> policy])

instance Core.FromJSON VaultAccessPolicy where
  parseJSON =
    Core.withObject "VaultAccessPolicy" Core.$
      \x -> VaultAccessPolicy' Core.<$> (x Core..:? "Policy")
