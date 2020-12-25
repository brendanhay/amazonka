{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionLimits
  ( ProtectionLimits (..),

    -- * Smart constructor
    mkProtectionLimits,

    -- * Lenses
    plProtectedResourceTypeLimits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.Limit as Types

-- | Limits settings on protections for your subscription.
--
-- /See:/ 'mkProtectionLimits' smart constructor.
newtype ProtectionLimits = ProtectionLimits'
  { -- | The maximum number of resource types that you can specify in a protection.
    protectedResourceTypeLimits :: [Types.Limit]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProtectionLimits' value with any optional fields omitted.
mkProtectionLimits ::
  ProtectionLimits
mkProtectionLimits =
  ProtectionLimits' {protectedResourceTypeLimits = Core.mempty}

-- | The maximum number of resource types that you can specify in a protection.
--
-- /Note:/ Consider using 'protectedResourceTypeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plProtectedResourceTypeLimits :: Lens.Lens' ProtectionLimits [Types.Limit]
plProtectedResourceTypeLimits = Lens.field @"protectedResourceTypeLimits"
{-# DEPRECATED plProtectedResourceTypeLimits "Use generic-lens or generic-optics with 'protectedResourceTypeLimits' instead." #-}

instance Core.FromJSON ProtectionLimits where
  parseJSON =
    Core.withObject "ProtectionLimits" Core.$
      \x ->
        ProtectionLimits'
          Core.<$> (x Core..:? "ProtectedResourceTypeLimits" Core..!= Core.mempty)
