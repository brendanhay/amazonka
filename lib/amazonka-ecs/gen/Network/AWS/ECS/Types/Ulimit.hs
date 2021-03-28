{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Ulimit
  ( Ulimit (..)
  -- * Smart constructor
  , mkUlimit
  -- * Lenses
  , uName
  , uSoftLimit
  , uHardLimit
  ) where

import qualified Network.AWS.ECS.Types.UlimitName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @ulimit@ settings to pass to the container.
--
-- /See:/ 'mkUlimit' smart constructor.
data Ulimit = Ulimit'
  { name :: Types.UlimitName
    -- ^ The @type@ of the @ulimit@ .
  , softLimit :: Core.Int
    -- ^ The soft limit for the ulimit type.
  , hardLimit :: Core.Int
    -- ^ The hard limit for the ulimit type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ulimit' value with any optional fields omitted.
mkUlimit
    :: Types.UlimitName -- ^ 'name'
    -> Core.Int -- ^ 'softLimit'
    -> Core.Int -- ^ 'hardLimit'
    -> Ulimit
mkUlimit name softLimit hardLimit
  = Ulimit'{name, softLimit, hardLimit}

-- | The @type@ of the @ulimit@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' Ulimit Types.UlimitName
uName = Lens.field @"name"
{-# INLINEABLE uName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The soft limit for the ulimit type.
--
-- /Note:/ Consider using 'softLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSoftLimit :: Lens.Lens' Ulimit Core.Int
uSoftLimit = Lens.field @"softLimit"
{-# INLINEABLE uSoftLimit #-}
{-# DEPRECATED softLimit "Use generic-lens or generic-optics with 'softLimit' instead"  #-}

-- | The hard limit for the ulimit type.
--
-- /Note:/ Consider using 'hardLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHardLimit :: Lens.Lens' Ulimit Core.Int
uHardLimit = Lens.field @"hardLimit"
{-# INLINEABLE uHardLimit #-}
{-# DEPRECATED hardLimit "Use generic-lens or generic-optics with 'hardLimit' instead"  #-}

instance Core.FromJSON Ulimit where
        toJSON Ulimit{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("softLimit" Core..= softLimit),
                  Core.Just ("hardLimit" Core..= hardLimit)])

instance Core.FromJSON Ulimit where
        parseJSON
          = Core.withObject "Ulimit" Core.$
              \ x ->
                Ulimit' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "softLimit" Core.<*>
                    x Core..: "hardLimit"
