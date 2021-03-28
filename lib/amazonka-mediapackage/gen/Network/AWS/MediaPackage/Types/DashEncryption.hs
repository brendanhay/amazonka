{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.DashEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.DashEncryption
  ( DashEncryption (..)
  -- * Smart constructor
  , mkDashEncryption
  -- * Lenses
  , deSpekeKeyProvider
  , deKeyRotationIntervalSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.SpekeKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
--
-- /See:/ 'mkDashEncryption' smart constructor.
data DashEncryption = DashEncryption'
  { spekeKeyProvider :: Types.SpekeKeyProvider
  , keyRotationIntervalSeconds :: Core.Maybe Core.Int
    -- ^ Time (in seconds) between each encryption key rotation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DashEncryption' value with any optional fields omitted.
mkDashEncryption
    :: Types.SpekeKeyProvider -- ^ 'spekeKeyProvider'
    -> DashEncryption
mkDashEncryption spekeKeyProvider
  = DashEncryption'{spekeKeyProvider,
                    keyRotationIntervalSeconds = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deSpekeKeyProvider :: Lens.Lens' DashEncryption Types.SpekeKeyProvider
deSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# INLINEABLE deSpekeKeyProvider #-}
{-# DEPRECATED spekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead"  #-}

-- | Time (in seconds) between each encryption key rotation.
--
-- /Note:/ Consider using 'keyRotationIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deKeyRotationIntervalSeconds :: Lens.Lens' DashEncryption (Core.Maybe Core.Int)
deKeyRotationIntervalSeconds = Lens.field @"keyRotationIntervalSeconds"
{-# INLINEABLE deKeyRotationIntervalSeconds #-}
{-# DEPRECATED keyRotationIntervalSeconds "Use generic-lens or generic-optics with 'keyRotationIntervalSeconds' instead"  #-}

instance Core.FromJSON DashEncryption where
        toJSON DashEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("spekeKeyProvider" Core..= spekeKeyProvider),
                  ("keyRotationIntervalSeconds" Core..=) Core.<$>
                    keyRotationIntervalSeconds])

instance Core.FromJSON DashEncryption where
        parseJSON
          = Core.withObject "DashEncryption" Core.$
              \ x ->
                DashEncryption' Core.<$>
                  (x Core..: "spekeKeyProvider") Core.<*>
                    x Core..:? "keyRotationIntervalSeconds"
