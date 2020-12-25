{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.CmafEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafEncryption
  ( CmafEncryption (..),

    -- * Smart constructor
    mkCmafEncryption,

    -- * Lenses
    ceSpekeKeyProvider,
    ceKeyRotationIntervalSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.SpekeKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | A Common Media Application Format (CMAF) encryption configuration.
--
-- /See:/ 'mkCmafEncryption' smart constructor.
data CmafEncryption = CmafEncryption'
  { spekeKeyProvider :: Types.SpekeKeyProvider,
    -- | Time (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmafEncryption' value with any optional fields omitted.
mkCmafEncryption ::
  -- | 'spekeKeyProvider'
  Types.SpekeKeyProvider ->
  CmafEncryption
mkCmafEncryption spekeKeyProvider =
  CmafEncryption'
    { spekeKeyProvider,
      keyRotationIntervalSeconds = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceSpekeKeyProvider :: Lens.Lens' CmafEncryption Types.SpekeKeyProvider
ceSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# DEPRECATED ceSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

-- | Time (in seconds) between each encryption key rotation.
--
-- /Note:/ Consider using 'keyRotationIntervalSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceKeyRotationIntervalSeconds :: Lens.Lens' CmafEncryption (Core.Maybe Core.Int)
ceKeyRotationIntervalSeconds = Lens.field @"keyRotationIntervalSeconds"
{-# DEPRECATED ceKeyRotationIntervalSeconds "Use generic-lens or generic-optics with 'keyRotationIntervalSeconds' instead." #-}

instance Core.FromJSON CmafEncryption where
  toJSON CmafEncryption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("spekeKeyProvider" Core..= spekeKeyProvider),
            ("keyRotationIntervalSeconds" Core..=)
              Core.<$> keyRotationIntervalSeconds
          ]
      )

instance Core.FromJSON CmafEncryption where
  parseJSON =
    Core.withObject "CmafEncryption" Core.$
      \x ->
        CmafEncryption'
          Core.<$> (x Core..: "spekeKeyProvider")
          Core.<*> (x Core..:? "keyRotationIntervalSeconds")
