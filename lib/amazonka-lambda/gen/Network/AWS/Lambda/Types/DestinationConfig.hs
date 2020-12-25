{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DestinationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.DestinationConfig
  ( DestinationConfig (..),

    -- * Smart constructor
    mkDestinationConfig,

    -- * Lenses
    dcOnFailure,
    dcOnSuccess,
  )
where

import qualified Network.AWS.Lambda.Types.OnFailure as Types
import qualified Network.AWS.Lambda.Types.OnSuccess as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A configuration object that specifies the destination of an event after Lambda processes it.
--
-- /See:/ 'mkDestinationConfig' smart constructor.
data DestinationConfig = DestinationConfig'
  { -- | The destination configuration for failed invocations.
    onFailure :: Core.Maybe Types.OnFailure,
    -- | The destination configuration for successful invocations.
    onSuccess :: Core.Maybe Types.OnSuccess
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationConfig' value with any optional fields omitted.
mkDestinationConfig ::
  DestinationConfig
mkDestinationConfig =
  DestinationConfig'
    { onFailure = Core.Nothing,
      onSuccess = Core.Nothing
    }

-- | The destination configuration for failed invocations.
--
-- /Note:/ Consider using 'onFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOnFailure :: Lens.Lens' DestinationConfig (Core.Maybe Types.OnFailure)
dcOnFailure = Lens.field @"onFailure"
{-# DEPRECATED dcOnFailure "Use generic-lens or generic-optics with 'onFailure' instead." #-}

-- | The destination configuration for successful invocations.
--
-- /Note:/ Consider using 'onSuccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcOnSuccess :: Lens.Lens' DestinationConfig (Core.Maybe Types.OnSuccess)
dcOnSuccess = Lens.field @"onSuccess"
{-# DEPRECATED dcOnSuccess "Use generic-lens or generic-optics with 'onSuccess' instead." #-}

instance Core.FromJSON DestinationConfig where
  toJSON DestinationConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("OnFailure" Core..=) Core.<$> onFailure,
            ("OnSuccess" Core..=) Core.<$> onSuccess
          ]
      )

instance Core.FromJSON DestinationConfig where
  parseJSON =
    Core.withObject "DestinationConfig" Core.$
      \x ->
        DestinationConfig'
          Core.<$> (x Core..:? "OnFailure") Core.<*> (x Core..:? "OnSuccess")
