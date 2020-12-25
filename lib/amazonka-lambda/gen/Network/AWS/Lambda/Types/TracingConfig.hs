{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.TracingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfig
  ( TracingConfig (..),

    -- * Smart constructor
    mkTracingConfig,

    -- * Lenses
    tcMode,
  )
where

import qualified Network.AWS.Lambda.Types.TracingMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The function's AWS X-Ray tracing configuration. To sample and record incoming requests, set @Mode@ to @Active@ .
--
-- /See:/ 'mkTracingConfig' smart constructor.
newtype TracingConfig = TracingConfig'
  { -- | The tracing mode.
    mode :: Core.Maybe Types.TracingMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TracingConfig' value with any optional fields omitted.
mkTracingConfig ::
  TracingConfig
mkTracingConfig = TracingConfig' {mode = Core.Nothing}

-- | The tracing mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMode :: Lens.Lens' TracingConfig (Core.Maybe Types.TracingMode)
tcMode = Lens.field @"mode"
{-# DEPRECATED tcMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Core.FromJSON TracingConfig where
  toJSON TracingConfig {..} =
    Core.object (Core.catMaybes [("Mode" Core..=) Core.<$> mode])
