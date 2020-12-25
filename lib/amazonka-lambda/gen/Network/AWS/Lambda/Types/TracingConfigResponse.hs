{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.TracingConfigResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfigResponse
  ( TracingConfigResponse (..),

    -- * Smart constructor
    mkTracingConfigResponse,

    -- * Lenses
    tcrMode,
  )
where

import qualified Network.AWS.Lambda.Types.TracingMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The function's AWS X-Ray tracing configuration.
--
-- /See:/ 'mkTracingConfigResponse' smart constructor.
newtype TracingConfigResponse = TracingConfigResponse'
  { -- | The tracing mode.
    mode :: Core.Maybe Types.TracingMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TracingConfigResponse' value with any optional fields omitted.
mkTracingConfigResponse ::
  TracingConfigResponse
mkTracingConfigResponse =
  TracingConfigResponse' {mode = Core.Nothing}

-- | The tracing mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrMode :: Lens.Lens' TracingConfigResponse (Core.Maybe Types.TracingMode)
tcrMode = Lens.field @"mode"
{-# DEPRECATED tcrMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Core.FromJSON TracingConfigResponse where
  parseJSON =
    Core.withObject "TracingConfigResponse" Core.$
      \x -> TracingConfigResponse' Core.<$> (x Core..:? "Mode")
