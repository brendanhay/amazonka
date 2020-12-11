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
    tcMode,
  )
where

import Network.AWS.Lambda.Types.TracingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The function's AWS X-Ray tracing configuration.
--
-- /See:/ 'mkTracingConfigResponse' smart constructor.
newtype TracingConfigResponse = TracingConfigResponse'
  { mode ::
      Lude.Maybe TracingMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TracingConfigResponse' with the minimum fields required to make a request.
--
-- * 'mode' - The tracing mode.
mkTracingConfigResponse ::
  TracingConfigResponse
mkTracingConfigResponse =
  TracingConfigResponse' {mode = Lude.Nothing}

-- | The tracing mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcMode :: Lens.Lens' TracingConfigResponse (Lude.Maybe TracingMode)
tcMode = Lens.lens (mode :: TracingConfigResponse -> Lude.Maybe TracingMode) (\s a -> s {mode = a} :: TracingConfigResponse)
{-# DEPRECATED tcMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Lude.FromJSON TracingConfigResponse where
  parseJSON =
    Lude.withObject
      "TracingConfigResponse"
      (\x -> TracingConfigResponse' Lude.<$> (x Lude..:? "Mode"))
