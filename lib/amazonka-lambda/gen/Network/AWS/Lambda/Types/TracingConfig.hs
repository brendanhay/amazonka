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
    tMode,
  )
where

import Network.AWS.Lambda.Types.TracingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The function's AWS X-Ray tracing configuration. To sample and record incoming requests, set @Mode@ to @Active@ .
--
-- /See:/ 'mkTracingConfig' smart constructor.
newtype TracingConfig = TracingConfig'
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

-- | Creates a value of 'TracingConfig' with the minimum fields required to make a request.
--
-- * 'mode' - The tracing mode.
mkTracingConfig ::
  TracingConfig
mkTracingConfig = TracingConfig' {mode = Lude.Nothing}

-- | The tracing mode.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tMode :: Lens.Lens' TracingConfig (Lude.Maybe TracingMode)
tMode = Lens.lens (mode :: TracingConfig -> Lude.Maybe TracingMode) (\s a -> s {mode = a} :: TracingConfig)
{-# DEPRECATED tMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Lude.ToJSON TracingConfig where
  toJSON TracingConfig' {..} =
    Lude.object (Lude.catMaybes [("Mode" Lude..=) Lude.<$> mode])
