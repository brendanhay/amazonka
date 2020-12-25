{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStrategy
  ( SamplingStrategy (..),

    -- * Smart constructor
    mkSamplingStrategy,

    -- * Lenses
    ssName,
    ssValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.SamplingStrategyName as Types

-- | The name and value of a sampling rule to apply to a trace summary.
--
-- /See:/ 'mkSamplingStrategy' smart constructor.
data SamplingStrategy = SamplingStrategy'
  { -- | The name of a sampling rule.
    name :: Core.Maybe Types.SamplingStrategyName,
    -- | The value of a sampling rule.
    value :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SamplingStrategy' value with any optional fields omitted.
mkSamplingStrategy ::
  SamplingStrategy
mkSamplingStrategy =
  SamplingStrategy' {name = Core.Nothing, value = Core.Nothing}

-- | The name of a sampling rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' SamplingStrategy (Core.Maybe Types.SamplingStrategyName)
ssName = Lens.field @"name"
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of a sampling rule.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssValue :: Lens.Lens' SamplingStrategy (Core.Maybe Core.Double)
ssValue = Lens.field @"value"
{-# DEPRECATED ssValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON SamplingStrategy where
  toJSON SamplingStrategy {..} =
    Core.object
      ( Core.catMaybes
          [("Name" Core..=) Core.<$> name, ("Value" Core..=) Core.<$> value]
      )
