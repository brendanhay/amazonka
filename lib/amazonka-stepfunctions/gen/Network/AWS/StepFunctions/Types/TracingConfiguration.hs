{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TracingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.TracingConfiguration
  ( TracingConfiguration (..)
  -- * Smart constructor
  , mkTracingConfiguration
  -- * Lenses
  , tcEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Selects whether or not the state machine's AWS X-Ray tracing is enabled. Default is @false@ 
--
-- /See:/ 'mkTracingConfiguration' smart constructor.
newtype TracingConfiguration = TracingConfiguration'
  { enabled :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , AWS X-Ray tracing is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TracingConfiguration' value with any optional fields omitted.
mkTracingConfiguration
    :: TracingConfiguration
mkTracingConfiguration
  = TracingConfiguration'{enabled = Core.Nothing}

-- | When set to @true@ , AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEnabled :: Lens.Lens' TracingConfiguration (Core.Maybe Core.Bool)
tcEnabled = Lens.field @"enabled"
{-# INLINEABLE tcEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON TracingConfiguration where
        toJSON TracingConfiguration{..}
          = Core.object
              (Core.catMaybes [("enabled" Core..=) Core.<$> enabled])

instance Core.FromJSON TracingConfiguration where
        parseJSON
          = Core.withObject "TracingConfiguration" Core.$
              \ x -> TracingConfiguration' Core.<$> (x Core..:? "enabled")
