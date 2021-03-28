{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LoggingConfiguration
  ( LoggingConfiguration (..)
  -- * Smart constructor
  , mkLoggingConfiguration
  -- * Lenses
  , lcDestinations
  , lcIncludeExecutionData
  , lcLevel
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.LogDestination as Types
import qualified Network.AWS.StepFunctions.Types.LogLevel as Types

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
--
-- /See:/ 'mkLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { destinations :: Core.Maybe [Types.LogDestination]
    -- ^ An array of objects that describes where your execution history events will be logged. Limited to size 1. Required, if your log level is not set to @OFF@ .
  , includeExecutionData :: Core.Maybe Core.Bool
    -- ^ Determines whether execution data is included in your log. When set to @false@ , data is excluded.
  , level :: Core.Maybe Types.LogLevel
    -- ^ Defines which category of execution history events are logged.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingConfiguration' value with any optional fields omitted.
mkLoggingConfiguration
    :: LoggingConfiguration
mkLoggingConfiguration
  = LoggingConfiguration'{destinations = Core.Nothing,
                          includeExecutionData = Core.Nothing, level = Core.Nothing}

-- | An array of objects that describes where your execution history events will be logged. Limited to size 1. Required, if your log level is not set to @OFF@ .
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcDestinations :: Lens.Lens' LoggingConfiguration (Core.Maybe [Types.LogDestination])
lcDestinations = Lens.field @"destinations"
{-# INLINEABLE lcDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | Determines whether execution data is included in your log. When set to @false@ , data is excluded.
--
-- /Note:/ Consider using 'includeExecutionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludeExecutionData :: Lens.Lens' LoggingConfiguration (Core.Maybe Core.Bool)
lcIncludeExecutionData = Lens.field @"includeExecutionData"
{-# INLINEABLE lcIncludeExecutionData #-}
{-# DEPRECATED includeExecutionData "Use generic-lens or generic-optics with 'includeExecutionData' instead"  #-}

-- | Defines which category of execution history events are logged.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLevel :: Lens.Lens' LoggingConfiguration (Core.Maybe Types.LogLevel)
lcLevel = Lens.field @"level"
{-# INLINEABLE lcLevel #-}
{-# DEPRECATED level "Use generic-lens or generic-optics with 'level' instead"  #-}

instance Core.FromJSON LoggingConfiguration where
        toJSON LoggingConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("destinations" Core..=) Core.<$> destinations,
                  ("includeExecutionData" Core..=) Core.<$> includeExecutionData,
                  ("level" Core..=) Core.<$> level])

instance Core.FromJSON LoggingConfiguration where
        parseJSON
          = Core.withObject "LoggingConfiguration" Core.$
              \ x ->
                LoggingConfiguration' Core.<$>
                  (x Core..:? "destinations") Core.<*>
                    x Core..:? "includeExecutionData"
                    Core.<*> x Core..:? "level"
