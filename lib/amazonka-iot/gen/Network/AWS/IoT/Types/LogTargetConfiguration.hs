{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.LogTargetConfiguration
  ( LogTargetConfiguration (..)
  -- * Smart constructor
  , mkLogTargetConfiguration
  -- * Lenses
  , ltcLogLevel
  , ltcLogTarget
  ) where

import qualified Network.AWS.IoT.Types.LogLevel as Types
import qualified Network.AWS.IoT.Types.LogTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The target configuration.
--
-- /See:/ 'mkLogTargetConfiguration' smart constructor.
data LogTargetConfiguration = LogTargetConfiguration'
  { logLevel :: Core.Maybe Types.LogLevel
    -- ^ The logging level.
  , logTarget :: Core.Maybe Types.LogTarget
    -- ^ A log target
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogTargetConfiguration' value with any optional fields omitted.
mkLogTargetConfiguration
    :: LogTargetConfiguration
mkLogTargetConfiguration
  = LogTargetConfiguration'{logLevel = Core.Nothing,
                            logTarget = Core.Nothing}

-- | The logging level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLogLevel :: Lens.Lens' LogTargetConfiguration (Core.Maybe Types.LogLevel)
ltcLogLevel = Lens.field @"logLevel"
{-# INLINEABLE ltcLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

-- | A log target
--
-- /Note:/ Consider using 'logTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLogTarget :: Lens.Lens' LogTargetConfiguration (Core.Maybe Types.LogTarget)
ltcLogTarget = Lens.field @"logTarget"
{-# INLINEABLE ltcLogTarget #-}
{-# DEPRECATED logTarget "Use generic-lens or generic-optics with 'logTarget' instead"  #-}

instance Core.FromJSON LogTargetConfiguration where
        parseJSON
          = Core.withObject "LogTargetConfiguration" Core.$
              \ x ->
                LogTargetConfiguration' Core.<$>
                  (x Core..:? "logLevel") Core.<*> x Core..:? "logTarget"
