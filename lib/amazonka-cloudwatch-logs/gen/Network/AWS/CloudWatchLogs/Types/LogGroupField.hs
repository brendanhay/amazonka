{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroupField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.LogGroupField
  ( LogGroupField (..)
  -- * Smart constructor
  , mkLogGroupField
  -- * Lenses
  , lgfName
  , lgfPercent
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.Field as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The fields contained in log events found by a @GetLogGroupFields@ operation, along with the percentage of queried log events in which each field appears.
--
-- /See:/ 'mkLogGroupField' smart constructor.
data LogGroupField = LogGroupField'
  { name :: Core.Maybe Types.Field
    -- ^ The name of a log field.
  , percent :: Core.Maybe Core.Natural
    -- ^ The percentage of log events queried that contained the field.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogGroupField' value with any optional fields omitted.
mkLogGroupField
    :: LogGroupField
mkLogGroupField
  = LogGroupField'{name = Core.Nothing, percent = Core.Nothing}

-- | The name of a log field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfName :: Lens.Lens' LogGroupField (Core.Maybe Types.Field)
lgfName = Lens.field @"name"
{-# INLINEABLE lgfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The percentage of log events queried that contained the field.
--
-- /Note:/ Consider using 'percent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgfPercent :: Lens.Lens' LogGroupField (Core.Maybe Core.Natural)
lgfPercent = Lens.field @"percent"
{-# INLINEABLE lgfPercent #-}
{-# DEPRECATED percent "Use generic-lens or generic-optics with 'percent' instead"  #-}

instance Core.FromJSON LogGroupField where
        parseJSON
          = Core.withObject "LogGroupField" Core.$
              \ x ->
                LogGroupField' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "percent"
