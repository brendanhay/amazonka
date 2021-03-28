{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.StepExecutionFilter
  ( StepExecutionFilter (..)
  -- * Smart constructor
  , mkStepExecutionFilter
  -- * Lenses
  , sefKey
  , sefValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.StepExecutionFilterKey as Types
import qualified Network.AWS.SSM.Types.StepExecutionFilterValue as Types

-- | A filter to limit the amount of step execution information returned by the call.
--
-- /See:/ 'mkStepExecutionFilter' smart constructor.
data StepExecutionFilter = StepExecutionFilter'
  { key :: Types.StepExecutionFilterKey
    -- ^ One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
  , values :: Core.NonEmpty Types.StepExecutionFilterValue
    -- ^ The values of the filter key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StepExecutionFilter' value with any optional fields omitted.
mkStepExecutionFilter
    :: Types.StepExecutionFilterKey -- ^ 'key'
    -> Core.NonEmpty Types.StepExecutionFilterValue -- ^ 'values'
    -> StepExecutionFilter
mkStepExecutionFilter key values
  = StepExecutionFilter'{key, values}

-- | One or more keys to limit the results. Valid filter keys include the following: StepName, Action, StepExecutionId, StepExecutionStatus, StartTimeBefore, StartTimeAfter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sefKey :: Lens.Lens' StepExecutionFilter Types.StepExecutionFilterKey
sefKey = Lens.field @"key"
{-# INLINEABLE sefKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The values of the filter key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sefValues :: Lens.Lens' StepExecutionFilter (Core.NonEmpty Types.StepExecutionFilterValue)
sefValues = Lens.field @"values"
{-# INLINEABLE sefValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON StepExecutionFilter where
        toJSON StepExecutionFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values)])
