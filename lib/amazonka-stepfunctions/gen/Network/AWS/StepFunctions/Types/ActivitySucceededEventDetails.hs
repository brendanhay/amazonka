{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
  ( ActivitySucceededEventDetails (..)
  -- * Smart constructor
  , mkActivitySucceededEventDetails
  -- * Lenses
  , asedOutput
  , asedOutputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about an activity that successfully terminated during an execution.
--
-- /See:/ 'mkActivitySucceededEventDetails' smart constructor.
data ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { output :: Core.Maybe Types.SensitiveData
    -- ^ The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
    -- ^ Contains details about the output of an execution history event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivitySucceededEventDetails' value with any optional fields omitted.
mkActivitySucceededEventDetails
    :: ActivitySucceededEventDetails
mkActivitySucceededEventDetails
  = ActivitySucceededEventDetails'{output = Core.Nothing,
                                   outputDetails = Core.Nothing}

-- | The JSON data output by the activity task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedOutput :: Lens.Lens' ActivitySucceededEventDetails (Core.Maybe Types.SensitiveData)
asedOutput = Lens.field @"output"
{-# INLINEABLE asedOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedOutputDetails :: Lens.Lens' ActivitySucceededEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
asedOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE asedOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

instance Core.FromJSON ActivitySucceededEventDetails where
        parseJSON
          = Core.withObject "ActivitySucceededEventDetails" Core.$
              \ x ->
                ActivitySucceededEventDetails' Core.<$>
                  (x Core..:? "output") Core.<*> x Core..:? "outputDetails"
