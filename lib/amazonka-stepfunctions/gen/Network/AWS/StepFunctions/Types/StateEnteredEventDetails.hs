{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateEnteredEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateEnteredEventDetails
  ( StateEnteredEventDetails (..)
  -- * Smart constructor
  , mkStateEnteredEventDetails
  -- * Lenses
  , seedName
  , seedInput
  , seedInputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.Name as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about a state entered during an execution.
--
-- /See:/ 'mkStateEnteredEventDetails' smart constructor.
data StateEnteredEventDetails = StateEnteredEventDetails'
  { name :: Types.Name
    -- ^ The name of the state.
  , input :: Core.Maybe Types.SensitiveData
    -- ^ The string that contains the JSON input data for the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , inputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
    -- ^ Contains details about the input for an execution history event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StateEnteredEventDetails' value with any optional fields omitted.
mkStateEnteredEventDetails
    :: Types.Name -- ^ 'name'
    -> StateEnteredEventDetails
mkStateEnteredEventDetails name
  = StateEnteredEventDetails'{name, input = Core.Nothing,
                              inputDetails = Core.Nothing}

-- | The name of the state.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedName :: Lens.Lens' StateEnteredEventDetails Types.Name
seedName = Lens.field @"name"
{-# INLINEABLE seedName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The string that contains the JSON input data for the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedInput :: Lens.Lens' StateEnteredEventDetails (Core.Maybe Types.SensitiveData)
seedInput = Lens.field @"input"
{-# INLINEABLE seedInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | Contains details about the input for an execution history event.
--
-- /Note:/ Consider using 'inputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seedInputDetails :: Lens.Lens' StateEnteredEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
seedInputDetails = Lens.field @"inputDetails"
{-# INLINEABLE seedInputDetails #-}
{-# DEPRECATED inputDetails "Use generic-lens or generic-optics with 'inputDetails' instead"  #-}

instance Core.FromJSON StateEnteredEventDetails where
        parseJSON
          = Core.withObject "StateEnteredEventDetails" Core.$
              \ x ->
                StateEnteredEventDetails' Core.<$>
                  (x Core..: "name") Core.<*> x Core..:? "input" Core.<*>
                    x Core..:? "inputDetails"
