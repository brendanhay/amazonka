{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateExitedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateExitedEventDetails
  ( StateExitedEventDetails (..)
  -- * Smart constructor
  , mkStateExitedEventDetails
  -- * Lenses
  , sName
  , sOutput
  , sOutputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails as Types
import qualified Network.AWS.StepFunctions.Types.Name as Types
import qualified Network.AWS.StepFunctions.Types.SensitiveData as Types

-- | Contains details about an exit from a state during an execution.
--
-- /See:/ 'mkStateExitedEventDetails' smart constructor.
data StateExitedEventDetails = StateExitedEventDetails'
  { name :: Types.Name
    -- ^ The name of the state.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@ 
--
--
--     * wildcard characters @? *@ 
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@ 
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
  , output :: Core.Maybe Types.SensitiveData
    -- ^ The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
  , outputDetails :: Core.Maybe Types.HistoryEventExecutionDataDetails
    -- ^ Contains details about the output of an execution history event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StateExitedEventDetails' value with any optional fields omitted.
mkStateExitedEventDetails
    :: Types.Name -- ^ 'name'
    -> StateExitedEventDetails
mkStateExitedEventDetails name
  = StateExitedEventDetails'{name, output = Core.Nothing,
                             outputDetails = Core.Nothing}

-- | The name of the state.
--
-- A name must /not/ contain:
--
--     * white space
--
--
--     * brackets @< > { } [ ]@ 
--
--
--     * wildcard characters @? *@ 
--
--
--     * special characters @" # % \ ^ | ~ ` $ & , ; : /@ 
--
--
--     * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
--
-- To enable logging with CloudWatch Logs, the name should only contain 0-9, A-Z, a-z, - and _.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StateExitedEventDetails Types.Name
sName = Lens.field @"name"
{-# INLINEABLE sName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The JSON output data of the state. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutput :: Lens.Lens' StateExitedEventDetails (Core.Maybe Types.SensitiveData)
sOutput = Lens.field @"output"
{-# INLINEABLE sOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | Contains details about the output of an execution history event.
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputDetails :: Lens.Lens' StateExitedEventDetails (Core.Maybe Types.HistoryEventExecutionDataDetails)
sOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE sOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

instance Core.FromJSON StateExitedEventDetails where
        parseJSON
          = Core.withObject "StateExitedEventDetails" Core.$
              \ x ->
                StateExitedEventDetails' Core.<$>
                  (x Core..: "name") Core.<*> x Core..:? "output" Core.<*>
                    x Core..:? "outputDetails"
