{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionListItem
  ( ExecutionListItem (..)
  -- * Smart constructor
  , mkExecutionListItem
  -- * Lenses
  , eliExecutionArn
  , eliStateMachineArn
  , eliName
  , eliStatus
  , eliStartDate
  , eliStopDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.Arn as Types
import qualified Network.AWS.StepFunctions.Types.ExecutionStatus as Types
import qualified Network.AWS.StepFunctions.Types.Name as Types

-- | Contains details about an execution.
--
-- /See:/ 'mkExecutionListItem' smart constructor.
data ExecutionListItem = ExecutionListItem'
  { executionArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that identifies the execution.
  , stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the executed state machine.
  , name :: Types.Name
    -- ^ The name of the execution.
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
  , status :: Types.ExecutionStatus
    -- ^ The current status of the execution.
  , startDate :: Core.NominalDiffTime
    -- ^ The date the execution started.
  , stopDate :: Core.Maybe Core.NominalDiffTime
    -- ^ If the execution already ended, the date the execution stopped.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExecutionListItem' value with any optional fields omitted.
mkExecutionListItem
    :: Types.Arn -- ^ 'executionArn'
    -> Types.Arn -- ^ 'stateMachineArn'
    -> Types.Name -- ^ 'name'
    -> Types.ExecutionStatus -- ^ 'status'
    -> Core.NominalDiffTime -- ^ 'startDate'
    -> ExecutionListItem
mkExecutionListItem executionArn stateMachineArn name status
  startDate
  = ExecutionListItem'{executionArn, stateMachineArn, name, status,
                       startDate, stopDate = Core.Nothing}

-- | The Amazon Resource Name (ARN) that identifies the execution.
--
-- /Note:/ Consider using 'executionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eliExecutionArn :: Lens.Lens' ExecutionListItem Types.Arn
eliExecutionArn = Lens.field @"executionArn"
{-# INLINEABLE eliExecutionArn #-}
{-# DEPRECATED executionArn "Use generic-lens or generic-optics with 'executionArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the executed state machine.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eliStateMachineArn :: Lens.Lens' ExecutionListItem Types.Arn
eliStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE eliStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

-- | The name of the execution.
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
eliName :: Lens.Lens' ExecutionListItem Types.Name
eliName = Lens.field @"name"
{-# INLINEABLE eliName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eliStatus :: Lens.Lens' ExecutionListItem Types.ExecutionStatus
eliStatus = Lens.field @"status"
{-# INLINEABLE eliStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date the execution started.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eliStartDate :: Lens.Lens' ExecutionListItem Core.NominalDiffTime
eliStartDate = Lens.field @"startDate"
{-# INLINEABLE eliStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | If the execution already ended, the date the execution stopped.
--
-- /Note:/ Consider using 'stopDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eliStopDate :: Lens.Lens' ExecutionListItem (Core.Maybe Core.NominalDiffTime)
eliStopDate = Lens.field @"stopDate"
{-# INLINEABLE eliStopDate #-}
{-# DEPRECATED stopDate "Use generic-lens or generic-optics with 'stopDate' instead"  #-}

instance Core.FromJSON ExecutionListItem where
        parseJSON
          = Core.withObject "ExecutionListItem" Core.$
              \ x ->
                ExecutionListItem' Core.<$>
                  (x Core..: "executionArn") Core.<*> x Core..: "stateMachineArn"
                    Core.<*> x Core..: "name"
                    Core.<*> x Core..: "status"
                    Core.<*> x Core..: "startDate"
                    Core.<*> x Core..:? "stopDate"
