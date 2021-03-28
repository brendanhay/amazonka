{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.QueryExecutionStatus
  ( QueryExecutionStatus (..)
  -- * Smart constructor
  , mkQueryExecutionStatus
  -- * Lenses
  , qesCompletionDateTime
  , qesState
  , qesStateChangeReason
  , qesSubmissionDateTime
  ) where

import qualified Network.AWS.Athena.Types.QueryExecutionState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- /See:/ 'mkQueryExecutionStatus' smart constructor.
data QueryExecutionStatus = QueryExecutionStatus'
  { completionDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the query completed.
  , state :: Core.Maybe Types.QueryExecutionState
    -- ^ The state of query execution. @QUEUED@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @RUNNING@ indicates that the query is in execution phase. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
  , stateChangeReason :: Core.Maybe Core.Text
    -- ^ Further detail about the status of the query.
  , submissionDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the query was submitted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'QueryExecutionStatus' value with any optional fields omitted.
mkQueryExecutionStatus
    :: QueryExecutionStatus
mkQueryExecutionStatus
  = QueryExecutionStatus'{completionDateTime = Core.Nothing,
                          state = Core.Nothing, stateChangeReason = Core.Nothing,
                          submissionDateTime = Core.Nothing}

-- | The date and time that the query completed.
--
-- /Note:/ Consider using 'completionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesCompletionDateTime :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.NominalDiffTime)
qesCompletionDateTime = Lens.field @"completionDateTime"
{-# INLINEABLE qesCompletionDateTime #-}
{-# DEPRECATED completionDateTime "Use generic-lens or generic-optics with 'completionDateTime' instead"  #-}

-- | The state of query execution. @QUEUED@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @RUNNING@ indicates that the query is in execution phase. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesState :: Lens.Lens' QueryExecutionStatus (Core.Maybe Types.QueryExecutionState)
qesState = Lens.field @"state"
{-# INLINEABLE qesState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Further detail about the status of the query.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesStateChangeReason :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.Text)
qesStateChangeReason = Lens.field @"stateChangeReason"
{-# INLINEABLE qesStateChangeReason #-}
{-# DEPRECATED stateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead"  #-}

-- | The date and time that the query was submitted.
--
-- /Note:/ Consider using 'submissionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesSubmissionDateTime :: Lens.Lens' QueryExecutionStatus (Core.Maybe Core.NominalDiffTime)
qesSubmissionDateTime = Lens.field @"submissionDateTime"
{-# INLINEABLE qesSubmissionDateTime #-}
{-# DEPRECATED submissionDateTime "Use generic-lens or generic-optics with 'submissionDateTime' instead"  #-}

instance Core.FromJSON QueryExecutionStatus where
        parseJSON
          = Core.withObject "QueryExecutionStatus" Core.$
              \ x ->
                QueryExecutionStatus' Core.<$>
                  (x Core..:? "CompletionDateTime") Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateChangeReason"
                    Core.<*> x Core..:? "SubmissionDateTime"
