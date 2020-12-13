{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionStatus
  ( QueryExecutionStatus (..),

    -- * Smart constructor
    mkQueryExecutionStatus,

    -- * Lenses
    qesState,
    qesStateChangeReason,
    qesSubmissionDateTime,
    qesCompletionDateTime,
  )
where

import Network.AWS.Athena.Types.QueryExecutionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- /See:/ 'mkQueryExecutionStatus' smart constructor.
data QueryExecutionStatus = QueryExecutionStatus'
  { -- | The state of query execution. @QUEUED@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @RUNNING@ indicates that the query is in execution phase. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
    state :: Lude.Maybe QueryExecutionState,
    -- | Further detail about the status of the query.
    stateChangeReason :: Lude.Maybe Lude.Text,
    -- | The date and time that the query was submitted.
    submissionDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The date and time that the query completed.
    completionDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryExecutionStatus' with the minimum fields required to make a request.
--
-- * 'state' - The state of query execution. @QUEUED@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @RUNNING@ indicates that the query is in execution phase. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
-- * 'stateChangeReason' - Further detail about the status of the query.
-- * 'submissionDateTime' - The date and time that the query was submitted.
-- * 'completionDateTime' - The date and time that the query completed.
mkQueryExecutionStatus ::
  QueryExecutionStatus
mkQueryExecutionStatus =
  QueryExecutionStatus'
    { state = Lude.Nothing,
      stateChangeReason = Lude.Nothing,
      submissionDateTime = Lude.Nothing,
      completionDateTime = Lude.Nothing
    }

-- | The state of query execution. @QUEUED@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @RUNNING@ indicates that the query is in execution phase. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesState :: Lens.Lens' QueryExecutionStatus (Lude.Maybe QueryExecutionState)
qesState = Lens.lens (state :: QueryExecutionStatus -> Lude.Maybe QueryExecutionState) (\s a -> s {state = a} :: QueryExecutionStatus)
{-# DEPRECATED qesState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Further detail about the status of the query.
--
-- /Note:/ Consider using 'stateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesStateChangeReason :: Lens.Lens' QueryExecutionStatus (Lude.Maybe Lude.Text)
qesStateChangeReason = Lens.lens (stateChangeReason :: QueryExecutionStatus -> Lude.Maybe Lude.Text) (\s a -> s {stateChangeReason = a} :: QueryExecutionStatus)
{-# DEPRECATED qesStateChangeReason "Use generic-lens or generic-optics with 'stateChangeReason' instead." #-}

-- | The date and time that the query was submitted.
--
-- /Note:/ Consider using 'submissionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesSubmissionDateTime :: Lens.Lens' QueryExecutionStatus (Lude.Maybe Lude.Timestamp)
qesSubmissionDateTime = Lens.lens (submissionDateTime :: QueryExecutionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {submissionDateTime = a} :: QueryExecutionStatus)
{-# DEPRECATED qesSubmissionDateTime "Use generic-lens or generic-optics with 'submissionDateTime' instead." #-}

-- | The date and time that the query completed.
--
-- /Note:/ Consider using 'completionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qesCompletionDateTime :: Lens.Lens' QueryExecutionStatus (Lude.Maybe Lude.Timestamp)
qesCompletionDateTime = Lens.lens (completionDateTime :: QueryExecutionStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionDateTime = a} :: QueryExecutionStatus)
{-# DEPRECATED qesCompletionDateTime "Use generic-lens or generic-optics with 'completionDateTime' instead." #-}

instance Lude.FromJSON QueryExecutionStatus where
  parseJSON =
    Lude.withObject
      "QueryExecutionStatus"
      ( \x ->
          QueryExecutionStatus'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StateChangeReason")
            Lude.<*> (x Lude..:? "SubmissionDateTime")
            Lude.<*> (x Lude..:? "CompletionDateTime")
      )
