{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecution
  ( ActionExecution (..),

    -- * Smart constructor
    mkActionExecution,

    -- * Lenses
    aeLastUpdatedBy,
    aeSummary,
    aeStatus,
    aeLastStatusChange,
    aeToken,
    aeExternalExecutionURL,
    aeExternalExecutionId,
    aeErrorDetails,
    aePercentComplete,
    aeActionExecutionId,
  )
where

import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.CodePipeline.Types.ErrorDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the run of an action.
--
-- /See:/ 'mkActionExecution' smart constructor.
data ActionExecution = ActionExecution'
  { lastUpdatedBy ::
      Lude.Maybe Lude.Text,
    summary :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe ActionExecutionStatus,
    lastStatusChange :: Lude.Maybe Lude.Timestamp,
    token :: Lude.Maybe Lude.Text,
    externalExecutionURL :: Lude.Maybe Lude.Text,
    externalExecutionId :: Lude.Maybe Lude.Text,
    errorDetails :: Lude.Maybe ErrorDetails,
    percentComplete :: Lude.Maybe Lude.Natural,
    actionExecutionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecution' with the minimum fields required to make a request.
--
-- * 'actionExecutionId' - ID of the workflow action execution in the current stage. Use the 'GetPipelineState' action to retrieve the current action execution details of the current stage.
-- * 'errorDetails' - The details of an error returned by a URL external to AWS.
-- * 'externalExecutionId' - The external ID of the run of the action.
-- * 'externalExecutionURL' - The URL of a resource external to AWS that is used when running the action (for example, an external repository URL).
-- * 'lastStatusChange' - The last status change of the action.
-- * 'lastUpdatedBy' - The ARN of the user who last changed the pipeline.
-- * 'percentComplete' - A percentage of completeness of the action as it runs.
-- * 'status' - The status of the action, or for a completed action, the last status of the action.
-- * 'summary' - A summary of the run of the action.
-- * 'token' - The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the @GetPipelineState@ command. It is used to validate that the approval request corresponding to this token is still valid.
mkActionExecution ::
  ActionExecution
mkActionExecution =
  ActionExecution'
    { lastUpdatedBy = Lude.Nothing,
      summary = Lude.Nothing,
      status = Lude.Nothing,
      lastStatusChange = Lude.Nothing,
      token = Lude.Nothing,
      externalExecutionURL = Lude.Nothing,
      externalExecutionId = Lude.Nothing,
      errorDetails = Lude.Nothing,
      percentComplete = Lude.Nothing,
      actionExecutionId = Lude.Nothing
    }

-- | The ARN of the user who last changed the pipeline.
--
-- /Note:/ Consider using 'lastUpdatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastUpdatedBy :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeLastUpdatedBy = Lens.lens (lastUpdatedBy :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {lastUpdatedBy = a} :: ActionExecution)
{-# DEPRECATED aeLastUpdatedBy "Use generic-lens or generic-optics with 'lastUpdatedBy' instead." #-}

-- | A summary of the run of the action.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeSummary :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeSummary = Lens.lens (summary :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {summary = a} :: ActionExecution)
{-# DEPRECATED aeSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The status of the action, or for a completed action, the last status of the action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatus :: Lens.Lens' ActionExecution (Lude.Maybe ActionExecutionStatus)
aeStatus = Lens.lens (status :: ActionExecution -> Lude.Maybe ActionExecutionStatus) (\s a -> s {status = a} :: ActionExecution)
{-# DEPRECATED aeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last status change of the action.
--
-- /Note:/ Consider using 'lastStatusChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastStatusChange :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Timestamp)
aeLastStatusChange = Lens.lens (lastStatusChange :: ActionExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastStatusChange = a} :: ActionExecution)
{-# DEPRECATED aeLastStatusChange "Use generic-lens or generic-optics with 'lastStatusChange' instead." #-}

-- | The system-generated token used to identify a unique approval request. The token for each open approval request can be obtained using the @GetPipelineState@ command. It is used to validate that the approval request corresponding to this token is still valid.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeToken :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeToken = Lens.lens (token :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: ActionExecution)
{-# DEPRECATED aeToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The URL of a resource external to AWS that is used when running the action (for example, an external repository URL).
--
-- /Note:/ Consider using 'externalExecutionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExternalExecutionURL :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeExternalExecutionURL = Lens.lens (externalExecutionURL :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionURL = a} :: ActionExecution)
{-# DEPRECATED aeExternalExecutionURL "Use generic-lens or generic-optics with 'externalExecutionURL' instead." #-}

-- | The external ID of the run of the action.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExternalExecutionId :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeExternalExecutionId = Lens.lens (externalExecutionId :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionId = a} :: ActionExecution)
{-# DEPRECATED aeExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The details of an error returned by a URL external to AWS.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeErrorDetails :: Lens.Lens' ActionExecution (Lude.Maybe ErrorDetails)
aeErrorDetails = Lens.lens (errorDetails :: ActionExecution -> Lude.Maybe ErrorDetails) (\s a -> s {errorDetails = a} :: ActionExecution)
{-# DEPRECATED aeErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | A percentage of completeness of the action as it runs.
--
-- /Note:/ Consider using 'percentComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aePercentComplete :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Natural)
aePercentComplete = Lens.lens (percentComplete :: ActionExecution -> Lude.Maybe Lude.Natural) (\s a -> s {percentComplete = a} :: ActionExecution)
{-# DEPRECATED aePercentComplete "Use generic-lens or generic-optics with 'percentComplete' instead." #-}

-- | ID of the workflow action execution in the current stage. Use the 'GetPipelineState' action to retrieve the current action execution details of the current stage.
--
-- /Note:/ Consider using 'actionExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeActionExecutionId :: Lens.Lens' ActionExecution (Lude.Maybe Lude.Text)
aeActionExecutionId = Lens.lens (actionExecutionId :: ActionExecution -> Lude.Maybe Lude.Text) (\s a -> s {actionExecutionId = a} :: ActionExecution)
{-# DEPRECATED aeActionExecutionId "Use generic-lens or generic-optics with 'actionExecutionId' instead." #-}

instance Lude.FromJSON ActionExecution where
  parseJSON =
    Lude.withObject
      "ActionExecution"
      ( \x ->
          ActionExecution'
            Lude.<$> (x Lude..:? "lastUpdatedBy")
            Lude.<*> (x Lude..:? "summary")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastStatusChange")
            Lude.<*> (x Lude..:? "token")
            Lude.<*> (x Lude..:? "externalExecutionUrl")
            Lude.<*> (x Lude..:? "externalExecutionId")
            Lude.<*> (x Lude..:? "errorDetails")
            Lude.<*> (x Lude..:? "percentComplete")
            Lude.<*> (x Lude..:? "actionExecutionId")
      )
