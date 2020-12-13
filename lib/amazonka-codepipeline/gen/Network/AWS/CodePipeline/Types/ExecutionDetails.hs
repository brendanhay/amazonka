{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutionDetails
  ( ExecutionDetails (..),

    -- * Smart constructor
    mkExecutionDetails,

    -- * Lenses
    edSummary,
    edExternalExecutionId,
    edPercentComplete,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.
--
-- /See:/ 'mkExecutionDetails' smart constructor.
data ExecutionDetails = ExecutionDetails'
  { -- | The summary of the current status of the actions.
    summary :: Lude.Maybe Lude.Text,
    -- | The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
    externalExecutionId :: Lude.Maybe Lude.Text,
    -- | The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
    percentComplete :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionDetails' with the minimum fields required to make a request.
--
-- * 'summary' - The summary of the current status of the actions.
-- * 'externalExecutionId' - The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
-- * 'percentComplete' - The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
mkExecutionDetails ::
  ExecutionDetails
mkExecutionDetails =
  ExecutionDetails'
    { summary = Lude.Nothing,
      externalExecutionId = Lude.Nothing,
      percentComplete = Lude.Nothing
    }

-- | The summary of the current status of the actions.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSummary :: Lens.Lens' ExecutionDetails (Lude.Maybe Lude.Text)
edSummary = Lens.lens (summary :: ExecutionDetails -> Lude.Maybe Lude.Text) (\s a -> s {summary = a} :: ExecutionDetails)
{-# DEPRECATED edSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExternalExecutionId :: Lens.Lens' ExecutionDetails (Lude.Maybe Lude.Text)
edExternalExecutionId = Lens.lens (externalExecutionId :: ExecutionDetails -> Lude.Maybe Lude.Text) (\s a -> s {externalExecutionId = a} :: ExecutionDetails)
{-# DEPRECATED edExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
--
-- /Note:/ Consider using 'percentComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPercentComplete :: Lens.Lens' ExecutionDetails (Lude.Maybe Lude.Natural)
edPercentComplete = Lens.lens (percentComplete :: ExecutionDetails -> Lude.Maybe Lude.Natural) (\s a -> s {percentComplete = a} :: ExecutionDetails)
{-# DEPRECATED edPercentComplete "Use generic-lens or generic-optics with 'percentComplete' instead." #-}

instance Lude.ToJSON ExecutionDetails where
  toJSON ExecutionDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("summary" Lude..=) Lude.<$> summary,
            ("externalExecutionId" Lude..=) Lude.<$> externalExecutionId,
            ("percentComplete" Lude..=) Lude.<$> percentComplete
          ]
      )
