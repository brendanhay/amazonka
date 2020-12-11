-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfos
  ( WorkflowExecutionInfos (..),

    -- * Smart constructor
    mkWorkflowExecutionInfos,

    -- * Lenses
    weiNextPageToken,
    weiExecutionInfos,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecutionInfo

-- | Contains a paginated list of information about workflow executions.
--
-- /See:/ 'mkWorkflowExecutionInfos' smart constructor.
data WorkflowExecutionInfos = WorkflowExecutionInfos'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    executionInfos :: [WorkflowExecutionInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionInfos' with the minimum fields required to make a request.
--
-- * 'executionInfos' - The list of workflow information structures.
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
mkWorkflowExecutionInfos ::
  WorkflowExecutionInfos
mkWorkflowExecutionInfos =
  WorkflowExecutionInfos'
    { nextPageToken = Lude.Nothing,
      executionInfos = Lude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiNextPageToken :: Lens.Lens' WorkflowExecutionInfos (Lude.Maybe Lude.Text)
weiNextPageToken = Lens.lens (nextPageToken :: WorkflowExecutionInfos -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: WorkflowExecutionInfos)
{-# DEPRECATED weiNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The list of workflow information structures.
--
-- /Note:/ Consider using 'executionInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiExecutionInfos :: Lens.Lens' WorkflowExecutionInfos [WorkflowExecutionInfo]
weiExecutionInfos = Lens.lens (executionInfos :: WorkflowExecutionInfos -> [WorkflowExecutionInfo]) (\s a -> s {executionInfos = a} :: WorkflowExecutionInfos)
{-# DEPRECATED weiExecutionInfos "Use generic-lens or generic-optics with 'executionInfos' instead." #-}

instance Lude.FromJSON WorkflowExecutionInfos where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionInfos"
      ( \x ->
          WorkflowExecutionInfos'
            Lude.<$> (x Lude..:? "nextPageToken")
            Lude.<*> (x Lude..:? "executionInfos" Lude..!= Lude.mempty)
      )
