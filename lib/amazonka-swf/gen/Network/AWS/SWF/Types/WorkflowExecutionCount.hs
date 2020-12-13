{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCount
  ( WorkflowExecutionCount (..),

    -- * Smart constructor
    mkWorkflowExecutionCount,

    -- * Lenses
    wecTruncated,
    wecCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the count of workflow executions returned from 'CountOpenWorkflowExecutions' or 'CountClosedWorkflowExecutions'
--
-- /See:/ 'mkWorkflowExecutionCount' smart constructor.
data WorkflowExecutionCount = WorkflowExecutionCount'
  { -- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
    truncated :: Lude.Maybe Lude.Bool,
    -- | The number of workflow executions.
    count :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionCount' with the minimum fields required to make a request.
--
-- * 'truncated' - If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
-- * 'count' - The number of workflow executions.
mkWorkflowExecutionCount ::
  -- | 'count'
  Lude.Natural ->
  WorkflowExecutionCount
mkWorkflowExecutionCount pCount_ =
  WorkflowExecutionCount'
    { truncated = Lude.Nothing,
      count = pCount_
    }

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTruncated :: Lens.Lens' WorkflowExecutionCount (Lude.Maybe Lude.Bool)
wecTruncated = Lens.lens (truncated :: WorkflowExecutionCount -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: WorkflowExecutionCount)
{-# DEPRECATED wecTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | The number of workflow executions.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecCount :: Lens.Lens' WorkflowExecutionCount Lude.Natural
wecCount = Lens.lens (count :: WorkflowExecutionCount -> Lude.Natural) (\s a -> s {count = a} :: WorkflowExecutionCount)
{-# DEPRECATED wecCount "Use generic-lens or generic-optics with 'count' instead." #-}

instance Lude.FromJSON WorkflowExecutionCount where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionCount"
      ( \x ->
          WorkflowExecutionCount'
            Lude.<$> (x Lude..:? "truncated") Lude.<*> (x Lude..: "count")
      )
