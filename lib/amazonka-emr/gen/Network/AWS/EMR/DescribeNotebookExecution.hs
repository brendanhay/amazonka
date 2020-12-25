{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details of a notebook execution.
module Network.AWS.EMR.DescribeNotebookExecution
  ( -- * Creating a request
    DescribeNotebookExecution (..),
    mkDescribeNotebookExecution,

    -- ** Request lenses
    dneNotebookExecutionId,

    -- * Destructuring the response
    DescribeNotebookExecutionResponse (..),
    mkDescribeNotebookExecutionResponse,

    -- ** Response lenses
    dnerrsNotebookExecution,
    dnerrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNotebookExecution' smart constructor.
newtype DescribeNotebookExecution = DescribeNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Types.XmlStringMaxLen256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotebookExecution' value with any optional fields omitted.
mkDescribeNotebookExecution ::
  -- | 'notebookExecutionId'
  Types.XmlStringMaxLen256 ->
  DescribeNotebookExecution
mkDescribeNotebookExecution notebookExecutionId =
  DescribeNotebookExecution' {notebookExecutionId}

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dneNotebookExecutionId :: Lens.Lens' DescribeNotebookExecution Types.XmlStringMaxLen256
dneNotebookExecutionId = Lens.field @"notebookExecutionId"
{-# DEPRECATED dneNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

instance Core.FromJSON DescribeNotebookExecution where
  toJSON DescribeNotebookExecution {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NotebookExecutionId" Core..= notebookExecutionId)]
      )

instance Core.AWSRequest DescribeNotebookExecution where
  type
    Rs DescribeNotebookExecution =
      DescribeNotebookExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.DescribeNotebookExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotebookExecutionResponse'
            Core.<$> (x Core..:? "NotebookExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeNotebookExecutionResponse' smart constructor.
data DescribeNotebookExecutionResponse = DescribeNotebookExecutionResponse'
  { -- | Properties of the notebook execution.
    notebookExecution :: Core.Maybe Types.NotebookExecution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeNotebookExecutionResponse' value with any optional fields omitted.
mkDescribeNotebookExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeNotebookExecutionResponse
mkDescribeNotebookExecutionResponse responseStatus =
  DescribeNotebookExecutionResponse'
    { notebookExecution =
        Core.Nothing,
      responseStatus
    }

-- | Properties of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnerrsNotebookExecution :: Lens.Lens' DescribeNotebookExecutionResponse (Core.Maybe Types.NotebookExecution)
dnerrsNotebookExecution = Lens.field @"notebookExecution"
{-# DEPRECATED dnerrsNotebookExecution "Use generic-lens or generic-optics with 'notebookExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnerrsResponseStatus :: Lens.Lens' DescribeNotebookExecutionResponse Core.Int
dnerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dnerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
