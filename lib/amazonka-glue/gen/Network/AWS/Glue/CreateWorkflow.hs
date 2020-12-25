{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateWorkflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new workflow.
module Network.AWS.Glue.CreateWorkflow
  ( -- * Creating a request
    CreateWorkflow (..),
    mkCreateWorkflow,

    -- ** Request lenses
    cwName,
    cwDefaultRunProperties,
    cwDescription,
    cwMaxConcurrentRuns,
    cwTags,

    -- * Destructuring the response
    CreateWorkflowResponse (..),
    mkCreateWorkflowResponse,

    -- ** Response lenses
    cwrrsName,
    cwrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { -- | The name to be assigned to the workflow. It should be unique within your account.
    name :: Types.NameString,
    -- | A collection of properties to be used as part of each execution of the workflow.
    defaultRunProperties :: Core.Maybe (Core.HashMap Types.IdString Types.GenericString),
    -- | A description of the workflow.
    description :: Core.Maybe Types.GenericString,
    -- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
    maxConcurrentRuns :: Core.Maybe Core.Int,
    -- | The tags to be used with this workflow.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkflow' value with any optional fields omitted.
mkCreateWorkflow ::
  -- | 'name'
  Types.NameString ->
  CreateWorkflow
mkCreateWorkflow name =
  CreateWorkflow'
    { name,
      defaultRunProperties = Core.Nothing,
      description = Core.Nothing,
      maxConcurrentRuns = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name to be assigned to the workflow. It should be unique within your account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwName :: Lens.Lens' CreateWorkflow Types.NameString
cwName = Lens.field @"name"
{-# DEPRECATED cwName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of properties to be used as part of each execution of the workflow.
--
-- /Note:/ Consider using 'defaultRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwDefaultRunProperties :: Lens.Lens' CreateWorkflow (Core.Maybe (Core.HashMap Types.IdString Types.GenericString))
cwDefaultRunProperties = Lens.field @"defaultRunProperties"
{-# DEPRECATED cwDefaultRunProperties "Use generic-lens or generic-optics with 'defaultRunProperties' instead." #-}

-- | A description of the workflow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwDescription :: Lens.Lens' CreateWorkflow (Core.Maybe Types.GenericString)
cwDescription = Lens.field @"description"
{-# DEPRECATED cwDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwMaxConcurrentRuns :: Lens.Lens' CreateWorkflow (Core.Maybe Core.Int)
cwMaxConcurrentRuns = Lens.field @"maxConcurrentRuns"
{-# DEPRECATED cwMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

-- | The tags to be used with this workflow.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwTags :: Lens.Lens' CreateWorkflow (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cwTags = Lens.field @"tags"
{-# DEPRECATED cwTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateWorkflow where
  toJSON CreateWorkflow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("DefaultRunProperties" Core..=) Core.<$> defaultRunProperties,
            ("Description" Core..=) Core.<$> description,
            ("MaxConcurrentRuns" Core..=) Core.<$> maxConcurrentRuns,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateWorkflow where
  type Rs CreateWorkflow = CreateWorkflowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.CreateWorkflow")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Core.<$> (x Core..:? "Name") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { -- | The name of the workflow which was provided as part of the request.
    name :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkflowResponse' value with any optional fields omitted.
mkCreateWorkflowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateWorkflowResponse
mkCreateWorkflowResponse responseStatus =
  CreateWorkflowResponse' {name = Core.Nothing, responseStatus}

-- | The name of the workflow which was provided as part of the request.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsName :: Lens.Lens' CreateWorkflowResponse (Core.Maybe Types.Name)
cwrrsName = Lens.field @"name"
{-# DEPRECATED cwrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsResponseStatus :: Lens.Lens' CreateWorkflowResponse Core.Int
cwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
