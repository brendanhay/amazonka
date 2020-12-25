{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the workflow run properties which were set during the run.
module Network.AWS.Glue.GetWorkflowRunProperties
  ( -- * Creating a request
    GetWorkflowRunProperties (..),
    mkGetWorkflowRunProperties,

    -- ** Request lenses
    gwrpName,
    gwrpRunId,

    -- * Destructuring the response
    GetWorkflowRunPropertiesResponse (..),
    mkGetWorkflowRunPropertiesResponse,

    -- ** Response lenses
    gwrprrsRunProperties,
    gwrprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetWorkflowRunProperties' smart constructor.
data GetWorkflowRunProperties = GetWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Types.Name,
    -- | The ID of the workflow run whose run properties should be returned.
    runId :: Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowRunProperties' value with any optional fields omitted.
mkGetWorkflowRunProperties ::
  -- | 'name'
  Types.Name ->
  -- | 'runId'
  Types.RunId ->
  GetWorkflowRunProperties
mkGetWorkflowRunProperties name runId =
  GetWorkflowRunProperties' {name, runId}

-- | Name of the workflow which was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrpName :: Lens.Lens' GetWorkflowRunProperties Types.Name
gwrpName = Lens.field @"name"
{-# DEPRECATED gwrpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run whose run properties should be returned.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrpRunId :: Lens.Lens' GetWorkflowRunProperties Types.RunId
gwrpRunId = Lens.field @"runId"
{-# DEPRECATED gwrpRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON GetWorkflowRunProperties where
  toJSON GetWorkflowRunProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId)
          ]
      )

instance Core.AWSRequest GetWorkflowRunProperties where
  type Rs GetWorkflowRunProperties = GetWorkflowRunPropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetWorkflowRunProperties")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunPropertiesResponse'
            Core.<$> (x Core..:? "RunProperties")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetWorkflowRunPropertiesResponse' smart constructor.
data GetWorkflowRunPropertiesResponse = GetWorkflowRunPropertiesResponse'
  { -- | The workflow run properties which were set during the specified run.
    runProperties :: Core.Maybe (Core.HashMap Types.IdString Types.GenericString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetWorkflowRunPropertiesResponse' value with any optional fields omitted.
mkGetWorkflowRunPropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetWorkflowRunPropertiesResponse
mkGetWorkflowRunPropertiesResponse responseStatus =
  GetWorkflowRunPropertiesResponse'
    { runProperties = Core.Nothing,
      responseStatus
    }

-- | The workflow run properties which were set during the specified run.
--
-- /Note:/ Consider using 'runProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrprrsRunProperties :: Lens.Lens' GetWorkflowRunPropertiesResponse (Core.Maybe (Core.HashMap Types.IdString Types.GenericString))
gwrprrsRunProperties = Lens.field @"runProperties"
{-# DEPRECATED gwrprrsRunProperties "Use generic-lens or generic-optics with 'runProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwrprrsResponseStatus :: Lens.Lens' GetWorkflowRunPropertiesResponse Core.Int
gwrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gwrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
