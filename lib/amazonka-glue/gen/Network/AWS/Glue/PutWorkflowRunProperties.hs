{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutWorkflowRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified workflow run properties for the given workflow run. If a property already exists for the specified run, then it overrides the value otherwise adds the property to existing properties.
module Network.AWS.Glue.PutWorkflowRunProperties
  ( -- * Creating a request
    PutWorkflowRunProperties (..),
    mkPutWorkflowRunProperties,

    -- ** Request lenses
    pwrpName,
    pwrpRunId,
    pwrpRunProperties,

    -- * Destructuring the response
    PutWorkflowRunPropertiesResponse (..),
    mkPutWorkflowRunPropertiesResponse,

    -- ** Response lenses
    pwrprrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutWorkflowRunProperties' smart constructor.
data PutWorkflowRunProperties = PutWorkflowRunProperties'
  { -- | Name of the workflow which was run.
    name :: Types.Name,
    -- | The ID of the workflow run for which the run properties should be updated.
    runId :: Types.RunId,
    -- | The properties to put for the specified run.
    runProperties :: Core.HashMap Types.IdString Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutWorkflowRunProperties' value with any optional fields omitted.
mkPutWorkflowRunProperties ::
  -- | 'name'
  Types.Name ->
  -- | 'runId'
  Types.RunId ->
  PutWorkflowRunProperties
mkPutWorkflowRunProperties name runId =
  PutWorkflowRunProperties'
    { name,
      runId,
      runProperties = Core.mempty
    }

-- | Name of the workflow which was run.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpName :: Lens.Lens' PutWorkflowRunProperties Types.Name
pwrpName = Lens.field @"name"
{-# DEPRECATED pwrpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the workflow run for which the run properties should be updated.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunId :: Lens.Lens' PutWorkflowRunProperties Types.RunId
pwrpRunId = Lens.field @"runId"
{-# DEPRECATED pwrpRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The properties to put for the specified run.
--
-- /Note:/ Consider using 'runProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrpRunProperties :: Lens.Lens' PutWorkflowRunProperties (Core.HashMap Types.IdString Types.GenericString)
pwrpRunProperties = Lens.field @"runProperties"
{-# DEPRECATED pwrpRunProperties "Use generic-lens or generic-optics with 'runProperties' instead." #-}

instance Core.FromJSON PutWorkflowRunProperties where
  toJSON PutWorkflowRunProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("RunId" Core..= runId),
            Core.Just ("RunProperties" Core..= runProperties)
          ]
      )

instance Core.AWSRequest PutWorkflowRunProperties where
  type Rs PutWorkflowRunProperties = PutWorkflowRunPropertiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.PutWorkflowRunProperties")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutWorkflowRunPropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutWorkflowRunPropertiesResponse' smart constructor.
newtype PutWorkflowRunPropertiesResponse = PutWorkflowRunPropertiesResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutWorkflowRunPropertiesResponse' value with any optional fields omitted.
mkPutWorkflowRunPropertiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutWorkflowRunPropertiesResponse
mkPutWorkflowRunPropertiesResponse responseStatus =
  PutWorkflowRunPropertiesResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwrprrsResponseStatus :: Lens.Lens' PutWorkflowRunPropertiesResponse Core.Int
pwrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pwrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
