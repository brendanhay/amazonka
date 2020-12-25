{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Network.AWS.SageMaker.DeleteFlowDefinition
  ( -- * Creating a request
    DeleteFlowDefinition (..),
    mkDeleteFlowDefinition,

    -- ** Request lenses
    dfdFlowDefinitionName,

    -- * Destructuring the response
    DeleteFlowDefinitionResponse (..),
    mkDeleteFlowDefinitionResponse,

    -- ** Response lenses
    dfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteFlowDefinition' smart constructor.
newtype DeleteFlowDefinition = DeleteFlowDefinition'
  { -- | The name of the flow definition you are deleting.
    flowDefinitionName :: Types.FlowDefinitionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowDefinition' value with any optional fields omitted.
mkDeleteFlowDefinition ::
  -- | 'flowDefinitionName'
  Types.FlowDefinitionName ->
  DeleteFlowDefinition
mkDeleteFlowDefinition flowDefinitionName =
  DeleteFlowDefinition' {flowDefinitionName}

-- | The name of the flow definition you are deleting.
--
-- /Note:/ Consider using 'flowDefinitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdFlowDefinitionName :: Lens.Lens' DeleteFlowDefinition Types.FlowDefinitionName
dfdFlowDefinitionName = Lens.field @"flowDefinitionName"
{-# DEPRECATED dfdFlowDefinitionName "Use generic-lens or generic-optics with 'flowDefinitionName' instead." #-}

instance Core.FromJSON DeleteFlowDefinition where
  toJSON DeleteFlowDefinition {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("FlowDefinitionName" Core..= flowDefinitionName)]
      )

instance Core.AWSRequest DeleteFlowDefinition where
  type Rs DeleteFlowDefinition = DeleteFlowDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteFlowDefinition")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFlowDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFlowDefinitionResponse' smart constructor.
newtype DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFlowDefinitionResponse' value with any optional fields omitted.
mkDeleteFlowDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFlowDefinitionResponse
mkDeleteFlowDefinitionResponse responseStatus =
  DeleteFlowDefinitionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfdrrsResponseStatus :: Lens.Lens' DeleteFlowDefinitionResponse Core.Int
dfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
