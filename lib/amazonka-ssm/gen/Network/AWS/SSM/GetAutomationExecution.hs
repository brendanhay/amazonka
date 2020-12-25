{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetAutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed information about a particular Automation execution.
module Network.AWS.SSM.GetAutomationExecution
  ( -- * Creating a request
    GetAutomationExecution (..),
    mkGetAutomationExecution,

    -- ** Request lenses
    gaeAutomationExecutionId,

    -- * Destructuring the response
    GetAutomationExecutionResponse (..),
    mkGetAutomationExecutionResponse,

    -- ** Response lenses
    gaerrsAutomationExecution,
    gaerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetAutomationExecution' smart constructor.
newtype GetAutomationExecution = GetAutomationExecution'
  { -- | The unique identifier for an existing automation execution to examine. The execution ID is returned by StartAutomationExecution when the execution of an Automation document is initiated.
    automationExecutionId :: Types.AutomationExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAutomationExecution' value with any optional fields omitted.
mkGetAutomationExecution ::
  -- | 'automationExecutionId'
  Types.AutomationExecutionId ->
  GetAutomationExecution
mkGetAutomationExecution automationExecutionId =
  GetAutomationExecution' {automationExecutionId}

-- | The unique identifier for an existing automation execution to examine. The execution ID is returned by StartAutomationExecution when the execution of an Automation document is initiated.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaeAutomationExecutionId :: Lens.Lens' GetAutomationExecution Types.AutomationExecutionId
gaeAutomationExecutionId = Lens.field @"automationExecutionId"
{-# DEPRECATED gaeAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

instance Core.FromJSON GetAutomationExecution where
  toJSON GetAutomationExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AutomationExecutionId" Core..= automationExecutionId)
          ]
      )

instance Core.AWSRequest GetAutomationExecution where
  type Rs GetAutomationExecution = GetAutomationExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetAutomationExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutomationExecutionResponse'
            Core.<$> (x Core..:? "AutomationExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAutomationExecutionResponse' smart constructor.
data GetAutomationExecutionResponse = GetAutomationExecutionResponse'
  { -- | Detailed information about the current state of an automation execution.
    automationExecution :: Core.Maybe Types.AutomationExecution,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetAutomationExecutionResponse' value with any optional fields omitted.
mkGetAutomationExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAutomationExecutionResponse
mkGetAutomationExecutionResponse responseStatus =
  GetAutomationExecutionResponse'
    { automationExecution =
        Core.Nothing,
      responseStatus
    }

-- | Detailed information about the current state of an automation execution.
--
-- /Note:/ Consider using 'automationExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaerrsAutomationExecution :: Lens.Lens' GetAutomationExecutionResponse (Core.Maybe Types.AutomationExecution)
gaerrsAutomationExecution = Lens.field @"automationExecution"
{-# DEPRECATED gaerrsAutomationExecution "Use generic-lens or generic-optics with 'automationExecution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaerrsResponseStatus :: Lens.Lens' GetAutomationExecutionResponse Core.Int
gaerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gaerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
