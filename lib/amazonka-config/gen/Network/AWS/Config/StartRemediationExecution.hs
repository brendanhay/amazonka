{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartRemediationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand remediation for the specified AWS Config rules against the last known remediation configuration. It runs an execution against the current state of your resources. Remediation execution is asynchronous.
--
-- You can specify up to 100 resource keys per request. An existing StartRemediationExecution call for the specified resource keys must complete before you can call the API again.
module Network.AWS.Config.StartRemediationExecution
  ( -- * Creating a request
    StartRemediationExecution (..),
    mkStartRemediationExecution,

    -- ** Request lenses
    sreConfigRuleName,
    sreResourceKeys,

    -- * Destructuring the response
    StartRemediationExecutionResponse (..),
    mkStartRemediationExecutionResponse,

    -- ** Response lenses
    srerrsFailedItems,
    srerrsFailureMessage,
    srerrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartRemediationExecution' smart constructor.
data StartRemediationExecution = StartRemediationExecution'
  { -- | The list of names of AWS Config rules that you want to run remediation execution for.
    configRuleName :: Types.ConfigRuleName,
    -- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
    resourceKeys :: Core.NonEmpty Types.ResourceKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartRemediationExecution' value with any optional fields omitted.
mkStartRemediationExecution ::
  -- | 'configRuleName'
  Types.ConfigRuleName ->
  -- | 'resourceKeys'
  Core.NonEmpty Types.ResourceKey ->
  StartRemediationExecution
mkStartRemediationExecution configRuleName resourceKeys =
  StartRemediationExecution' {configRuleName, resourceKeys}

-- | The list of names of AWS Config rules that you want to run remediation execution for.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreConfigRuleName :: Lens.Lens' StartRemediationExecution Types.ConfigRuleName
sreConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED sreConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID.
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreResourceKeys :: Lens.Lens' StartRemediationExecution (Core.NonEmpty Types.ResourceKey)
sreResourceKeys = Lens.field @"resourceKeys"
{-# DEPRECATED sreResourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead." #-}

instance Core.FromJSON StartRemediationExecution where
  toJSON StartRemediationExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("ResourceKeys" Core..= resourceKeys)
          ]
      )

instance Core.AWSRequest StartRemediationExecution where
  type
    Rs StartRemediationExecution =
      StartRemediationExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.StartRemediationExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRemediationExecutionResponse'
            Core.<$> (x Core..:? "FailedItems")
            Core.<*> (x Core..:? "FailureMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartRemediationExecutionResponse' smart constructor.
data StartRemediationExecutionResponse = StartRemediationExecutionResponse'
  { -- | For resources that have failed to start execution, the API returns a resource key object.
    failedItems :: Core.Maybe (Core.NonEmpty Types.ResourceKey),
    -- | Returns a failure message. For example, the resource is already compliant.
    failureMessage :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartRemediationExecutionResponse' value with any optional fields omitted.
mkStartRemediationExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartRemediationExecutionResponse
mkStartRemediationExecutionResponse responseStatus =
  StartRemediationExecutionResponse'
    { failedItems = Core.Nothing,
      failureMessage = Core.Nothing,
      responseStatus
    }

-- | For resources that have failed to start execution, the API returns a resource key object.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsFailedItems :: Lens.Lens' StartRemediationExecutionResponse (Core.Maybe (Core.NonEmpty Types.ResourceKey))
srerrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED srerrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | Returns a failure message. For example, the resource is already compliant.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsFailureMessage :: Lens.Lens' StartRemediationExecutionResponse (Core.Maybe Types.String)
srerrsFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED srerrsFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsResponseStatus :: Lens.Lens' StartRemediationExecutionResponse Core.Int
srerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
