{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartRemediationExecution (..)
    , mkStartRemediationExecution
    -- ** Request lenses
    , sreConfigRuleName
    , sreResourceKeys

    -- * Destructuring the response
    , StartRemediationExecutionResponse (..)
    , mkStartRemediationExecutionResponse
    -- ** Response lenses
    , srerrsFailedItems
    , srerrsFailureMessage
    , srerrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartRemediationExecution' smart constructor.
data StartRemediationExecution = StartRemediationExecution'
  { configRuleName :: Types.ConfigRuleName
    -- ^ The list of names of AWS Config rules that you want to run remediation execution for.
  , resourceKeys :: Core.NonEmpty Types.ResourceKey
    -- ^ A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartRemediationExecution' value with any optional fields omitted.
mkStartRemediationExecution
    :: Types.ConfigRuleName -- ^ 'configRuleName'
    -> Core.NonEmpty Types.ResourceKey -- ^ 'resourceKeys'
    -> StartRemediationExecution
mkStartRemediationExecution configRuleName resourceKeys
  = StartRemediationExecution'{configRuleName, resourceKeys}

-- | The list of names of AWS Config rules that you want to run remediation execution for.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreConfigRuleName :: Lens.Lens' StartRemediationExecution Types.ConfigRuleName
sreConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE sreConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

-- | A list of resource keys to be processed with the current request. Each element in the list consists of the resource type and resource ID. 
--
-- /Note:/ Consider using 'resourceKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sreResourceKeys :: Lens.Lens' StartRemediationExecution (Core.NonEmpty Types.ResourceKey)
sreResourceKeys = Lens.field @"resourceKeys"
{-# INLINEABLE sreResourceKeys #-}
{-# DEPRECATED resourceKeys "Use generic-lens or generic-optics with 'resourceKeys' instead"  #-}

instance Core.ToQuery StartRemediationExecution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartRemediationExecution where
        toHeaders StartRemediationExecution{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.StartRemediationExecution")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartRemediationExecution where
        toJSON StartRemediationExecution{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleName" Core..= configRuleName),
                  Core.Just ("ResourceKeys" Core..= resourceKeys)])

instance Core.AWSRequest StartRemediationExecution where
        type Rs StartRemediationExecution =
             StartRemediationExecutionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartRemediationExecutionResponse' Core.<$>
                   (x Core..:? "FailedItems") Core.<*> x Core..:? "FailureMessage"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartRemediationExecutionResponse' smart constructor.
data StartRemediationExecutionResponse = StartRemediationExecutionResponse'
  { failedItems :: Core.Maybe (Core.NonEmpty Types.ResourceKey)
    -- ^ For resources that have failed to start execution, the API returns a resource key object.
  , failureMessage :: Core.Maybe Core.Text
    -- ^ Returns a failure message. For example, the resource is already compliant.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartRemediationExecutionResponse' value with any optional fields omitted.
mkStartRemediationExecutionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartRemediationExecutionResponse
mkStartRemediationExecutionResponse responseStatus
  = StartRemediationExecutionResponse'{failedItems = Core.Nothing,
                                       failureMessage = Core.Nothing, responseStatus}

-- | For resources that have failed to start execution, the API returns a resource key object.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsFailedItems :: Lens.Lens' StartRemediationExecutionResponse (Core.Maybe (Core.NonEmpty Types.ResourceKey))
srerrsFailedItems = Lens.field @"failedItems"
{-# INLINEABLE srerrsFailedItems #-}
{-# DEPRECATED failedItems "Use generic-lens or generic-optics with 'failedItems' instead"  #-}

-- | Returns a failure message. For example, the resource is already compliant.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsFailureMessage :: Lens.Lens' StartRemediationExecutionResponse (Core.Maybe Core.Text)
srerrsFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE srerrsFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srerrsResponseStatus :: Lens.Lens' StartRemediationExecutionResponse Core.Int
srerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
