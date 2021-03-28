{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the provisioned concurrency configuration for a function's alias or version.
module Network.AWS.Lambda.GetProvisionedConcurrencyConfig
    (
    -- * Creating a request
      GetProvisionedConcurrencyConfig (..)
    , mkGetProvisionedConcurrencyConfig
    -- ** Request lenses
    , gpccFunctionName
    , gpccQualifier

    -- * Destructuring the response
    , GetProvisionedConcurrencyConfigResponse (..)
    , mkGetProvisionedConcurrencyConfigResponse
    -- ** Response lenses
    , gpccrrsAllocatedProvisionedConcurrentExecutions
    , gpccrrsAvailableProvisionedConcurrentExecutions
    , gpccrrsLastModified
    , gpccrrsRequestedProvisionedConcurrentExecutions
    , gpccrrsStatus
    , gpccrrsStatusReason
    , gpccrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
  { functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
  , qualifier :: Types.Qualifier
    -- ^ The version number or alias name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedConcurrencyConfig' value with any optional fields omitted.
mkGetProvisionedConcurrencyConfig
    :: Types.FunctionName -- ^ 'functionName'
    -> Types.Qualifier -- ^ 'qualifier'
    -> GetProvisionedConcurrencyConfig
mkGetProvisionedConcurrencyConfig functionName qualifier
  = GetProvisionedConcurrencyConfig'{functionName, qualifier}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @my-function@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccFunctionName :: Lens.Lens' GetProvisionedConcurrencyConfig Types.FunctionName
gpccFunctionName = Lens.field @"functionName"
{-# INLINEABLE gpccFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccQualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Types.Qualifier
gpccQualifier = Lens.field @"qualifier"
{-# INLINEABLE gpccQualifier #-}
{-# DEPRECATED qualifier "Use generic-lens or generic-optics with 'qualifier' instead"  #-}

instance Core.ToQuery GetProvisionedConcurrencyConfig where
        toQuery GetProvisionedConcurrencyConfig{..}
          = Core.toQueryPair "Qualifier" qualifier

instance Core.ToHeaders GetProvisionedConcurrencyConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetProvisionedConcurrencyConfig where
        type Rs GetProvisionedConcurrencyConfig =
             GetProvisionedConcurrencyConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2019-09-30/functions/" Core.<> Core.toText functionName Core.<>
                             "/provisioned-concurrency",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetProvisionedConcurrencyConfigResponse' Core.<$>
                   (x Core..:? "AllocatedProvisionedConcurrentExecutions") Core.<*>
                     x Core..:? "AvailableProvisionedConcurrentExecutions"
                     Core.<*> x Core..:? "LastModified"
                     Core.<*> x Core..:? "RequestedProvisionedConcurrentExecutions"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "StatusReason"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency allocated.
  , availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency available.
  , lastModified :: Core.Maybe Types.LastModified
    -- ^ The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
  , requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural
    -- ^ The amount of provisioned concurrency requested.
  , status :: Core.Maybe Types.ProvisionedConcurrencyStatusEnum
    -- ^ The status of the allocation process.
  , statusReason :: Core.Maybe Core.Text
    -- ^ For failed allocations, the reason that provisioned concurrency could not be allocated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedConcurrencyConfigResponse' value with any optional fields omitted.
mkGetProvisionedConcurrencyConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetProvisionedConcurrencyConfigResponse
mkGetProvisionedConcurrencyConfigResponse responseStatus
  = GetProvisionedConcurrencyConfigResponse'{allocatedProvisionedConcurrentExecutions
                                               = Core.Nothing,
                                             availableProvisionedConcurrentExecutions =
                                               Core.Nothing,
                                             lastModified = Core.Nothing,
                                             requestedProvisionedConcurrentExecutions =
                                               Core.Nothing,
                                             status = Core.Nothing, statusReason = Core.Nothing,
                                             responseStatus}

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsAllocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsAllocatedProvisionedConcurrentExecutions = Lens.field @"allocatedProvisionedConcurrentExecutions"
{-# INLINEABLE gpccrrsAllocatedProvisionedConcurrentExecutions #-}
{-# DEPRECATED allocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead"  #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsAvailableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsAvailableProvisionedConcurrentExecutions = Lens.field @"availableProvisionedConcurrentExecutions"
{-# INLINEABLE gpccrrsAvailableProvisionedConcurrentExecutions #-}
{-# DEPRECATED availableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead"  #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsLastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Types.LastModified)
gpccrrsLastModified = Lens.field @"lastModified"
{-# INLINEABLE gpccrrsLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsRequestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsRequestedProvisionedConcurrentExecutions = Lens.field @"requestedProvisionedConcurrentExecutions"
{-# INLINEABLE gpccrrsRequestedProvisionedConcurrentExecutions #-}
{-# DEPRECATED requestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead"  #-}

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Types.ProvisionedConcurrencyStatusEnum)
gpccrrsStatus = Lens.field @"status"
{-# INLINEABLE gpccrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsStatusReason :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Text)
gpccrrsStatusReason = Lens.field @"statusReason"
{-# INLINEABLE gpccrrsStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsResponseStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse Core.Int
gpccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
