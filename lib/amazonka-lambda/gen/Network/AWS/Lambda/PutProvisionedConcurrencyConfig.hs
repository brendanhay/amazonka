{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PutProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a provisioned concurrency configuration to a function's alias or version.
module Network.AWS.Lambda.PutProvisionedConcurrencyConfig
  ( -- * Creating a request
    PutProvisionedConcurrencyConfig (..),
    mkPutProvisionedConcurrencyConfig,

    -- ** Request lenses
    ppccFunctionName,
    ppccQualifier,
    ppccProvisionedConcurrentExecutions,

    -- * Destructuring the response
    PutProvisionedConcurrencyConfigResponse (..),
    mkPutProvisionedConcurrencyConfigResponse,

    -- ** Response lenses
    ppccrrsAllocatedProvisionedConcurrentExecutions,
    ppccrrsAvailableProvisionedConcurrentExecutions,
    ppccrrsLastModified,
    ppccrrsRequestedProvisionedConcurrentExecutions,
    ppccrrsStatus,
    ppccrrsStatusReason,
    ppccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutProvisionedConcurrencyConfig' smart constructor.
data PutProvisionedConcurrencyConfig = PutProvisionedConcurrencyConfig'
  { -- | The name of the Lambda function.
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
    functionName :: Types.FunctionName,
    -- | The version number or alias name.
    qualifier :: Types.Qualifier,
    -- | The amount of provisioned concurrency to allocate for the version or alias.
    provisionedConcurrentExecutions :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutProvisionedConcurrencyConfig' value with any optional fields omitted.
mkPutProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'qualifier'
  Types.Qualifier ->
  -- | 'provisionedConcurrentExecutions'
  Core.Natural ->
  PutProvisionedConcurrencyConfig
mkPutProvisionedConcurrencyConfig
  functionName
  qualifier
  provisionedConcurrentExecutions =
    PutProvisionedConcurrencyConfig'
      { functionName,
        qualifier,
        provisionedConcurrentExecutions
      }

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
ppccFunctionName :: Lens.Lens' PutProvisionedConcurrencyConfig Types.FunctionName
ppccFunctionName = Lens.field @"functionName"
{-# DEPRECATED ppccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccQualifier :: Lens.Lens' PutProvisionedConcurrencyConfig Types.Qualifier
ppccQualifier = Lens.field @"qualifier"
{-# DEPRECATED ppccQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | The amount of provisioned concurrency to allocate for the version or alias.
--
-- /Note:/ Consider using 'provisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfig Core.Natural
ppccProvisionedConcurrentExecutions = Lens.field @"provisionedConcurrentExecutions"
{-# DEPRECATED ppccProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'provisionedConcurrentExecutions' instead." #-}

instance Core.FromJSON PutProvisionedConcurrencyConfig where
  toJSON PutProvisionedConcurrencyConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ProvisionedConcurrentExecutions"
                  Core..= provisionedConcurrentExecutions
              )
          ]
      )

instance Core.AWSRequest PutProvisionedConcurrencyConfig where
  type
    Rs PutProvisionedConcurrencyConfig =
      PutProvisionedConcurrencyConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-30/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/provisioned-concurrency")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProvisionedConcurrencyConfigResponse'
            Core.<$> (x Core..:? "AllocatedProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "AvailableProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "LastModified")
            Core.<*> (x Core..:? "RequestedProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutProvisionedConcurrencyConfigResponse' smart constructor.
data PutProvisionedConcurrencyConfigResponse = PutProvisionedConcurrencyConfigResponse'
  { -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
    lastModified :: Core.Maybe Types.LastModified,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The status of the allocation process.
    status :: Core.Maybe Types.ProvisionedConcurrencyStatusEnum,
    -- | For failed allocations, the reason that provisioned concurrency could not be allocated.
    statusReason :: Core.Maybe Types.StatusReason,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutProvisionedConcurrencyConfigResponse' value with any optional fields omitted.
mkPutProvisionedConcurrencyConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutProvisionedConcurrencyConfigResponse
mkPutProvisionedConcurrencyConfigResponse responseStatus =
  PutProvisionedConcurrencyConfigResponse'
    { allocatedProvisionedConcurrentExecutions =
        Core.Nothing,
      availableProvisionedConcurrentExecutions =
        Core.Nothing,
      lastModified = Core.Nothing,
      requestedProvisionedConcurrentExecutions =
        Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing,
      responseStatus
    }

-- | The amount of provisioned concurrency allocated.
--
-- /Note:/ Consider using 'allocatedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsAllocatedProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
ppccrrsAllocatedProvisionedConcurrentExecutions = Lens.field @"allocatedProvisionedConcurrentExecutions"
{-# DEPRECATED ppccrrsAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsAvailableProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
ppccrrsAvailableProvisionedConcurrentExecutions = Lens.field @"availableProvisionedConcurrentExecutions"
{-# DEPRECATED ppccrrsAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsLastModified :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Types.LastModified)
ppccrrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED ppccrrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsRequestedProvisionedConcurrentExecutions :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
ppccrrsRequestedProvisionedConcurrentExecutions = Lens.field @"requestedProvisionedConcurrentExecutions"
{-# DEPRECATED ppccrrsRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsStatus :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Types.ProvisionedConcurrencyStatusEnum)
ppccrrsStatus = Lens.field @"status"
{-# DEPRECATED ppccrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsStatusReason :: Lens.Lens' PutProvisionedConcurrencyConfigResponse (Core.Maybe Types.StatusReason)
ppccrrsStatusReason = Lens.field @"statusReason"
{-# DEPRECATED ppccrrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppccrrsResponseStatus :: Lens.Lens' PutProvisionedConcurrencyConfigResponse Core.Int
ppccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ppccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
