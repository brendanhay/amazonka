{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetProvisionedConcurrencyConfig (..),
    mkGetProvisionedConcurrencyConfig,

    -- ** Request lenses
    gpccFunctionName,
    gpccQualifier,

    -- * Destructuring the response
    GetProvisionedConcurrencyConfigResponse (..),
    mkGetProvisionedConcurrencyConfigResponse,

    -- ** Response lenses
    gpccrrsAllocatedProvisionedConcurrentExecutions,
    gpccrrsAvailableProvisionedConcurrentExecutions,
    gpccrrsLastModified,
    gpccrrsRequestedProvisionedConcurrentExecutions,
    gpccrrsStatus,
    gpccrrsStatusReason,
    gpccrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
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
    qualifier :: Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetProvisionedConcurrencyConfig' value with any optional fields omitted.
mkGetProvisionedConcurrencyConfig ::
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'qualifier'
  Types.Qualifier ->
  GetProvisionedConcurrencyConfig
mkGetProvisionedConcurrencyConfig functionName qualifier =
  GetProvisionedConcurrencyConfig' {functionName, qualifier}

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
{-# DEPRECATED gpccFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccQualifier :: Lens.Lens' GetProvisionedConcurrencyConfig Types.Qualifier
gpccQualifier = Lens.field @"qualifier"
{-# DEPRECATED gpccQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest GetProvisionedConcurrencyConfig where
  type
    Rs GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-30/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/provisioned-concurrency")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            Core.<$> (x Core..:? "AllocatedProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "AvailableProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "LastModified")
            Core.<*> (x Core..:? "RequestedProvisionedConcurrentExecutions")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
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

-- | Creates a 'GetProvisionedConcurrencyConfigResponse' value with any optional fields omitted.
mkGetProvisionedConcurrencyConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetProvisionedConcurrencyConfigResponse
mkGetProvisionedConcurrencyConfigResponse responseStatus =
  GetProvisionedConcurrencyConfigResponse'
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
gpccrrsAllocatedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsAllocatedProvisionedConcurrentExecutions = Lens.field @"allocatedProvisionedConcurrentExecutions"
{-# DEPRECATED gpccrrsAllocatedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'allocatedProvisionedConcurrentExecutions' instead." #-}

-- | The amount of provisioned concurrency available.
--
-- /Note:/ Consider using 'availableProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsAvailableProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsAvailableProvisionedConcurrentExecutions = Lens.field @"availableProvisionedConcurrentExecutions"
{-# DEPRECATED gpccrrsAvailableProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'availableProvisionedConcurrentExecutions' instead." #-}

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsLastModified :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Types.LastModified)
gpccrrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED gpccrrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The amount of provisioned concurrency requested.
--
-- /Note:/ Consider using 'requestedProvisionedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsRequestedProvisionedConcurrentExecutions :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Core.Natural)
gpccrrsRequestedProvisionedConcurrentExecutions = Lens.field @"requestedProvisionedConcurrentExecutions"
{-# DEPRECATED gpccrrsRequestedProvisionedConcurrentExecutions "Use generic-lens or generic-optics with 'requestedProvisionedConcurrentExecutions' instead." #-}

-- | The status of the allocation process.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Types.ProvisionedConcurrencyStatusEnum)
gpccrrsStatus = Lens.field @"status"
{-# DEPRECATED gpccrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsStatusReason :: Lens.Lens' GetProvisionedConcurrencyConfigResponse (Core.Maybe Types.StatusReason)
gpccrrsStatusReason = Lens.field @"statusReason"
{-# DEPRECATED gpccrrsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpccrrsResponseStatus :: Lens.Lens' GetProvisionedConcurrencyConfigResponse Core.Int
gpccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
