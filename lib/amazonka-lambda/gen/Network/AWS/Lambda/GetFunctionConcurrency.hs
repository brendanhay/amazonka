{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the reserved concurrency configuration for a function. To set a concurrency limit for a function, use 'PutFunctionConcurrency' .
module Network.AWS.Lambda.GetFunctionConcurrency
  ( -- * Creating a request
    GetFunctionConcurrency (..),
    mkGetFunctionConcurrency,

    -- ** Request lenses
    gFunctionName,

    -- * Destructuring the response
    GetFunctionConcurrencyResponse (..),
    mkGetFunctionConcurrencyResponse,

    -- ** Response lenses
    gfcrrsReservedConcurrentExecutions,
    gfcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunctionConcurrency' smart constructor.
newtype GetFunctionConcurrency = GetFunctionConcurrency'
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
    functionName :: Types.FunctionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionConcurrency' value with any optional fields omitted.
mkGetFunctionConcurrency ::
  -- | 'functionName'
  Types.FunctionName ->
  GetFunctionConcurrency
mkGetFunctionConcurrency functionName =
  GetFunctionConcurrency' {functionName}

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
gFunctionName :: Lens.Lens' GetFunctionConcurrency Types.FunctionName
gFunctionName = Lens.field @"functionName"
{-# DEPRECATED gFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Core.AWSRequest GetFunctionConcurrency where
  type Rs GetFunctionConcurrency = GetFunctionConcurrencyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-30/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/concurrency")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionConcurrencyResponse'
            Core.<$> (x Core..:? "ReservedConcurrentExecutions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFunctionConcurrencyResponse' smart constructor.
data GetFunctionConcurrencyResponse = GetFunctionConcurrencyResponse'
  { -- | The number of simultaneous executions that are reserved for the function.
    reservedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionConcurrencyResponse' value with any optional fields omitted.
mkGetFunctionConcurrencyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFunctionConcurrencyResponse
mkGetFunctionConcurrencyResponse responseStatus =
  GetFunctionConcurrencyResponse'
    { reservedConcurrentExecutions =
        Core.Nothing,
      responseStatus
    }

-- | The number of simultaneous executions that are reserved for the function.
--
-- /Note:/ Consider using 'reservedConcurrentExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcrrsReservedConcurrentExecutions :: Lens.Lens' GetFunctionConcurrencyResponse (Core.Maybe Core.Natural)
gfcrrsReservedConcurrentExecutions = Lens.field @"reservedConcurrentExecutions"
{-# DEPRECATED gfcrrsReservedConcurrentExecutions "Use generic-lens or generic-optics with 'reservedConcurrentExecutions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfcrrsResponseStatus :: Lens.Lens' GetFunctionConcurrencyResponse Core.Int
gfcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
