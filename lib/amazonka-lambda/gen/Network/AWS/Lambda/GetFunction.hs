{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the function or function version, with a link to download the deployment package that's valid for 10 minutes. If you specify a function version, only details that are specific to that version are returned.
module Network.AWS.Lambda.GetFunction
  ( -- * Creating a request
    GetFunction (..),
    mkGetFunction,

    -- ** Request lenses
    gfFunctionName,
    gfQualifier,

    -- * Destructuring the response
    GetFunctionResponse (..),
    mkGetFunctionResponse,

    -- ** Response lenses
    gfrrsCode,
    gfrrsConcurrency,
    gfrrsConfiguration,
    gfrrsTags,
    gfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFunction' smart constructor.
data GetFunction = GetFunction'
  { -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:my-function@ .
    --
    --
    -- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.NamespacedFunctionName,
    -- | Specify a version or alias to get details about a published version of the function.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunction' value with any optional fields omitted.
mkGetFunction ::
  -- | 'functionName'
  Types.NamespacedFunctionName ->
  GetFunction
mkGetFunction functionName =
  GetFunction' {functionName, qualifier = Core.Nothing}

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .
--
--
--     * __Partial ARN__ - @123456789012:function:my-function@ .
--
--
-- You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFunctionName :: Lens.Lens' GetFunction Types.NamespacedFunctionName
gfFunctionName = Lens.field @"functionName"
{-# DEPRECATED gfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to get details about a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfQualifier :: Lens.Lens' GetFunction (Core.Maybe Types.Qualifier)
gfQualifier = Lens.field @"qualifier"
{-# DEPRECATED gfQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest GetFunction where
  type Rs GetFunction = GetFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/2015-03-31/functions/" Core.<> (Core.toText functionName)),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            Core.<$> (x Core..:? "Code")
            Core.<*> (x Core..:? "Concurrency")
            Core.<*> (x Core..:? "Configuration")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { -- | The deployment package of the function or version.
    code :: Core.Maybe Types.FunctionCodeLocation,
    -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
    concurrency :: Core.Maybe Types.Concurrency,
    -- | The configuration of the function or version.
    configuration :: Core.Maybe Types.FunctionConfiguration,
    -- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFunctionResponse' value with any optional fields omitted.
mkGetFunctionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFunctionResponse
mkGetFunctionResponse responseStatus =
  GetFunctionResponse'
    { code = Core.Nothing,
      concurrency = Core.Nothing,
      configuration = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The deployment package of the function or version.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsCode :: Lens.Lens' GetFunctionResponse (Core.Maybe Types.FunctionCodeLocation)
gfrrsCode = Lens.field @"code"
{-# DEPRECATED gfrrsCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html reserved concurrency> .
--
-- /Note:/ Consider using 'concurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsConcurrency :: Lens.Lens' GetFunctionResponse (Core.Maybe Types.Concurrency)
gfrrsConcurrency = Lens.field @"concurrency"
{-# DEPRECATED gfrrsConcurrency "Use generic-lens or generic-optics with 'concurrency' instead." #-}

-- | The configuration of the function or version.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsConfiguration :: Lens.Lens' GetFunctionResponse (Core.Maybe Types.FunctionConfiguration)
gfrrsConfiguration = Lens.field @"configuration"
{-# DEPRECATED gfrrsConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The function's <https://docs.aws.amazon.com/lambda/latest/dg/tagging.html tags> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsTags :: Lens.Lens' GetFunctionResponse (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
gfrrsTags = Lens.field @"tags"
{-# DEPRECATED gfrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFunctionResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
