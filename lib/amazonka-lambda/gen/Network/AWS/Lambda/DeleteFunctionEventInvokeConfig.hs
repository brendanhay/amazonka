{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration for asynchronous invocation for a function, version, or alias.
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
  ( -- * Creating a request
    DeleteFunctionEventInvokeConfig (..),
    mkDeleteFunctionEventInvokeConfig,

    -- ** Request lenses
    dfeicFunctionName,
    dfeicQualifier,

    -- * Destructuring the response
    DeleteFunctionEventInvokeConfigResponse (..),
    mkDeleteFunctionEventInvokeConfigResponse,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunctionEventInvokeConfig' smart constructor.
data DeleteFunctionEventInvokeConfig = DeleteFunctionEventInvokeConfig'
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
    functionName :: Types.FunctionName,
    -- | A version number or alias name.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionEventInvokeConfig' value with any optional fields omitted.
mkDeleteFunctionEventInvokeConfig ::
  -- | 'functionName'
  Types.FunctionName ->
  DeleteFunctionEventInvokeConfig
mkDeleteFunctionEventInvokeConfig functionName =
  DeleteFunctionEventInvokeConfig'
    { functionName,
      qualifier = Core.Nothing
    }

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
dfeicFunctionName :: Lens.Lens' DeleteFunctionEventInvokeConfig Types.FunctionName
dfeicFunctionName = Lens.field @"functionName"
{-# DEPRECATED dfeicFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A version number or alias name.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeicQualifier :: Lens.Lens' DeleteFunctionEventInvokeConfig (Core.Maybe Types.Qualifier)
dfeicQualifier = Lens.field @"qualifier"
{-# DEPRECATED dfeicQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest DeleteFunctionEventInvokeConfig where
  type
    Rs DeleteFunctionEventInvokeConfig =
      DeleteFunctionEventInvokeConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2019-09-25/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/event-invoke-config")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteFunctionEventInvokeConfigResponse'

-- | /See:/ 'mkDeleteFunctionEventInvokeConfigResponse' smart constructor.
data DeleteFunctionEventInvokeConfigResponse = DeleteFunctionEventInvokeConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionEventInvokeConfigResponse' value with any optional fields omitted.
mkDeleteFunctionEventInvokeConfigResponse ::
  DeleteFunctionEventInvokeConfigResponse
mkDeleteFunctionEventInvokeConfigResponse =
  DeleteFunctionEventInvokeConfigResponse'
