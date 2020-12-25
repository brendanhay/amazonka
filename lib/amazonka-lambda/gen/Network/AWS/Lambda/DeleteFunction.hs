{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function. To delete a specific function version, use the @Qualifier@ parameter. Otherwise, all versions and aliases are deleted.
--
-- To delete Lambda event source mappings that invoke a function, use 'DeleteEventSourceMapping' . For AWS services and resources that invoke your function directly, delete the trigger in the service where you originally configured it.
module Network.AWS.Lambda.DeleteFunction
  ( -- * Creating a request
    DeleteFunction (..),
    mkDeleteFunction,

    -- ** Request lenses
    dfFunctionName,
    dfQualifier,

    -- * Destructuring the response
    DeleteFunctionResponse (..),
    mkDeleteFunctionResponse,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFunction' smart constructor.
data DeleteFunction = DeleteFunction'
  { -- | The name of the Lambda function or version.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).
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
    -- | Specify a version to delete. You can't delete a version that's referenced by an alias.
    qualifier :: Core.Maybe Types.Qualifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunction' value with any optional fields omitted.
mkDeleteFunction ::
  -- | 'functionName'
  Types.FunctionName ->
  DeleteFunction
mkDeleteFunction functionName =
  DeleteFunction' {functionName, qualifier = Core.Nothing}

-- | The name of the Lambda function or version.
--
-- __Name formats__
--
--     * __Function name__ - @my-function@ (name-only), @my-function:1@ (with version).
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
dfFunctionName :: Lens.Lens' DeleteFunction Types.FunctionName
dfFunctionName = Lens.field @"functionName"
{-# DEPRECATED dfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version to delete. You can't delete a version that's referenced by an alias.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfQualifier :: Lens.Lens' DeleteFunction (Core.Maybe Types.Qualifier)
dfQualifier = Lens.field @"qualifier"
{-# DEPRECATED dfQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

instance Core.AWSRequest DeleteFunction where
  type Rs DeleteFunction = DeleteFunctionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2015-03-31/functions/" Core.<> (Core.toText functionName)),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteFunctionResponse'

-- | /See:/ 'mkDeleteFunctionResponse' smart constructor.
data DeleteFunctionResponse = DeleteFunctionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFunctionResponse' value with any optional fields omitted.
mkDeleteFunctionResponse ::
  DeleteFunctionResponse
mkDeleteFunctionResponse = DeleteFunctionResponse'
