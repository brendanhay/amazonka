{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes function-use permission from an AWS service or another account. You can get the ID of the statement from the output of 'GetPolicy' .
module Network.AWS.Lambda.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpFunctionName,
    rpStatementId,
    rpQualifier,
    rpRevisionId,

    -- * Destructuring the response
    RemovePermissionResponse (..),
    mkRemovePermissionResponse,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
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
    -- | Statement ID of the permission to remove.
    statementId :: Types.NamespacedStatementId,
    -- | Specify a version or alias to remove permissions from a published version of the function.
    qualifier :: Core.Maybe Types.Qualifier,
    -- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Core.Maybe Types.RevisionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermission' value with any optional fields omitted.
mkRemovePermission ::
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'statementId'
  Types.NamespacedStatementId ->
  RemovePermission
mkRemovePermission functionName statementId =
  RemovePermission'
    { functionName,
      statementId,
      qualifier = Core.Nothing,
      revisionId = Core.Nothing
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
rpFunctionName :: Lens.Lens' RemovePermission Types.FunctionName
rpFunctionName = Lens.field @"functionName"
{-# DEPRECATED rpFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Statement ID of the permission to remove.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatementId :: Lens.Lens' RemovePermission Types.NamespacedStatementId
rpStatementId = Lens.field @"statementId"
{-# DEPRECATED rpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Specify a version or alias to remove permissions from a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpQualifier :: Lens.Lens' RemovePermission (Core.Maybe Types.Qualifier)
rpQualifier = Lens.field @"qualifier"
{-# DEPRECATED rpQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRevisionId :: Lens.Lens' RemovePermission (Core.Maybe Types.RevisionId)
rpRevisionId = Lens.field @"revisionId"
{-# DEPRECATED rpRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Core.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/policy/")
                Core.<> (Core.toText statementId)
            ),
        Core._rqQuery =
          Core.toQueryValue "Qualifier" Core.<$> qualifier
            Core.<> (Core.toQueryValue "RevisionId" Core.<$> revisionId),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response = Response.receiveNull RemovePermissionResponse'

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemovePermissionResponse' value with any optional fields omitted.
mkRemovePermissionResponse ::
  RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'
