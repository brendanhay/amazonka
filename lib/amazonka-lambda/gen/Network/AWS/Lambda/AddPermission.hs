{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS service or another account permission to use a function. You can apply the policy at the function level, or specify a qualifier to restrict access to a single version or alias. If you use a qualifier, the invoker must use the full Amazon Resource Name (ARN) of that version or alias to invoke the function.
--
-- To grant permission to another account, specify the account ID as the @Principal@ . For AWS services, the principal is a domain-style identifier defined by the service, like @s3.amazonaws.com@ or @sns.amazonaws.com@ . For AWS services, you can also specify the ARN of the associated resource as the @SourceArn@ . If you grant permission to a service principal without specifying the source, other accounts could potentially configure resources in their account to invoke your Lambda function.
-- This action adds a statement to a resource-based permissions policy for the function. For more information about function policies, see <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html Lambda Function Policies> .
module Network.AWS.Lambda.AddPermission
  ( -- * Creating a request
    AddPermission (..),
    mkAddPermission,

    -- ** Request lenses
    apFunctionName,
    apStatementId,
    apAction,
    apPrincipal,
    apEventSourceToken,
    apQualifier,
    apRevisionId,
    apSourceAccount,
    apSourceArn,

    -- * Destructuring the response
    AddPermissionResponse (..),
    mkAddPermissionResponse,

    -- ** Response lenses
    aprrsStatement,
    aprrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
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
    -- | A statement identifier that differentiates the statement from others in the same policy.
    statementId :: Types.StatementId,
    -- | The action that the principal can use on the function. For example, @lambda:InvokeFunction@ or @lambda:GetFunction@ .
    action :: Types.Action,
    -- | The AWS service or account that invokes the function. If you specify a service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the function through that service.
    principal :: Types.Principal,
    -- | For Alexa Smart Home functions, a token that must be supplied by the invoker.
    eventSourceToken :: Core.Maybe Types.EventSourceToken,
    -- | Specify a version or alias to add permissions to a published version of the function.
    qualifier :: Core.Maybe Types.Qualifier,
    -- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Core.Maybe Types.String,
    -- | For Amazon S3, the ID of the account that owns the resource. Use this together with @SourceArn@ to ensure that the resource is owned by the specified account. It is possible for an Amazon S3 bucket to be deleted by its owner and recreated by another account.
    sourceAccount :: Core.Maybe Types.SourceOwner,
    -- | For AWS services, the ARN of the AWS resource that invokes the function. For example, an Amazon S3 bucket or Amazon SNS topic.
    sourceArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermission' value with any optional fields omitted.
mkAddPermission ::
  -- | 'functionName'
  Types.FunctionName ->
  -- | 'statementId'
  Types.StatementId ->
  -- | 'action'
  Types.Action ->
  -- | 'principal'
  Types.Principal ->
  AddPermission
mkAddPermission functionName statementId action principal =
  AddPermission'
    { functionName,
      statementId,
      action,
      principal,
      eventSourceToken = Core.Nothing,
      qualifier = Core.Nothing,
      revisionId = Core.Nothing,
      sourceAccount = Core.Nothing,
      sourceArn = Core.Nothing
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
apFunctionName :: Lens.Lens' AddPermission Types.FunctionName
apFunctionName = Lens.field @"functionName"
{-# DEPRECATED apFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | A statement identifier that differentiates the statement from others in the same policy.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apStatementId :: Lens.Lens' AddPermission Types.StatementId
apStatementId = Lens.field @"statementId"
{-# DEPRECATED apStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | The action that the principal can use on the function. For example, @lambda:InvokeFunction@ or @lambda:GetFunction@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAction :: Lens.Lens' AddPermission Types.Action
apAction = Lens.field @"action"
{-# DEPRECATED apAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The AWS service or account that invokes the function. If you specify a service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the function through that service.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipal :: Lens.Lens' AddPermission Types.Principal
apPrincipal = Lens.field @"principal"
{-# DEPRECATED apPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | For Alexa Smart Home functions, a token that must be supplied by the invoker.
--
-- /Note:/ Consider using 'eventSourceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apEventSourceToken :: Lens.Lens' AddPermission (Core.Maybe Types.EventSourceToken)
apEventSourceToken = Lens.field @"eventSourceToken"
{-# DEPRECATED apEventSourceToken "Use generic-lens or generic-optics with 'eventSourceToken' instead." #-}

-- | Specify a version or alias to add permissions to a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apQualifier :: Lens.Lens' AddPermission (Core.Maybe Types.Qualifier)
apQualifier = Lens.field @"qualifier"
{-# DEPRECATED apQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apRevisionId :: Lens.Lens' AddPermission (Core.Maybe Types.String)
apRevisionId = Lens.field @"revisionId"
{-# DEPRECATED apRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | For Amazon S3, the ID of the account that owns the resource. Use this together with @SourceArn@ to ensure that the resource is owned by the specified account. It is possible for an Amazon S3 bucket to be deleted by its owner and recreated by another account.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSourceAccount :: Lens.Lens' AddPermission (Core.Maybe Types.SourceOwner)
apSourceAccount = Lens.field @"sourceAccount"
{-# DEPRECATED apSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

-- | For AWS services, the ARN of the AWS resource that invokes the function. For example, an Amazon S3 bucket or Amazon SNS topic.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSourceArn :: Lens.Lens' AddPermission (Core.Maybe Types.Arn)
apSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED apSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

instance Core.FromJSON AddPermission where
  toJSON AddPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StatementId" Core..= statementId),
            Core.Just ("Action" Core..= action),
            Core.Just ("Principal" Core..= principal),
            ("EventSourceToken" Core..=) Core.<$> eventSourceToken,
            ("RevisionId" Core..=) Core.<$> revisionId,
            ("SourceAccount" Core..=) Core.<$> sourceAccount,
            ("SourceArn" Core..=) Core.<$> sourceArn
          ]
      )

instance Core.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.toQueryValue "Qualifier" Core.<$> qualifier,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddPermissionResponse'
            Core.<$> (x Core..:? "Statement") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  { -- | The permission statement that's added to the function policy.
    statement :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddPermissionResponse' value with any optional fields omitted.
mkAddPermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddPermissionResponse
mkAddPermissionResponse responseStatus =
  AddPermissionResponse' {statement = Core.Nothing, responseStatus}

-- | The permission statement that's added to the function policy.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsStatement :: Lens.Lens' AddPermissionResponse (Core.Maybe Types.String)
aprrsStatement = Lens.field @"statement"
{-# DEPRECATED aprrsStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprrsResponseStatus :: Lens.Lens' AddPermissionResponse Core.Int
aprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
