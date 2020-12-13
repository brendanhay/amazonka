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
    apSourceAccount,
    apEventSourceToken,
    apSourceARN,
    apAction,
    apPrincipal,
    apFunctionName,
    apQualifier,
    apStatementId,
    apRevisionId,

    -- * Destructuring the response
    AddPermissionResponse (..),
    mkAddPermissionResponse,

    -- ** Response lenses
    aprsStatement,
    aprsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | For Amazon S3, the ID of the account that owns the resource. Use this together with @SourceArn@ to ensure that the resource is owned by the specified account. It is possible for an Amazon S3 bucket to be deleted by its owner and recreated by another account.
    sourceAccount :: Lude.Maybe Lude.Text,
    -- | For Alexa Smart Home functions, a token that must be supplied by the invoker.
    eventSourceToken :: Lude.Maybe Lude.Text,
    -- | For AWS services, the ARN of the AWS resource that invokes the function. For example, an Amazon S3 bucket or Amazon SNS topic.
    sourceARN :: Lude.Maybe Lude.Text,
    -- | The action that the principal can use on the function. For example, @lambda:InvokeFunction@ or @lambda:GetFunction@ .
    action :: Lude.Text,
    -- | The AWS service or account that invokes the function. If you specify a service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the function through that service.
    principal :: Lude.Text,
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
    functionName :: Lude.Text,
    -- | Specify a version or alias to add permissions to a published version of the function.
    qualifier :: Lude.Maybe Lude.Text,
    -- | A statement identifier that differentiates the statement from others in the same policy.
    statementId :: Lude.Text,
    -- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- * 'sourceAccount' - For Amazon S3, the ID of the account that owns the resource. Use this together with @SourceArn@ to ensure that the resource is owned by the specified account. It is possible for an Amazon S3 bucket to be deleted by its owner and recreated by another account.
-- * 'eventSourceToken' - For Alexa Smart Home functions, a token that must be supplied by the invoker.
-- * 'sourceARN' - For AWS services, the ARN of the AWS resource that invokes the function. For example, an Amazon S3 bucket or Amazon SNS topic.
-- * 'action' - The action that the principal can use on the function. For example, @lambda:InvokeFunction@ or @lambda:GetFunction@ .
-- * 'principal' - The AWS service or account that invokes the function. If you specify a service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the function through that service.
-- * 'functionName' - The name of the Lambda function, version, or alias.
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
-- * 'qualifier' - Specify a version or alias to add permissions to a published version of the function.
-- * 'statementId' - A statement identifier that differentiates the statement from others in the same policy.
-- * 'revisionId' - Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
mkAddPermission ::
  -- | 'action'
  Lude.Text ->
  -- | 'principal'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  -- | 'statementId'
  Lude.Text ->
  AddPermission
mkAddPermission pAction_ pPrincipal_ pFunctionName_ pStatementId_ =
  AddPermission'
    { sourceAccount = Lude.Nothing,
      eventSourceToken = Lude.Nothing,
      sourceARN = Lude.Nothing,
      action = pAction_,
      principal = pPrincipal_,
      functionName = pFunctionName_,
      qualifier = Lude.Nothing,
      statementId = pStatementId_,
      revisionId = Lude.Nothing
    }

-- | For Amazon S3, the ID of the account that owns the resource. Use this together with @SourceArn@ to ensure that the resource is owned by the specified account. It is possible for an Amazon S3 bucket to be deleted by its owner and recreated by another account.
--
-- /Note:/ Consider using 'sourceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSourceAccount :: Lens.Lens' AddPermission (Lude.Maybe Lude.Text)
apSourceAccount = Lens.lens (sourceAccount :: AddPermission -> Lude.Maybe Lude.Text) (\s a -> s {sourceAccount = a} :: AddPermission)
{-# DEPRECATED apSourceAccount "Use generic-lens or generic-optics with 'sourceAccount' instead." #-}

-- | For Alexa Smart Home functions, a token that must be supplied by the invoker.
--
-- /Note:/ Consider using 'eventSourceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apEventSourceToken :: Lens.Lens' AddPermission (Lude.Maybe Lude.Text)
apEventSourceToken = Lens.lens (eventSourceToken :: AddPermission -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceToken = a} :: AddPermission)
{-# DEPRECATED apEventSourceToken "Use generic-lens or generic-optics with 'eventSourceToken' instead." #-}

-- | For AWS services, the ARN of the AWS resource that invokes the function. For example, an Amazon S3 bucket or Amazon SNS topic.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSourceARN :: Lens.Lens' AddPermission (Lude.Maybe Lude.Text)
apSourceARN = Lens.lens (sourceARN :: AddPermission -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: AddPermission)
{-# DEPRECATED apSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | The action that the principal can use on the function. For example, @lambda:InvokeFunction@ or @lambda:GetFunction@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAction :: Lens.Lens' AddPermission Lude.Text
apAction = Lens.lens (action :: AddPermission -> Lude.Text) (\s a -> s {action = a} :: AddPermission)
{-# DEPRECATED apAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The AWS service or account that invokes the function. If you specify a service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the function through that service.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipal :: Lens.Lens' AddPermission Lude.Text
apPrincipal = Lens.lens (principal :: AddPermission -> Lude.Text) (\s a -> s {principal = a} :: AddPermission)
{-# DEPRECATED apPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

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
apFunctionName :: Lens.Lens' AddPermission Lude.Text
apFunctionName = Lens.lens (functionName :: AddPermission -> Lude.Text) (\s a -> s {functionName = a} :: AddPermission)
{-# DEPRECATED apFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Specify a version or alias to add permissions to a published version of the function.
--
-- /Note:/ Consider using 'qualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apQualifier :: Lens.Lens' AddPermission (Lude.Maybe Lude.Text)
apQualifier = Lens.lens (qualifier :: AddPermission -> Lude.Maybe Lude.Text) (\s a -> s {qualifier = a} :: AddPermission)
{-# DEPRECATED apQualifier "Use generic-lens or generic-optics with 'qualifier' instead." #-}

-- | A statement identifier that differentiates the statement from others in the same policy.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apStatementId :: Lens.Lens' AddPermission Lude.Text
apStatementId = Lens.lens (statementId :: AddPermission -> Lude.Text) (\s a -> s {statementId = a} :: AddPermission)
{-# DEPRECATED apStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

-- | Only update the policy if the revision ID matches the ID that's specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apRevisionId :: Lens.Lens' AddPermission (Lude.Maybe Lude.Text)
apRevisionId = Lens.lens (revisionId :: AddPermission -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: AddPermission)
{-# DEPRECATED apRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Req.postJSON lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddPermissionResponse'
            Lude.<$> (x Lude..?> "Statement") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddPermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AddPermission where
  toJSON AddPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceAccount" Lude..=) Lude.<$> sourceAccount,
            ("EventSourceToken" Lude..=) Lude.<$> eventSourceToken,
            ("SourceArn" Lude..=) Lude.<$> sourceARN,
            Lude.Just ("Action" Lude..= action),
            Lude.Just ("Principal" Lude..= principal),
            Lude.Just ("StatementId" Lude..= statementId),
            ("RevisionId" Lude..=) Lude.<$> revisionId
          ]
      )

instance Lude.ToPath AddPermission where
  toPath AddPermission' {..} =
    Lude.mconcat
      ["/2015-03-31/functions/", Lude.toBS functionName, "/policy"]

instance Lude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Lude.mconcat ["Qualifier" Lude.=: qualifier]

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  { -- | The permission statement that's added to the function policy.
    statement :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
--
-- * 'statement' - The permission statement that's added to the function policy.
-- * 'responseStatus' - The response status code.
mkAddPermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddPermissionResponse
mkAddPermissionResponse pResponseStatus_ =
  AddPermissionResponse'
    { statement = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The permission statement that's added to the function policy.
--
-- /Note:/ Consider using 'statement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsStatement :: Lens.Lens' AddPermissionResponse (Lude.Maybe Lude.Text)
aprsStatement = Lens.lens (statement :: AddPermissionResponse -> Lude.Maybe Lude.Text) (\s a -> s {statement = a} :: AddPermissionResponse)
{-# DEPRECATED aprsStatement "Use generic-lens or generic-optics with 'statement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aprsResponseStatus :: Lens.Lens' AddPermissionResponse Lude.Int
aprsResponseStatus = Lens.lens (responseStatus :: AddPermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddPermissionResponse)
{-# DEPRECATED aprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
