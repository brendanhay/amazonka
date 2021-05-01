{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.AddPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS service or another account permission to use a function.
-- You can apply the policy at the function level, or specify a qualifier
-- to restrict access to a single version or alias. If you use a qualifier,
-- the invoker must use the full Amazon Resource Name (ARN) of that version
-- or alias to invoke the function.
--
-- To grant permission to another account, specify the account ID as the
-- @Principal@. For AWS services, the principal is a domain-style
-- identifier defined by the service, like @s3.amazonaws.com@ or
-- @sns.amazonaws.com@. For AWS services, you can also specify the ARN of
-- the associated resource as the @SourceArn@. If you grant permission to a
-- service principal without specifying the source, other accounts could
-- potentially configure resources in their account to invoke your Lambda
-- function.
--
-- This action adds a statement to a resource-based permissions policy for
-- the function. For more information about function policies, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html Lambda Function Policies>.
module Network.AWS.Lambda.AddPermission
  ( -- * Creating a Request
    AddPermission (..),
    newAddPermission,

    -- * Request Lenses
    addPermission_revisionId,
    addPermission_qualifier,
    addPermission_eventSourceToken,
    addPermission_sourceAccount,
    addPermission_sourceArn,
    addPermission_functionName,
    addPermission_statementId,
    addPermission_action,
    addPermission_principal,

    -- * Destructuring the Response
    AddPermissionResponse (..),
    newAddPermissionResponse,

    -- * Response Lenses
    addPermissionResponse_statement,
    addPermissionResponse_httpStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | Only update the policy if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a policy that has changed
    -- since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Specify a version or alias to add permissions to a published version of
    -- the function.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | For Alexa Smart Home functions, a token that must be supplied by the
    -- invoker.
    eventSourceToken :: Prelude.Maybe Prelude.Text,
    -- | For Amazon S3, the ID of the account that owns the resource. Use this
    -- together with @SourceArn@ to ensure that the resource is owned by the
    -- specified account. It is possible for an Amazon S3 bucket to be deleted
    -- by its owner and recreated by another account.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | For AWS services, the ARN of the AWS resource that invokes the function.
    -- For example, an Amazon S3 bucket or Amazon SNS topic.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text,
    -- | A statement identifier that differentiates the statement from others in
    -- the same policy.
    statementId :: Prelude.Text,
    -- | The action that the principal can use on the function. For example,
    -- @lambda:InvokeFunction@ or @lambda:GetFunction@.
    action :: Prelude.Text,
    -- | The AWS service or account that invokes the function. If you specify a
    -- service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the
    -- function through that service.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'addPermission_revisionId' - Only update the policy if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
--
-- 'qualifier', 'addPermission_qualifier' - Specify a version or alias to add permissions to a published version of
-- the function.
--
-- 'eventSourceToken', 'addPermission_eventSourceToken' - For Alexa Smart Home functions, a token that must be supplied by the
-- invoker.
--
-- 'sourceAccount', 'addPermission_sourceAccount' - For Amazon S3, the ID of the account that owns the resource. Use this
-- together with @SourceArn@ to ensure that the resource is owned by the
-- specified account. It is possible for an Amazon S3 bucket to be deleted
-- by its owner and recreated by another account.
--
-- 'sourceArn', 'addPermission_sourceArn' - For AWS services, the ARN of the AWS resource that invokes the function.
-- For example, an Amazon S3 bucket or Amazon SNS topic.
--
-- 'functionName', 'addPermission_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
--
-- 'statementId', 'addPermission_statementId' - A statement identifier that differentiates the statement from others in
-- the same policy.
--
-- 'action', 'addPermission_action' - The action that the principal can use on the function. For example,
-- @lambda:InvokeFunction@ or @lambda:GetFunction@.
--
-- 'principal', 'addPermission_principal' - The AWS service or account that invokes the function. If you specify a
-- service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the
-- function through that service.
newAddPermission ::
  -- | 'functionName'
  Prelude.Text ->
  -- | 'statementId'
  Prelude.Text ->
  -- | 'action'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  AddPermission
newAddPermission
  pFunctionName_
  pStatementId_
  pAction_
  pPrincipal_ =
    AddPermission'
      { revisionId = Prelude.Nothing,
        qualifier = Prelude.Nothing,
        eventSourceToken = Prelude.Nothing,
        sourceAccount = Prelude.Nothing,
        sourceArn = Prelude.Nothing,
        functionName = pFunctionName_,
        statementId = pStatementId_,
        action = pAction_,
        principal = pPrincipal_
      }

-- | Only update the policy if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
addPermission_revisionId :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_revisionId = Lens.lens (\AddPermission' {revisionId} -> revisionId) (\s@AddPermission' {} a -> s {revisionId = a} :: AddPermission)

-- | Specify a version or alias to add permissions to a published version of
-- the function.
addPermission_qualifier :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_qualifier = Lens.lens (\AddPermission' {qualifier} -> qualifier) (\s@AddPermission' {} a -> s {qualifier = a} :: AddPermission)

-- | For Alexa Smart Home functions, a token that must be supplied by the
-- invoker.
addPermission_eventSourceToken :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_eventSourceToken = Lens.lens (\AddPermission' {eventSourceToken} -> eventSourceToken) (\s@AddPermission' {} a -> s {eventSourceToken = a} :: AddPermission)

-- | For Amazon S3, the ID of the account that owns the resource. Use this
-- together with @SourceArn@ to ensure that the resource is owned by the
-- specified account. It is possible for an Amazon S3 bucket to be deleted
-- by its owner and recreated by another account.
addPermission_sourceAccount :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_sourceAccount = Lens.lens (\AddPermission' {sourceAccount} -> sourceAccount) (\s@AddPermission' {} a -> s {sourceAccount = a} :: AddPermission)

-- | For AWS services, the ARN of the AWS resource that invokes the function.
-- For example, an Amazon S3 bucket or Amazon SNS topic.
addPermission_sourceArn :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_sourceArn = Lens.lens (\AddPermission' {sourceArn} -> sourceArn) (\s@AddPermission' {} a -> s {sourceArn = a} :: AddPermission)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
addPermission_functionName :: Lens.Lens' AddPermission Prelude.Text
addPermission_functionName = Lens.lens (\AddPermission' {functionName} -> functionName) (\s@AddPermission' {} a -> s {functionName = a} :: AddPermission)

-- | A statement identifier that differentiates the statement from others in
-- the same policy.
addPermission_statementId :: Lens.Lens' AddPermission Prelude.Text
addPermission_statementId = Lens.lens (\AddPermission' {statementId} -> statementId) (\s@AddPermission' {} a -> s {statementId = a} :: AddPermission)

-- | The action that the principal can use on the function. For example,
-- @lambda:InvokeFunction@ or @lambda:GetFunction@.
addPermission_action :: Lens.Lens' AddPermission Prelude.Text
addPermission_action = Lens.lens (\AddPermission' {action} -> action) (\s@AddPermission' {} a -> s {action = a} :: AddPermission)

-- | The AWS service or account that invokes the function. If you specify a
-- service, use @SourceArn@ or @SourceAccount@ to limit who can invoke the
-- function through that service.
addPermission_principal :: Lens.Lens' AddPermission Prelude.Text
addPermission_principal = Lens.lens (\AddPermission' {principal} -> principal) (\s@AddPermission' {} a -> s {principal = a} :: AddPermission)

instance Prelude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddPermissionResponse'
            Prelude.<$> (x Prelude..?> "Statement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddPermission

instance Prelude.NFData AddPermission

instance Prelude.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON AddPermission where
  toJSON AddPermission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RevisionId" Prelude..=) Prelude.<$> revisionId,
            ("EventSourceToken" Prelude..=)
              Prelude.<$> eventSourceToken,
            ("SourceAccount" Prelude..=)
              Prelude.<$> sourceAccount,
            ("SourceArn" Prelude..=) Prelude.<$> sourceArn,
            Prelude.Just ("StatementId" Prelude..= statementId),
            Prelude.Just ("Action" Prelude..= action),
            Prelude.Just ("Principal" Prelude..= principal)
          ]
      )

instance Prelude.ToPath AddPermission where
  toPath AddPermission' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Prelude.toBS functionName,
        "/policy"
      ]

instance Prelude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat ["Qualifier" Prelude.=: qualifier]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  { -- | The permission statement that\'s added to the function policy.
    statement :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statement', 'addPermissionResponse_statement' - The permission statement that\'s added to the function policy.
--
-- 'httpStatus', 'addPermissionResponse_httpStatus' - The response's http status code.
newAddPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddPermissionResponse
newAddPermissionResponse pHttpStatus_ =
  AddPermissionResponse'
    { statement = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The permission statement that\'s added to the function policy.
addPermissionResponse_statement :: Lens.Lens' AddPermissionResponse (Prelude.Maybe Prelude.Text)
addPermissionResponse_statement = Lens.lens (\AddPermissionResponse' {statement} -> statement) (\s@AddPermissionResponse' {} a -> s {statement = a} :: AddPermissionResponse)

-- | The response's http status code.
addPermissionResponse_httpStatus :: Lens.Lens' AddPermissionResponse Prelude.Int
addPermissionResponse_httpStatus = Lens.lens (\AddPermissionResponse' {httpStatus} -> httpStatus) (\s@AddPermissionResponse' {} a -> s {httpStatus = a} :: AddPermissionResponse)

instance Prelude.NFData AddPermissionResponse
