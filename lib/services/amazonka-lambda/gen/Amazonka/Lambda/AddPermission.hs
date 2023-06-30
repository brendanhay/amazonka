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
-- Module      : Amazonka.Lambda.AddPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an Amazon Web Service, Amazon Web Services account, or Amazon Web
-- Services organization permission to use a function. You can apply the
-- policy at the function level, or specify a qualifier to restrict access
-- to a single version or alias. If you use a qualifier, the invoker must
-- use the full Amazon Resource Name (ARN) of that version or alias to
-- invoke the function. Note: Lambda does not support adding policies to
-- version $LATEST.
--
-- To grant permission to another account, specify the account ID as the
-- @Principal@. To grant permission to an organization defined in
-- Organizations, specify the organization ID as the @PrincipalOrgID@. For
-- Amazon Web Services, the principal is a domain-style identifier that the
-- service defines, such as @s3.amazonaws.com@ or @sns.amazonaws.com@. For
-- Amazon Web Services, you can also specify the ARN of the associated
-- resource as the @SourceArn@. If you grant permission to a service
-- principal without specifying the source, other accounts could
-- potentially configure resources in their account to invoke your Lambda
-- function.
--
-- This operation adds a statement to a resource-based permissions policy
-- for the function. For more information about function policies, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/access-control-resource-based.html Using resource-based policies for Lambda>.
module Amazonka.Lambda.AddPermission
  ( -- * Creating a Request
    AddPermission (..),
    newAddPermission,

    -- * Request Lenses
    addPermission_eventSourceToken,
    addPermission_functionUrlAuthType,
    addPermission_principalOrgID,
    addPermission_qualifier,
    addPermission_revisionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | For Alexa Smart Home functions, a token that the invoker must supply.
    eventSourceToken :: Prelude.Maybe Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated IAM users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    functionUrlAuthType :: Prelude.Maybe FunctionUrlAuthType,
    -- | The identifier for your organization in Organizations. Use this to grant
    -- permissions to all the Amazon Web Services accounts under this
    -- organization.
    principalOrgID :: Prelude.Maybe Prelude.Text,
    -- | Specify a version or alias to add permissions to a published version of
    -- the function.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | Update the policy only if the revision ID matches the ID that\'s
    -- specified. Use this option to avoid modifying a policy that has changed
    -- since you last read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | For Amazon Web Service, the ID of the Amazon Web Services account that
    -- owns the resource. Use this together with @SourceArn@ to ensure that the
    -- specified account owns the resource. It is possible for an Amazon S3
    -- bucket to be deleted by its owner and recreated by another account.
    sourceAccount :: Prelude.Maybe Prelude.Text,
    -- | For Amazon Web Services, the ARN of the Amazon Web Services resource
    -- that invokes the function. For example, an Amazon S3 bucket or Amazon
    -- SNS topic.
    --
    -- Note that Lambda configures the comparison using the @StringLike@
    -- operator.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ –
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ – @123456789012:function:my-function@.
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
    -- | The Amazon Web Service or Amazon Web Services account that invokes the
    -- function. If you specify a service, use @SourceArn@ or @SourceAccount@
    -- to limit who can invoke the function through that service.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceToken', 'addPermission_eventSourceToken' - For Alexa Smart Home functions, a token that the invoker must supply.
--
-- 'functionUrlAuthType', 'addPermission_functionUrlAuthType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
--
-- 'principalOrgID', 'addPermission_principalOrgID' - The identifier for your organization in Organizations. Use this to grant
-- permissions to all the Amazon Web Services accounts under this
-- organization.
--
-- 'qualifier', 'addPermission_qualifier' - Specify a version or alias to add permissions to a published version of
-- the function.
--
-- 'revisionId', 'addPermission_revisionId' - Update the policy only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
--
-- 'sourceAccount', 'addPermission_sourceAccount' - For Amazon Web Service, the ID of the Amazon Web Services account that
-- owns the resource. Use this together with @SourceArn@ to ensure that the
-- specified account owns the resource. It is possible for an Amazon S3
-- bucket to be deleted by its owner and recreated by another account.
--
-- 'sourceArn', 'addPermission_sourceArn' - For Amazon Web Services, the ARN of the Amazon Web Services resource
-- that invokes the function. For example, an Amazon S3 bucket or Amazon
-- SNS topic.
--
-- Note that Lambda configures the comparison using the @StringLike@
-- operator.
--
-- 'functionName', 'addPermission_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
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
-- 'principal', 'addPermission_principal' - The Amazon Web Service or Amazon Web Services account that invokes the
-- function. If you specify a service, use @SourceArn@ or @SourceAccount@
-- to limit who can invoke the function through that service.
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
      { eventSourceToken = Prelude.Nothing,
        functionUrlAuthType = Prelude.Nothing,
        principalOrgID = Prelude.Nothing,
        qualifier = Prelude.Nothing,
        revisionId = Prelude.Nothing,
        sourceAccount = Prelude.Nothing,
        sourceArn = Prelude.Nothing,
        functionName = pFunctionName_,
        statementId = pStatementId_,
        action = pAction_,
        principal = pPrincipal_
      }

-- | For Alexa Smart Home functions, a token that the invoker must supply.
addPermission_eventSourceToken :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_eventSourceToken = Lens.lens (\AddPermission' {eventSourceToken} -> eventSourceToken) (\s@AddPermission' {} a -> s {eventSourceToken = a} :: AddPermission)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
addPermission_functionUrlAuthType :: Lens.Lens' AddPermission (Prelude.Maybe FunctionUrlAuthType)
addPermission_functionUrlAuthType = Lens.lens (\AddPermission' {functionUrlAuthType} -> functionUrlAuthType) (\s@AddPermission' {} a -> s {functionUrlAuthType = a} :: AddPermission)

-- | The identifier for your organization in Organizations. Use this to grant
-- permissions to all the Amazon Web Services accounts under this
-- organization.
addPermission_principalOrgID :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_principalOrgID = Lens.lens (\AddPermission' {principalOrgID} -> principalOrgID) (\s@AddPermission' {} a -> s {principalOrgID = a} :: AddPermission)

-- | Specify a version or alias to add permissions to a published version of
-- the function.
addPermission_qualifier :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_qualifier = Lens.lens (\AddPermission' {qualifier} -> qualifier) (\s@AddPermission' {} a -> s {qualifier = a} :: AddPermission)

-- | Update the policy only if the revision ID matches the ID that\'s
-- specified. Use this option to avoid modifying a policy that has changed
-- since you last read it.
addPermission_revisionId :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_revisionId = Lens.lens (\AddPermission' {revisionId} -> revisionId) (\s@AddPermission' {} a -> s {revisionId = a} :: AddPermission)

-- | For Amazon Web Service, the ID of the Amazon Web Services account that
-- owns the resource. Use this together with @SourceArn@ to ensure that the
-- specified account owns the resource. It is possible for an Amazon S3
-- bucket to be deleted by its owner and recreated by another account.
addPermission_sourceAccount :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_sourceAccount = Lens.lens (\AddPermission' {sourceAccount} -> sourceAccount) (\s@AddPermission' {} a -> s {sourceAccount = a} :: AddPermission)

-- | For Amazon Web Services, the ARN of the Amazon Web Services resource
-- that invokes the function. For example, an Amazon S3 bucket or Amazon
-- SNS topic.
--
-- Note that Lambda configures the comparison using the @StringLike@
-- operator.
addPermission_sourceArn :: Lens.Lens' AddPermission (Prelude.Maybe Prelude.Text)
addPermission_sourceArn = Lens.lens (\AddPermission' {sourceArn} -> sourceArn) (\s@AddPermission' {} a -> s {sourceArn = a} :: AddPermission)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ – @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ –
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ – @123456789012:function:my-function@.
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

-- | The Amazon Web Service or Amazon Web Services account that invokes the
-- function. If you specify a service, use @SourceArn@ or @SourceAccount@
-- to limit who can invoke the function through that service.
addPermission_principal :: Lens.Lens' AddPermission Prelude.Text
addPermission_principal = Lens.lens (\AddPermission' {principal} -> principal) (\s@AddPermission' {} a -> s {principal = a} :: AddPermission)

instance Core.AWSRequest AddPermission where
  type
    AWSResponse AddPermission =
      AddPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddPermissionResponse'
            Prelude.<$> (x Data..?> "Statement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddPermission where
  hashWithSalt _salt AddPermission' {..} =
    _salt
      `Prelude.hashWithSalt` eventSourceToken
      `Prelude.hashWithSalt` functionUrlAuthType
      `Prelude.hashWithSalt` principalOrgID
      `Prelude.hashWithSalt` qualifier
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` sourceAccount
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` statementId
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` principal

instance Prelude.NFData AddPermission where
  rnf AddPermission' {..} =
    Prelude.rnf eventSourceToken
      `Prelude.seq` Prelude.rnf functionUrlAuthType
      `Prelude.seq` Prelude.rnf principalOrgID
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf sourceAccount
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf statementId
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf principal

instance Data.ToHeaders AddPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AddPermission where
  toJSON AddPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventSourceToken" Data..=)
              Prelude.<$> eventSourceToken,
            ("FunctionUrlAuthType" Data..=)
              Prelude.<$> functionUrlAuthType,
            ("PrincipalOrgID" Data..=)
              Prelude.<$> principalOrgID,
            ("RevisionId" Data..=) Prelude.<$> revisionId,
            ("SourceAccount" Data..=) Prelude.<$> sourceAccount,
            ("SourceArn" Data..=) Prelude.<$> sourceArn,
            Prelude.Just ("StatementId" Data..= statementId),
            Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Principal" Data..= principal)
          ]
      )

instance Data.ToPath AddPermission where
  toPath AddPermission' {..} =
    Prelude.mconcat
      [ "/2015-03-31/functions/",
        Data.toBS functionName,
        "/policy"
      ]

instance Data.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]

-- | /See:/ 'newAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  { -- | The permission statement that\'s added to the function policy.
    statement :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AddPermissionResponse where
  rnf AddPermissionResponse' {..} =
    Prelude.rnf statement
      `Prelude.seq` Prelude.rnf httpStatus
