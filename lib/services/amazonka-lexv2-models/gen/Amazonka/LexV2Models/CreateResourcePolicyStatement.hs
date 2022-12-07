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
-- Module      : Amazonka.LexV2Models.CreateResourcePolicyStatement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new resource policy statement to a bot or bot alias. If a
-- resource policy exists, the statement is added to the current resource
-- policy. If a policy doesn\'t exist, a new policy is created.
--
-- You can\'t create a resource policy statement that allows cross-account
-- access.
module Amazonka.LexV2Models.CreateResourcePolicyStatement
  ( -- * Creating a Request
    CreateResourcePolicyStatement (..),
    newCreateResourcePolicyStatement,

    -- * Request Lenses
    createResourcePolicyStatement_expectedRevisionId,
    createResourcePolicyStatement_condition,
    createResourcePolicyStatement_resourceArn,
    createResourcePolicyStatement_statementId,
    createResourcePolicyStatement_effect,
    createResourcePolicyStatement_principal,
    createResourcePolicyStatement_action,

    -- * Destructuring the Response
    CreateResourcePolicyStatementResponse (..),
    newCreateResourcePolicyStatementResponse,

    -- * Response Lenses
    createResourcePolicyStatementResponse_revisionId,
    createResourcePolicyStatementResponse_resourceArn,
    createResourcePolicyStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourcePolicyStatement' smart constructor.
data CreateResourcePolicyStatement = CreateResourcePolicyStatement'
  { -- | The identifier of the revision of the policy to edit. If this revision
    -- ID doesn\'t match the current revision ID, Amazon Lex throws an
    -- exception.
    --
    -- If you don\'t specify a revision, Amazon Lex overwrites the contents of
    -- the policy with the new values.
    expectedRevisionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies a condition when the policy is in effect. If the principal of
    -- the policy is a service principal, you must provide two condition
    -- blocks, one with a SourceAccount global condition key and one with a
    -- SourceArn global condition key.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM JSON policy elements: Condition>
    -- .
    condition :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Text,
    -- | The name of the statement. The ID is the same as the @Sid@ IAM property.
    -- The statement name must be unique within the policy. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_sid.html IAM JSON policy elements: Sid>.
    statementId :: Prelude.Text,
    -- | Determines whether the statement allows or denies access to the
    -- resource.
    effect :: Effect,
    -- | An IAM principal, such as an IAM users, IAM roles, or AWS services that
    -- is allowed or denied access to a resource. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html AWS JSON policy elements: Principal>.
    principal :: [Principal],
    -- | The Amazon Lex action that this policy either allows or denies. The
    -- action must apply to the resource type of the specified ARN. For more
    -- information, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazonlexv2.html Actions, resources, and condition keys for Amazon Lex V2>.
    action :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourcePolicyStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedRevisionId', 'createResourcePolicyStatement_expectedRevisionId' - The identifier of the revision of the policy to edit. If this revision
-- ID doesn\'t match the current revision ID, Amazon Lex throws an
-- exception.
--
-- If you don\'t specify a revision, Amazon Lex overwrites the contents of
-- the policy with the new values.
--
-- 'condition', 'createResourcePolicyStatement_condition' - Specifies a condition when the policy is in effect. If the principal of
-- the policy is a service principal, you must provide two condition
-- blocks, one with a SourceAccount global condition key and one with a
-- SourceArn global condition key.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM JSON policy elements: Condition>
-- .
--
-- 'resourceArn', 'createResourcePolicyStatement_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'statementId', 'createResourcePolicyStatement_statementId' - The name of the statement. The ID is the same as the @Sid@ IAM property.
-- The statement name must be unique within the policy. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_sid.html IAM JSON policy elements: Sid>.
--
-- 'effect', 'createResourcePolicyStatement_effect' - Determines whether the statement allows or denies access to the
-- resource.
--
-- 'principal', 'createResourcePolicyStatement_principal' - An IAM principal, such as an IAM users, IAM roles, or AWS services that
-- is allowed or denied access to a resource. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html AWS JSON policy elements: Principal>.
--
-- 'action', 'createResourcePolicyStatement_action' - The Amazon Lex action that this policy either allows or denies. The
-- action must apply to the resource type of the specified ARN. For more
-- information, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazonlexv2.html Actions, resources, and condition keys for Amazon Lex V2>.
newCreateResourcePolicyStatement ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'statementId'
  Prelude.Text ->
  -- | 'effect'
  Effect ->
  CreateResourcePolicyStatement
newCreateResourcePolicyStatement
  pResourceArn_
  pStatementId_
  pEffect_ =
    CreateResourcePolicyStatement'
      { expectedRevisionId =
          Prelude.Nothing,
        condition = Prelude.Nothing,
        resourceArn = pResourceArn_,
        statementId = pStatementId_,
        effect = pEffect_,
        principal = Prelude.mempty,
        action = Prelude.mempty
      }

-- | The identifier of the revision of the policy to edit. If this revision
-- ID doesn\'t match the current revision ID, Amazon Lex throws an
-- exception.
--
-- If you don\'t specify a revision, Amazon Lex overwrites the contents of
-- the policy with the new values.
createResourcePolicyStatement_expectedRevisionId :: Lens.Lens' CreateResourcePolicyStatement (Prelude.Maybe Prelude.Text)
createResourcePolicyStatement_expectedRevisionId = Lens.lens (\CreateResourcePolicyStatement' {expectedRevisionId} -> expectedRevisionId) (\s@CreateResourcePolicyStatement' {} a -> s {expectedRevisionId = a} :: CreateResourcePolicyStatement)

-- | Specifies a condition when the policy is in effect. If the principal of
-- the policy is a service principal, you must provide two condition
-- blocks, one with a SourceAccount global condition key and one with a
-- SourceArn global condition key.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM JSON policy elements: Condition>
-- .
createResourcePolicyStatement_condition :: Lens.Lens' CreateResourcePolicyStatement (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
createResourcePolicyStatement_condition = Lens.lens (\CreateResourcePolicyStatement' {condition} -> condition) (\s@CreateResourcePolicyStatement' {} a -> s {condition = a} :: CreateResourcePolicyStatement) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
createResourcePolicyStatement_resourceArn :: Lens.Lens' CreateResourcePolicyStatement Prelude.Text
createResourcePolicyStatement_resourceArn = Lens.lens (\CreateResourcePolicyStatement' {resourceArn} -> resourceArn) (\s@CreateResourcePolicyStatement' {} a -> s {resourceArn = a} :: CreateResourcePolicyStatement)

-- | The name of the statement. The ID is the same as the @Sid@ IAM property.
-- The statement name must be unique within the policy. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_sid.html IAM JSON policy elements: Sid>.
createResourcePolicyStatement_statementId :: Lens.Lens' CreateResourcePolicyStatement Prelude.Text
createResourcePolicyStatement_statementId = Lens.lens (\CreateResourcePolicyStatement' {statementId} -> statementId) (\s@CreateResourcePolicyStatement' {} a -> s {statementId = a} :: CreateResourcePolicyStatement)

-- | Determines whether the statement allows or denies access to the
-- resource.
createResourcePolicyStatement_effect :: Lens.Lens' CreateResourcePolicyStatement Effect
createResourcePolicyStatement_effect = Lens.lens (\CreateResourcePolicyStatement' {effect} -> effect) (\s@CreateResourcePolicyStatement' {} a -> s {effect = a} :: CreateResourcePolicyStatement)

-- | An IAM principal, such as an IAM users, IAM roles, or AWS services that
-- is allowed or denied access to a resource. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_principal.html AWS JSON policy elements: Principal>.
createResourcePolicyStatement_principal :: Lens.Lens' CreateResourcePolicyStatement [Principal]
createResourcePolicyStatement_principal = Lens.lens (\CreateResourcePolicyStatement' {principal} -> principal) (\s@CreateResourcePolicyStatement' {} a -> s {principal = a} :: CreateResourcePolicyStatement) Prelude.. Lens.coerced

-- | The Amazon Lex action that this policy either allows or denies. The
-- action must apply to the resource type of the specified ARN. For more
-- information, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_amazonlexv2.html Actions, resources, and condition keys for Amazon Lex V2>.
createResourcePolicyStatement_action :: Lens.Lens' CreateResourcePolicyStatement [Prelude.Text]
createResourcePolicyStatement_action = Lens.lens (\CreateResourcePolicyStatement' {action} -> action) (\s@CreateResourcePolicyStatement' {} a -> s {action = a} :: CreateResourcePolicyStatement) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    CreateResourcePolicyStatement
  where
  type
    AWSResponse CreateResourcePolicyStatement =
      CreateResourcePolicyStatementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourcePolicyStatementResponse'
            Prelude.<$> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "resourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateResourcePolicyStatement
  where
  hashWithSalt _salt CreateResourcePolicyStatement' {..} =
    _salt `Prelude.hashWithSalt` expectedRevisionId
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` statementId
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` action

instance Prelude.NFData CreateResourcePolicyStatement where
  rnf CreateResourcePolicyStatement' {..} =
    Prelude.rnf expectedRevisionId
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf statementId
      `Prelude.seq` Prelude.rnf effect
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf action

instance Data.ToHeaders CreateResourcePolicyStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResourcePolicyStatement where
  toJSON CreateResourcePolicyStatement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("condition" Data..=) Prelude.<$> condition,
            Prelude.Just ("statementId" Data..= statementId),
            Prelude.Just ("effect" Data..= effect),
            Prelude.Just ("principal" Data..= principal),
            Prelude.Just ("action" Data..= action)
          ]
      )

instance Data.ToPath CreateResourcePolicyStatement where
  toPath CreateResourcePolicyStatement' {..} =
    Prelude.mconcat
      ["/policy/", Data.toBS resourceArn, "/statements/"]

instance Data.ToQuery CreateResourcePolicyStatement where
  toQuery CreateResourcePolicyStatement' {..} =
    Prelude.mconcat
      ["expectedRevisionId" Data.=: expectedRevisionId]

-- | /See:/ 'newCreateResourcePolicyStatementResponse' smart constructor.
data CreateResourcePolicyStatementResponse = CreateResourcePolicyStatementResponse'
  { -- | The current revision of the resource policy. Use the revision ID to make
    -- sure that you are updating the most current version of a resource policy
    -- when you add a policy statement to a resource, delete a resource, or
    -- update a resource.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
    -- policy is attached to.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourcePolicyStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'createResourcePolicyStatementResponse_revisionId' - The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
--
-- 'resourceArn', 'createResourcePolicyStatementResponse_resourceArn' - The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
--
-- 'httpStatus', 'createResourcePolicyStatementResponse_httpStatus' - The response's http status code.
newCreateResourcePolicyStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourcePolicyStatementResponse
newCreateResourcePolicyStatementResponse pHttpStatus_ =
  CreateResourcePolicyStatementResponse'
    { revisionId =
        Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current revision of the resource policy. Use the revision ID to make
-- sure that you are updating the most current version of a resource policy
-- when you add a policy statement to a resource, delete a resource, or
-- update a resource.
createResourcePolicyStatementResponse_revisionId :: Lens.Lens' CreateResourcePolicyStatementResponse (Prelude.Maybe Prelude.Text)
createResourcePolicyStatementResponse_revisionId = Lens.lens (\CreateResourcePolicyStatementResponse' {revisionId} -> revisionId) (\s@CreateResourcePolicyStatementResponse' {} a -> s {revisionId = a} :: CreateResourcePolicyStatementResponse)

-- | The Amazon Resource Name (ARN) of the bot or bot alias that the resource
-- policy is attached to.
createResourcePolicyStatementResponse_resourceArn :: Lens.Lens' CreateResourcePolicyStatementResponse (Prelude.Maybe Prelude.Text)
createResourcePolicyStatementResponse_resourceArn = Lens.lens (\CreateResourcePolicyStatementResponse' {resourceArn} -> resourceArn) (\s@CreateResourcePolicyStatementResponse' {} a -> s {resourceArn = a} :: CreateResourcePolicyStatementResponse)

-- | The response's http status code.
createResourcePolicyStatementResponse_httpStatus :: Lens.Lens' CreateResourcePolicyStatementResponse Prelude.Int
createResourcePolicyStatementResponse_httpStatus = Lens.lens (\CreateResourcePolicyStatementResponse' {httpStatus} -> httpStatus) (\s@CreateResourcePolicyStatementResponse' {} a -> s {httpStatus = a} :: CreateResourcePolicyStatementResponse)

instance
  Prelude.NFData
    CreateResourcePolicyStatementResponse
  where
  rnf CreateResourcePolicyStatementResponse' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf httpStatus
