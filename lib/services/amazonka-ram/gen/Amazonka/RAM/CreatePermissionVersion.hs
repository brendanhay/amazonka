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
-- Module      : Amazonka.RAM.CreatePermissionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified customer managed permission. The
-- new version is automatically set as the default version of the customer
-- managed permission. New resource shares automatically use the default
-- permission. Existing resource shares continue to use their original
-- permission versions, but you can use ReplacePermissionAssociations to
-- update them.
--
-- If the specified customer managed permission already has the maximum of
-- 5 versions, then you must delete one of the existing versions before you
-- can create a new one.
module Amazonka.RAM.CreatePermissionVersion
  ( -- * Creating a Request
    CreatePermissionVersion (..),
    newCreatePermissionVersion,

    -- * Request Lenses
    createPermissionVersion_clientToken,
    createPermissionVersion_permissionArn,
    createPermissionVersion_policyTemplate,

    -- * Destructuring the Response
    CreatePermissionVersionResponse (..),
    newCreatePermissionVersionResponse,

    -- * Response Lenses
    createPermissionVersionResponse_clientToken,
    createPermissionVersionResponse_permission,
    createPermissionVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePermissionVersion' smart constructor.
data CreatePermissionVersion = CreatePermissionVersion'
  { -- | Specifies a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    --
    -- If you retry the operation with the same @ClientToken@, but with
    -- different parameters, the retry fails with an
    -- @IdempotentParameterMismatch@ error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the customer managed permission you\'re creating a new version for.
    permissionArn :: Prelude.Text,
    -- | A string in JSON format string that contains the following elements of a
    -- resource-based policy:
    --
    -- -   __Effect__: must be set to @ALLOW@.
    --
    -- -   __Action__: specifies the actions that are allowed by this customer
    --     managed permission. The list must contain only actions that are
    --     supported by the specified resource type. For a list of all actions
    --     supported by each resource type, see
    --     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
    --     in the /Identity and Access Management User Guide/.
    --
    -- -   __Condition__: (optional) specifies conditional parameters that must
    --     evaluate to true when a user attempts an action for that action to
    --     be allowed. For more information about the Condition element, see
    --     <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM policies: Condition element>
    --     in the /Identity and Access Management User Guide/.
    --
    -- This template can\'t include either the @Resource@ or @Principal@
    -- elements. Those are both filled in by RAM when it instantiates the
    -- resource-based policy on each resource shared using this managed
    -- permission. The @Resource@ comes from the ARN of the specific resource
    -- that you are sharing. The @Principal@ comes from the list of identities
    -- added to the resource share.
    policyTemplate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPermissionVersion_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
--
-- 'permissionArn', 'createPermissionVersion_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission you\'re creating a new version for.
--
-- 'policyTemplate', 'createPermissionVersion_policyTemplate' - A string in JSON format string that contains the following elements of a
-- resource-based policy:
--
-- -   __Effect__: must be set to @ALLOW@.
--
-- -   __Action__: specifies the actions that are allowed by this customer
--     managed permission. The list must contain only actions that are
--     supported by the specified resource type. For a list of all actions
--     supported by each resource type, see
--     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
--     in the /Identity and Access Management User Guide/.
--
-- -   __Condition__: (optional) specifies conditional parameters that must
--     evaluate to true when a user attempts an action for that action to
--     be allowed. For more information about the Condition element, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM policies: Condition element>
--     in the /Identity and Access Management User Guide/.
--
-- This template can\'t include either the @Resource@ or @Principal@
-- elements. Those are both filled in by RAM when it instantiates the
-- resource-based policy on each resource shared using this managed
-- permission. The @Resource@ comes from the ARN of the specific resource
-- that you are sharing. The @Principal@ comes from the list of identities
-- added to the resource share.
newCreatePermissionVersion ::
  -- | 'permissionArn'
  Prelude.Text ->
  -- | 'policyTemplate'
  Prelude.Text ->
  CreatePermissionVersion
newCreatePermissionVersion
  pPermissionArn_
  pPolicyTemplate_ =
    CreatePermissionVersion'
      { clientToken =
          Prelude.Nothing,
        permissionArn = pPermissionArn_,
        policyTemplate = pPolicyTemplate_
      }

-- | Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- If you retry the operation with the same @ClientToken@, but with
-- different parameters, the retry fails with an
-- @IdempotentParameterMismatch@ error.
createPermissionVersion_clientToken :: Lens.Lens' CreatePermissionVersion (Prelude.Maybe Prelude.Text)
createPermissionVersion_clientToken = Lens.lens (\CreatePermissionVersion' {clientToken} -> clientToken) (\s@CreatePermissionVersion' {} a -> s {clientToken = a} :: CreatePermissionVersion)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the customer managed permission you\'re creating a new version for.
createPermissionVersion_permissionArn :: Lens.Lens' CreatePermissionVersion Prelude.Text
createPermissionVersion_permissionArn = Lens.lens (\CreatePermissionVersion' {permissionArn} -> permissionArn) (\s@CreatePermissionVersion' {} a -> s {permissionArn = a} :: CreatePermissionVersion)

-- | A string in JSON format string that contains the following elements of a
-- resource-based policy:
--
-- -   __Effect__: must be set to @ALLOW@.
--
-- -   __Action__: specifies the actions that are allowed by this customer
--     managed permission. The list must contain only actions that are
--     supported by the specified resource type. For a list of all actions
--     supported by each resource type, see
--     <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
--     in the /Identity and Access Management User Guide/.
--
-- -   __Condition__: (optional) specifies conditional parameters that must
--     evaluate to true when a user attempts an action for that action to
--     be allowed. For more information about the Condition element, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html IAM policies: Condition element>
--     in the /Identity and Access Management User Guide/.
--
-- This template can\'t include either the @Resource@ or @Principal@
-- elements. Those are both filled in by RAM when it instantiates the
-- resource-based policy on each resource shared using this managed
-- permission. The @Resource@ comes from the ARN of the specific resource
-- that you are sharing. The @Principal@ comes from the list of identities
-- added to the resource share.
createPermissionVersion_policyTemplate :: Lens.Lens' CreatePermissionVersion Prelude.Text
createPermissionVersion_policyTemplate = Lens.lens (\CreatePermissionVersion' {policyTemplate} -> policyTemplate) (\s@CreatePermissionVersion' {} a -> s {policyTemplate = a} :: CreatePermissionVersion)

instance Core.AWSRequest CreatePermissionVersion where
  type
    AWSResponse CreatePermissionVersion =
      CreatePermissionVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePermissionVersionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "permission")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePermissionVersion where
  hashWithSalt _salt CreatePermissionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionArn
      `Prelude.hashWithSalt` policyTemplate

instance Prelude.NFData CreatePermissionVersion where
  rnf CreatePermissionVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf policyTemplate

instance Data.ToHeaders CreatePermissionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePermissionVersion where
  toJSON CreatePermissionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("permissionArn" Data..= permissionArn),
            Prelude.Just
              ("policyTemplate" Data..= policyTemplate)
          ]
      )

instance Data.ToPath CreatePermissionVersion where
  toPath = Prelude.const "/createpermissionversion"

instance Data.ToQuery CreatePermissionVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePermissionVersionResponse' smart constructor.
data CreatePermissionVersionResponse = CreatePermissionVersionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    permission :: Prelude.Maybe ResourceSharePermissionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPermissionVersionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'permission', 'createPermissionVersionResponse_permission' - Undocumented member.
--
-- 'httpStatus', 'createPermissionVersionResponse_httpStatus' - The response's http status code.
newCreatePermissionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePermissionVersionResponse
newCreatePermissionVersionResponse pHttpStatus_ =
  CreatePermissionVersionResponse'
    { clientToken =
        Prelude.Nothing,
      permission = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
createPermissionVersionResponse_clientToken :: Lens.Lens' CreatePermissionVersionResponse (Prelude.Maybe Prelude.Text)
createPermissionVersionResponse_clientToken = Lens.lens (\CreatePermissionVersionResponse' {clientToken} -> clientToken) (\s@CreatePermissionVersionResponse' {} a -> s {clientToken = a} :: CreatePermissionVersionResponse)

-- | Undocumented member.
createPermissionVersionResponse_permission :: Lens.Lens' CreatePermissionVersionResponse (Prelude.Maybe ResourceSharePermissionDetail)
createPermissionVersionResponse_permission = Lens.lens (\CreatePermissionVersionResponse' {permission} -> permission) (\s@CreatePermissionVersionResponse' {} a -> s {permission = a} :: CreatePermissionVersionResponse)

-- | The response's http status code.
createPermissionVersionResponse_httpStatus :: Lens.Lens' CreatePermissionVersionResponse Prelude.Int
createPermissionVersionResponse_httpStatus = Lens.lens (\CreatePermissionVersionResponse' {httpStatus} -> httpStatus) (\s@CreatePermissionVersionResponse' {} a -> s {httpStatus = a} :: CreatePermissionVersionResponse)

instance
  Prelude.NFData
    CreatePermissionVersionResponse
  where
  rnf CreatePermissionVersionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf httpStatus
