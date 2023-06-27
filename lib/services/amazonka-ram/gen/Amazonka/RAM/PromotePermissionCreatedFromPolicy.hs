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
-- Module      : Amazonka.RAM.PromotePermissionCreatedFromPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you attach a resource-based policy to a resource, RAM automatically
-- creates a resource share of @featureSet@=@CREATED_FROM_POLICY@ with a
-- managed permission that has the same IAM permissions as the original
-- resource-based policy. However, this type of managed permission is
-- visible to only the resource share owner, and the associated resource
-- share can\'t be modified by using RAM.
--
-- This operation creates a separate, fully manageable customer managed
-- permission that has the same IAM permissions as the original
-- resource-based policy. You can associate this customer managed
-- permission to any resource shares.
--
-- Before you use PromoteResourceShareCreatedFromPolicy, you should first
-- run this operation to ensure that you have an appropriate customer
-- managed permission that can be associated with the promoted resource
-- share.
--
-- -   The original @CREATED_FROM_POLICY@ policy isn\'t deleted, and
--     resource shares using that original policy aren\'t automatically
--     updated.
--
-- -   You can\'t modify a @CREATED_FROM_POLICY@ resource share so you
--     can\'t associate the new customer managed permission by using
--     @ReplacePermsissionAssociations@. However, if you use
--     PromoteResourceShareCreatedFromPolicy, that operation automatically
--     associates the fully manageable customer managed permission to the
--     newly promoted @STANDARD@ resource share.
--
-- -   After you promote a resource share, if the original
--     @CREATED_FROM_POLICY@ managed permission has no other associations
--     to A resource share, then RAM automatically deletes it.
module Amazonka.RAM.PromotePermissionCreatedFromPolicy
  ( -- * Creating a Request
    PromotePermissionCreatedFromPolicy (..),
    newPromotePermissionCreatedFromPolicy,

    -- * Request Lenses
    promotePermissionCreatedFromPolicy_clientToken,
    promotePermissionCreatedFromPolicy_permissionArn,
    promotePermissionCreatedFromPolicy_name,

    -- * Destructuring the Response
    PromotePermissionCreatedFromPolicyResponse (..),
    newPromotePermissionCreatedFromPolicyResponse,

    -- * Response Lenses
    promotePermissionCreatedFromPolicyResponse_clientToken,
    promotePermissionCreatedFromPolicyResponse_permission,
    promotePermissionCreatedFromPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPromotePermissionCreatedFromPolicy' smart constructor.
data PromotePermissionCreatedFromPolicy = PromotePermissionCreatedFromPolicy'
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
    -- of the @CREATED_FROM_POLICY@ permission that you want to promote. You
    -- can get this
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- by calling the ListResourceSharePermissions operation.
    permissionArn :: Prelude.Text,
    -- | Specifies a name for the promoted customer managed permission.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromotePermissionCreatedFromPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'promotePermissionCreatedFromPolicy_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'permissionArn', 'promotePermissionCreatedFromPolicy_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the @CREATED_FROM_POLICY@ permission that you want to promote. You
-- can get this
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- by calling the ListResourceSharePermissions operation.
--
-- 'name', 'promotePermissionCreatedFromPolicy_name' - Specifies a name for the promoted customer managed permission.
newPromotePermissionCreatedFromPolicy ::
  -- | 'permissionArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  PromotePermissionCreatedFromPolicy
newPromotePermissionCreatedFromPolicy
  pPermissionArn_
  pName_ =
    PromotePermissionCreatedFromPolicy'
      { clientToken =
          Prelude.Nothing,
        permissionArn = pPermissionArn_,
        name = pName_
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
promotePermissionCreatedFromPolicy_clientToken :: Lens.Lens' PromotePermissionCreatedFromPolicy (Prelude.Maybe Prelude.Text)
promotePermissionCreatedFromPolicy_clientToken = Lens.lens (\PromotePermissionCreatedFromPolicy' {clientToken} -> clientToken) (\s@PromotePermissionCreatedFromPolicy' {} a -> s {clientToken = a} :: PromotePermissionCreatedFromPolicy)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the @CREATED_FROM_POLICY@ permission that you want to promote. You
-- can get this
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- by calling the ListResourceSharePermissions operation.
promotePermissionCreatedFromPolicy_permissionArn :: Lens.Lens' PromotePermissionCreatedFromPolicy Prelude.Text
promotePermissionCreatedFromPolicy_permissionArn = Lens.lens (\PromotePermissionCreatedFromPolicy' {permissionArn} -> permissionArn) (\s@PromotePermissionCreatedFromPolicy' {} a -> s {permissionArn = a} :: PromotePermissionCreatedFromPolicy)

-- | Specifies a name for the promoted customer managed permission.
promotePermissionCreatedFromPolicy_name :: Lens.Lens' PromotePermissionCreatedFromPolicy Prelude.Text
promotePermissionCreatedFromPolicy_name = Lens.lens (\PromotePermissionCreatedFromPolicy' {name} -> name) (\s@PromotePermissionCreatedFromPolicy' {} a -> s {name = a} :: PromotePermissionCreatedFromPolicy)

instance
  Core.AWSRequest
    PromotePermissionCreatedFromPolicy
  where
  type
    AWSResponse PromotePermissionCreatedFromPolicy =
      PromotePermissionCreatedFromPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PromotePermissionCreatedFromPolicyResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "permission")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PromotePermissionCreatedFromPolicy
  where
  hashWithSalt
    _salt
    PromotePermissionCreatedFromPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` permissionArn
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    PromotePermissionCreatedFromPolicy
  where
  rnf PromotePermissionCreatedFromPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToHeaders
    PromotePermissionCreatedFromPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PromotePermissionCreatedFromPolicy
  where
  toJSON PromotePermissionCreatedFromPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("permissionArn" Data..= permissionArn),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance
  Data.ToPath
    PromotePermissionCreatedFromPolicy
  where
  toPath =
    Prelude.const "/promotepermissioncreatedfrompolicy"

instance
  Data.ToQuery
    PromotePermissionCreatedFromPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPromotePermissionCreatedFromPolicyResponse' smart constructor.
data PromotePermissionCreatedFromPolicyResponse = PromotePermissionCreatedFromPolicyResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    permission :: Prelude.Maybe ResourceSharePermissionSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromotePermissionCreatedFromPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'promotePermissionCreatedFromPolicyResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'permission', 'promotePermissionCreatedFromPolicyResponse_permission' - Undocumented member.
--
-- 'httpStatus', 'promotePermissionCreatedFromPolicyResponse_httpStatus' - The response's http status code.
newPromotePermissionCreatedFromPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PromotePermissionCreatedFromPolicyResponse
newPromotePermissionCreatedFromPolicyResponse
  pHttpStatus_ =
    PromotePermissionCreatedFromPolicyResponse'
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
promotePermissionCreatedFromPolicyResponse_clientToken :: Lens.Lens' PromotePermissionCreatedFromPolicyResponse (Prelude.Maybe Prelude.Text)
promotePermissionCreatedFromPolicyResponse_clientToken = Lens.lens (\PromotePermissionCreatedFromPolicyResponse' {clientToken} -> clientToken) (\s@PromotePermissionCreatedFromPolicyResponse' {} a -> s {clientToken = a} :: PromotePermissionCreatedFromPolicyResponse)

-- | Undocumented member.
promotePermissionCreatedFromPolicyResponse_permission :: Lens.Lens' PromotePermissionCreatedFromPolicyResponse (Prelude.Maybe ResourceSharePermissionSummary)
promotePermissionCreatedFromPolicyResponse_permission = Lens.lens (\PromotePermissionCreatedFromPolicyResponse' {permission} -> permission) (\s@PromotePermissionCreatedFromPolicyResponse' {} a -> s {permission = a} :: PromotePermissionCreatedFromPolicyResponse)

-- | The response's http status code.
promotePermissionCreatedFromPolicyResponse_httpStatus :: Lens.Lens' PromotePermissionCreatedFromPolicyResponse Prelude.Int
promotePermissionCreatedFromPolicyResponse_httpStatus = Lens.lens (\PromotePermissionCreatedFromPolicyResponse' {httpStatus} -> httpStatus) (\s@PromotePermissionCreatedFromPolicyResponse' {} a -> s {httpStatus = a} :: PromotePermissionCreatedFromPolicyResponse)

instance
  Prelude.NFData
    PromotePermissionCreatedFromPolicyResponse
  where
  rnf PromotePermissionCreatedFromPolicyResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf httpStatus
