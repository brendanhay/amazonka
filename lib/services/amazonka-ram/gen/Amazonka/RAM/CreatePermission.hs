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
-- Module      : Amazonka.RAM.CreatePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customer managed permission for a specified resource type that
-- you can attach to resource shares. It is created in the Amazon Web
-- Services Region in which you call the operation.
module Amazonka.RAM.CreatePermission
  ( -- * Creating a Request
    CreatePermission (..),
    newCreatePermission,

    -- * Request Lenses
    createPermission_clientToken,
    createPermission_tags,
    createPermission_name,
    createPermission_resourceType,
    createPermission_policyTemplate,

    -- * Destructuring the Response
    CreatePermissionResponse (..),
    newCreatePermissionResponse,

    -- * Response Lenses
    createPermissionResponse_clientToken,
    createPermissionResponse_permission,
    createPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePermission' smart constructor.
data CreatePermission = CreatePermission'
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
    -- | Specifies a list of one or more tag key and value pairs to attach to the
    -- permission.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the name of the customer managed permission. The name must be
    -- unique within the Amazon Web Services Region.
    name :: Prelude.Text,
    -- | Specifies the name of the resource type that this customer managed
    -- permission applies to.
    --
    -- The format is @ @/@\<service-code>@/@:@/@\<resource-type>@/@ @ and is
    -- not case sensitive. For example, to specify an Amazon EC2 Subnet, you
    -- can use the string @ec2:subnet@. To see the list of valid values for
    -- this parameter, query the ListResourceTypes operation.
    resourceType :: Prelude.Text,
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
-- Create a value of 'CreatePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPermission_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'tags', 'createPermission_tags' - Specifies a list of one or more tag key and value pairs to attach to the
-- permission.
--
-- 'name', 'createPermission_name' - Specifies the name of the customer managed permission. The name must be
-- unique within the Amazon Web Services Region.
--
-- 'resourceType', 'createPermission_resourceType' - Specifies the name of the resource type that this customer managed
-- permission applies to.
--
-- The format is @ @/@\<service-code>@/@:@/@\<resource-type>@/@ @ and is
-- not case sensitive. For example, to specify an Amazon EC2 Subnet, you
-- can use the string @ec2:subnet@. To see the list of valid values for
-- this parameter, query the ListResourceTypes operation.
--
-- 'policyTemplate', 'createPermission_policyTemplate' - A string in JSON format string that contains the following elements of a
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
newCreatePermission ::
  -- | 'name'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'policyTemplate'
  Prelude.Text ->
  CreatePermission
newCreatePermission
  pName_
  pResourceType_
  pPolicyTemplate_ =
    CreatePermission'
      { clientToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        resourceType = pResourceType_,
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
createPermission_clientToken :: Lens.Lens' CreatePermission (Prelude.Maybe Prelude.Text)
createPermission_clientToken = Lens.lens (\CreatePermission' {clientToken} -> clientToken) (\s@CreatePermission' {} a -> s {clientToken = a} :: CreatePermission)

-- | Specifies a list of one or more tag key and value pairs to attach to the
-- permission.
createPermission_tags :: Lens.Lens' CreatePermission (Prelude.Maybe [Tag])
createPermission_tags = Lens.lens (\CreatePermission' {tags} -> tags) (\s@CreatePermission' {} a -> s {tags = a} :: CreatePermission) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the customer managed permission. The name must be
-- unique within the Amazon Web Services Region.
createPermission_name :: Lens.Lens' CreatePermission Prelude.Text
createPermission_name = Lens.lens (\CreatePermission' {name} -> name) (\s@CreatePermission' {} a -> s {name = a} :: CreatePermission)

-- | Specifies the name of the resource type that this customer managed
-- permission applies to.
--
-- The format is @ @/@\<service-code>@/@:@/@\<resource-type>@/@ @ and is
-- not case sensitive. For example, to specify an Amazon EC2 Subnet, you
-- can use the string @ec2:subnet@. To see the list of valid values for
-- this parameter, query the ListResourceTypes operation.
createPermission_resourceType :: Lens.Lens' CreatePermission Prelude.Text
createPermission_resourceType = Lens.lens (\CreatePermission' {resourceType} -> resourceType) (\s@CreatePermission' {} a -> s {resourceType = a} :: CreatePermission)

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
createPermission_policyTemplate :: Lens.Lens' CreatePermission Prelude.Text
createPermission_policyTemplate = Lens.lens (\CreatePermission' {policyTemplate} -> policyTemplate) (\s@CreatePermission' {} a -> s {policyTemplate = a} :: CreatePermission)

instance Core.AWSRequest CreatePermission where
  type
    AWSResponse CreatePermission =
      CreatePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePermissionResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "permission")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePermission where
  hashWithSalt _salt CreatePermission' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` policyTemplate

instance Prelude.NFData CreatePermission where
  rnf CreatePermission' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf policyTemplate

instance Data.ToHeaders CreatePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePermission where
  toJSON CreatePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("resourceType" Data..= resourceType),
            Prelude.Just
              ("policyTemplate" Data..= policyTemplate)
          ]
      )

instance Data.ToPath CreatePermission where
  toPath = Prelude.const "/createpermission"

instance Data.ToQuery CreatePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePermissionResponse' smart constructor.
data CreatePermissionResponse = CreatePermissionResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A structure with information about this customer managed permission.
    permission :: Prelude.Maybe ResourceSharePermissionSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createPermissionResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'permission', 'createPermissionResponse_permission' - A structure with information about this customer managed permission.
--
-- 'httpStatus', 'createPermissionResponse_httpStatus' - The response's http status code.
newCreatePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePermissionResponse
newCreatePermissionResponse pHttpStatus_ =
  CreatePermissionResponse'
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
createPermissionResponse_clientToken :: Lens.Lens' CreatePermissionResponse (Prelude.Maybe Prelude.Text)
createPermissionResponse_clientToken = Lens.lens (\CreatePermissionResponse' {clientToken} -> clientToken) (\s@CreatePermissionResponse' {} a -> s {clientToken = a} :: CreatePermissionResponse)

-- | A structure with information about this customer managed permission.
createPermissionResponse_permission :: Lens.Lens' CreatePermissionResponse (Prelude.Maybe ResourceSharePermissionSummary)
createPermissionResponse_permission = Lens.lens (\CreatePermissionResponse' {permission} -> permission) (\s@CreatePermissionResponse' {} a -> s {permission = a} :: CreatePermissionResponse)

-- | The response's http status code.
createPermissionResponse_httpStatus :: Lens.Lens' CreatePermissionResponse Prelude.Int
createPermissionResponse_httpStatus = Lens.lens (\CreatePermissionResponse' {httpStatus} -> httpStatus) (\s@CreatePermissionResponse' {} a -> s {httpStatus = a} :: CreatePermissionResponse)

instance Prelude.NFData CreatePermissionResponse where
  rnf CreatePermissionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permission
      `Prelude.seq` Prelude.rnf httpStatus
