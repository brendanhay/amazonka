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
-- Module      : Amazonka.RAM.CreateResourceShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource share. You must provide a list of the Amazon Resource
-- Names (ARNs) for the resources you want to share. You must also specify
-- who you want to share the resources with, and the permissions that you
-- grant them.
--
-- Sharing a resource makes it available for use by principals outside of
-- the Amazon Web Services account that created the resource. Sharing
-- doesn\'t change any permissions or quotas that apply to the resource in
-- the account that created it.
module Amazonka.RAM.CreateResourceShare
  ( -- * Creating a Request
    CreateResourceShare (..),
    newCreateResourceShare,

    -- * Request Lenses
    createResourceShare_tags,
    createResourceShare_clientToken,
    createResourceShare_permissionArns,
    createResourceShare_principals,
    createResourceShare_resourceArns,
    createResourceShare_allowExternalPrincipals,
    createResourceShare_name,

    -- * Destructuring the Response
    CreateResourceShareResponse (..),
    newCreateResourceShareResponse,

    -- * Response Lenses
    createResourceShareResponse_clientToken,
    createResourceShareResponse_resourceShare,
    createResourceShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceShare' smart constructor.
data CreateResourceShare = CreateResourceShare'
  { -- | One or more tags.
    tags :: Prelude.Maybe [Tag],
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the permissions to associate with
    -- the resource share. If you do not specify an ARN for the permission, RAM
    -- automatically attaches the default version of the permission for each
    -- resource type. Only one permission can be associated with each resource
    -- type in a resource share.
    permissionArns :: Prelude.Maybe [Prelude.Text],
    -- | The principals to associate with the resource share. The possible values
    -- are:
    --
    -- -   An Amazon Web Services account ID
    --
    -- -   An Amazon Resource Name (ARN) of an organization in Organizations
    --
    -- -   An ARN of an organizational unit (OU) in Organizations
    --
    -- -   An ARN of an IAM role
    --
    -- -   An ARN of an IAM user
    --
    -- Not all resource types can be shared with IAM roles and IAM users. For
    -- more information, see
    -- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and IAM users>
    -- in the /Resource Access Manager User Guide/.
    principals :: Prelude.Maybe [Prelude.Text],
    -- | The ARNs of the resources to associate with the resource share.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether principals outside your organization in Organizations
    -- can be associated with a resource share.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource share.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createResourceShare_tags' - One or more tags.
--
-- 'clientToken', 'createResourceShare_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'permissionArns', 'createResourceShare_permissionArns' - The Amazon Resource Names (ARNs) of the permissions to associate with
-- the resource share. If you do not specify an ARN for the permission, RAM
-- automatically attaches the default version of the permission for each
-- resource type. Only one permission can be associated with each resource
-- type in a resource share.
--
-- 'principals', 'createResourceShare_principals' - The principals to associate with the resource share. The possible values
-- are:
--
-- -   An Amazon Web Services account ID
--
-- -   An Amazon Resource Name (ARN) of an organization in Organizations
--
-- -   An ARN of an organizational unit (OU) in Organizations
--
-- -   An ARN of an IAM role
--
-- -   An ARN of an IAM user
--
-- Not all resource types can be shared with IAM roles and IAM users. For
-- more information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and IAM users>
-- in the /Resource Access Manager User Guide/.
--
-- 'resourceArns', 'createResourceShare_resourceArns' - The ARNs of the resources to associate with the resource share.
--
-- 'allowExternalPrincipals', 'createResourceShare_allowExternalPrincipals' - Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
--
-- 'name', 'createResourceShare_name' - The name of the resource share.
newCreateResourceShare ::
  -- | 'name'
  Prelude.Text ->
  CreateResourceShare
newCreateResourceShare pName_ =
  CreateResourceShare'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      permissionArns = Prelude.Nothing,
      principals = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      allowExternalPrincipals = Prelude.Nothing,
      name = pName_
    }

-- | One or more tags.
createResourceShare_tags :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Tag])
createResourceShare_tags = Lens.lens (\CreateResourceShare' {tags} -> tags) (\s@CreateResourceShare' {} a -> s {tags = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createResourceShare_clientToken :: Lens.Lens' CreateResourceShare (Prelude.Maybe Prelude.Text)
createResourceShare_clientToken = Lens.lens (\CreateResourceShare' {clientToken} -> clientToken) (\s@CreateResourceShare' {} a -> s {clientToken = a} :: CreateResourceShare)

-- | The Amazon Resource Names (ARNs) of the permissions to associate with
-- the resource share. If you do not specify an ARN for the permission, RAM
-- automatically attaches the default version of the permission for each
-- resource type. Only one permission can be associated with each resource
-- type in a resource share.
createResourceShare_permissionArns :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_permissionArns = Lens.lens (\CreateResourceShare' {permissionArns} -> permissionArns) (\s@CreateResourceShare' {} a -> s {permissionArns = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The principals to associate with the resource share. The possible values
-- are:
--
-- -   An Amazon Web Services account ID
--
-- -   An Amazon Resource Name (ARN) of an organization in Organizations
--
-- -   An ARN of an organizational unit (OU) in Organizations
--
-- -   An ARN of an IAM role
--
-- -   An ARN of an IAM user
--
-- Not all resource types can be shared with IAM roles and IAM users. For
-- more information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and IAM users>
-- in the /Resource Access Manager User Guide/.
createResourceShare_principals :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_principals = Lens.lens (\CreateResourceShare' {principals} -> principals) (\s@CreateResourceShare' {} a -> s {principals = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of the resources to associate with the resource share.
createResourceShare_resourceArns :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_resourceArns = Lens.lens (\CreateResourceShare' {resourceArns} -> resourceArns) (\s@CreateResourceShare' {} a -> s {resourceArns = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
createResourceShare_allowExternalPrincipals :: Lens.Lens' CreateResourceShare (Prelude.Maybe Prelude.Bool)
createResourceShare_allowExternalPrincipals = Lens.lens (\CreateResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@CreateResourceShare' {} a -> s {allowExternalPrincipals = a} :: CreateResourceShare)

-- | The name of the resource share.
createResourceShare_name :: Lens.Lens' CreateResourceShare Prelude.Text
createResourceShare_name = Lens.lens (\CreateResourceShare' {name} -> name) (\s@CreateResourceShare' {} a -> s {name = a} :: CreateResourceShare)

instance Core.AWSRequest CreateResourceShare where
  type
    AWSResponse CreateResourceShare =
      CreateResourceShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceShareResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "resourceShare")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResourceShare where
  hashWithSalt _salt CreateResourceShare' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` permissionArns
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` allowExternalPrincipals
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateResourceShare where
  rnf CreateResourceShare' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf permissionArns
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf allowExternalPrincipals
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateResourceShare where
  toJSON CreateResourceShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("permissionArns" Core..=)
              Prelude.<$> permissionArns,
            ("principals" Core..=) Prelude.<$> principals,
            ("resourceArns" Core..=) Prelude.<$> resourceArns,
            ("allowExternalPrincipals" Core..=)
              Prelude.<$> allowExternalPrincipals,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateResourceShare where
  toPath = Prelude.const "/createresourceshare"

instance Core.ToQuery CreateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceShareResponse' smart constructor.
data CreateResourceShareResponse = CreateResourceShareResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the resource share.
    resourceShare :: Prelude.Maybe ResourceShare,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createResourceShareResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShare', 'createResourceShareResponse_resourceShare' - Information about the resource share.
--
-- 'httpStatus', 'createResourceShareResponse_httpStatus' - The response's http status code.
newCreateResourceShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceShareResponse
newCreateResourceShareResponse pHttpStatus_ =
  CreateResourceShareResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShare = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createResourceShareResponse_clientToken :: Lens.Lens' CreateResourceShareResponse (Prelude.Maybe Prelude.Text)
createResourceShareResponse_clientToken = Lens.lens (\CreateResourceShareResponse' {clientToken} -> clientToken) (\s@CreateResourceShareResponse' {} a -> s {clientToken = a} :: CreateResourceShareResponse)

-- | Information about the resource share.
createResourceShareResponse_resourceShare :: Lens.Lens' CreateResourceShareResponse (Prelude.Maybe ResourceShare)
createResourceShareResponse_resourceShare = Lens.lens (\CreateResourceShareResponse' {resourceShare} -> resourceShare) (\s@CreateResourceShareResponse' {} a -> s {resourceShare = a} :: CreateResourceShareResponse)

-- | The response's http status code.
createResourceShareResponse_httpStatus :: Lens.Lens' CreateResourceShareResponse Prelude.Int
createResourceShareResponse_httpStatus = Lens.lens (\CreateResourceShareResponse' {httpStatus} -> httpStatus) (\s@CreateResourceShareResponse' {} a -> s {httpStatus = a} :: CreateResourceShareResponse)

instance Prelude.NFData CreateResourceShareResponse where
  rnf CreateResourceShareResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShare
      `Prelude.seq` Prelude.rnf httpStatus
