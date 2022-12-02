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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource share. You can provide a list of the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- for the resources that you want to share, a list of principals you want
-- to share the resources with, and the permissions to grant those
-- principals.
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResourceShare' smart constructor.
data CreateResourceShare = CreateResourceShare'
  { -- | Specifies one or more tags to attach to the resource share itself. It
    -- doesn\'t attach the tags to the resources associated with the resource
    -- share.
    tags :: Prelude.Maybe [Tag],
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
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- of the RAM permission to associate with the resource share. If you do
    -- not specify an ARN for the permission, RAM automatically attaches the
    -- default version of the permission for each resource type. You can
    -- associate only one permission with each resource type included in the
    -- resource share.
    permissionArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies a list of one or more principals to associate with the
    -- resource share.
    --
    -- You can include the following values:
    --
    -- -   An Amazon Web Services account ID, for example: @123456789012@
    --
    -- -   An
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    --     of an organization in Organizations, for example:
    --     @organizations::123456789012:organization\/o-exampleorgid@
    --
    -- -   An ARN of an organizational unit (OU) in Organizations, for example:
    --     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
    --
    -- -   An ARN of an IAM role, for example:
    --     @iam::123456789012:role\/rolename@
    --
    -- -   An ARN of an IAM user, for example:
    --     @iam::123456789012user\/username@
    --
    -- Not all resource types can be shared with IAM roles and users. For more
    -- information, see
    -- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
    -- in the /Resource Access Manager User Guide/.
    principals :: Prelude.Maybe [Prelude.Text],
    -- | Specifies a list of one or more ARNs of the resources to associate with
    -- the resource share.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether principals outside your organization in Organizations
    -- can be associated with a resource share. A value of @true@ lets you
    -- share with individual Amazon Web Services accounts that are /not/ in
    -- your organization. A value of @false@ only has meaning if your account
    -- is a member of an Amazon Web Services Organization. The default value is
    -- @true@.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the resource share.
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
-- 'tags', 'createResourceShare_tags' - Specifies one or more tags to attach to the resource share itself. It
-- doesn\'t attach the tags to the resources associated with the resource
-- share.
--
-- 'clientToken', 'createResourceShare_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'permissionArns', 'createResourceShare_permissionArns' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the RAM permission to associate with the resource share. If you do
-- not specify an ARN for the permission, RAM automatically attaches the
-- default version of the permission for each resource type. You can
-- associate only one permission with each resource type included in the
-- resource share.
--
-- 'principals', 'createResourceShare_principals' - Specifies a list of one or more principals to associate with the
-- resource share.
--
-- You can include the following values:
--
-- -   An Amazon Web Services account ID, for example: @123456789012@
--
-- -   An
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of an organization in Organizations, for example:
--     @organizations::123456789012:organization\/o-exampleorgid@
--
-- -   An ARN of an organizational unit (OU) in Organizations, for example:
--     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
--
-- -   An ARN of an IAM role, for example:
--     @iam::123456789012:role\/rolename@
--
-- -   An ARN of an IAM user, for example:
--     @iam::123456789012user\/username@
--
-- Not all resource types can be shared with IAM roles and users. For more
-- information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
-- in the /Resource Access Manager User Guide/.
--
-- 'resourceArns', 'createResourceShare_resourceArns' - Specifies a list of one or more ARNs of the resources to associate with
-- the resource share.
--
-- 'allowExternalPrincipals', 'createResourceShare_allowExternalPrincipals' - Specifies whether principals outside your organization in Organizations
-- can be associated with a resource share. A value of @true@ lets you
-- share with individual Amazon Web Services accounts that are /not/ in
-- your organization. A value of @false@ only has meaning if your account
-- is a member of an Amazon Web Services Organization. The default value is
-- @true@.
--
-- 'name', 'createResourceShare_name' - Specifies the name of the resource share.
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

-- | Specifies one or more tags to attach to the resource share itself. It
-- doesn\'t attach the tags to the resources associated with the resource
-- share.
createResourceShare_tags :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Tag])
createResourceShare_tags = Lens.lens (\CreateResourceShare' {tags} -> tags) (\s@CreateResourceShare' {} a -> s {tags = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

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
createResourceShare_clientToken :: Lens.Lens' CreateResourceShare (Prelude.Maybe Prelude.Text)
createResourceShare_clientToken = Lens.lens (\CreateResourceShare' {clientToken} -> clientToken) (\s@CreateResourceShare' {} a -> s {clientToken = a} :: CreateResourceShare)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the RAM permission to associate with the resource share. If you do
-- not specify an ARN for the permission, RAM automatically attaches the
-- default version of the permission for each resource type. You can
-- associate only one permission with each resource type included in the
-- resource share.
createResourceShare_permissionArns :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_permissionArns = Lens.lens (\CreateResourceShare' {permissionArns} -> permissionArns) (\s@CreateResourceShare' {} a -> s {permissionArns = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of one or more principals to associate with the
-- resource share.
--
-- You can include the following values:
--
-- -   An Amazon Web Services account ID, for example: @123456789012@
--
-- -   An
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of an organization in Organizations, for example:
--     @organizations::123456789012:organization\/o-exampleorgid@
--
-- -   An ARN of an organizational unit (OU) in Organizations, for example:
--     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
--
-- -   An ARN of an IAM role, for example:
--     @iam::123456789012:role\/rolename@
--
-- -   An ARN of an IAM user, for example:
--     @iam::123456789012user\/username@
--
-- Not all resource types can be shared with IAM roles and users. For more
-- information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
-- in the /Resource Access Manager User Guide/.
createResourceShare_principals :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_principals = Lens.lens (\CreateResourceShare' {principals} -> principals) (\s@CreateResourceShare' {} a -> s {principals = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of one or more ARNs of the resources to associate with
-- the resource share.
createResourceShare_resourceArns :: Lens.Lens' CreateResourceShare (Prelude.Maybe [Prelude.Text])
createResourceShare_resourceArns = Lens.lens (\CreateResourceShare' {resourceArns} -> resourceArns) (\s@CreateResourceShare' {} a -> s {resourceArns = a} :: CreateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether principals outside your organization in Organizations
-- can be associated with a resource share. A value of @true@ lets you
-- share with individual Amazon Web Services accounts that are /not/ in
-- your organization. A value of @false@ only has meaning if your account
-- is a member of an Amazon Web Services Organization. The default value is
-- @true@.
createResourceShare_allowExternalPrincipals :: Lens.Lens' CreateResourceShare (Prelude.Maybe Prelude.Bool)
createResourceShare_allowExternalPrincipals = Lens.lens (\CreateResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@CreateResourceShare' {} a -> s {allowExternalPrincipals = a} :: CreateResourceShare)

-- | Specifies the name of the resource share.
createResourceShare_name :: Lens.Lens' CreateResourceShare Prelude.Text
createResourceShare_name = Lens.lens (\CreateResourceShare' {name} -> name) (\s@CreateResourceShare' {} a -> s {name = a} :: CreateResourceShare)

instance Core.AWSRequest CreateResourceShare where
  type
    AWSResponse CreateResourceShare =
      CreateResourceShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceShareResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "resourceShare")
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

instance Data.ToHeaders CreateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResourceShare where
  toJSON CreateResourceShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("permissionArns" Data..=)
              Prelude.<$> permissionArns,
            ("principals" Data..=) Prelude.<$> principals,
            ("resourceArns" Data..=) Prelude.<$> resourceArns,
            ("allowExternalPrincipals" Data..=)
              Prelude.<$> allowExternalPrincipals,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateResourceShare where
  toPath = Prelude.const "/createresourceshare"

instance Data.ToQuery CreateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceShareResponse' smart constructor.
data CreateResourceShareResponse = CreateResourceShareResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An object with information about the new resource share.
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
-- 'clientToken', 'createResourceShareResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'resourceShare', 'createResourceShareResponse_resourceShare' - An object with information about the new resource share.
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

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
createResourceShareResponse_clientToken :: Lens.Lens' CreateResourceShareResponse (Prelude.Maybe Prelude.Text)
createResourceShareResponse_clientToken = Lens.lens (\CreateResourceShareResponse' {clientToken} -> clientToken) (\s@CreateResourceShareResponse' {} a -> s {clientToken = a} :: CreateResourceShareResponse)

-- | An object with information about the new resource share.
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
