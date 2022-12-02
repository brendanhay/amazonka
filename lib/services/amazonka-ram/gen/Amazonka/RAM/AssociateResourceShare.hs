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
-- Module      : Amazonka.RAM.AssociateResourceShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified list of principals and list of resources to a
-- resource share. Principals that already have access to this resource
-- share immediately receive access to the added resources. Newly added
-- principals immediately receive access to the resources shared in this
-- resource share.
module Amazonka.RAM.AssociateResourceShare
  ( -- * Creating a Request
    AssociateResourceShare (..),
    newAssociateResourceShare,

    -- * Request Lenses
    associateResourceShare_clientToken,
    associateResourceShare_principals,
    associateResourceShare_resourceArns,
    associateResourceShare_resourceShareArn,

    -- * Destructuring the Response
    AssociateResourceShareResponse (..),
    newAssociateResourceShareResponse,

    -- * Response Lenses
    associateResourceShareResponse_clientToken,
    associateResourceShareResponse_resourceShareAssociations,
    associateResourceShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateResourceShare' smart constructor.
data AssociateResourceShare = AssociateResourceShare'
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
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a list of principals to whom you want to the resource share.
    -- This can be @null@ if you want to add only resources.
    --
    -- What the principals can do with the resources in the share is determined
    -- by the RAM permissions that you associate with the resource share. See
    -- AssociateResourceSharePermission.
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
    -- | Specifies a list of
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- of the resources that you want to share. This can be @null@ if you want
    -- to add only principals.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share that you want to add principals or resources to.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateResourceShare_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'principals', 'associateResourceShare_principals' - Specifies a list of principals to whom you want to the resource share.
-- This can be @null@ if you want to add only resources.
--
-- What the principals can do with the resources in the share is determined
-- by the RAM permissions that you associate with the resource share. See
-- AssociateResourceSharePermission.
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
-- 'resourceArns', 'associateResourceShare_resourceArns' - Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resources that you want to share. This can be @null@ if you want
-- to add only principals.
--
-- 'resourceShareArn', 'associateResourceShare_resourceShareArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to add principals or resources to.
newAssociateResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  AssociateResourceShare
newAssociateResourceShare pResourceShareArn_ =
  AssociateResourceShare'
    { clientToken =
        Prelude.Nothing,
      principals = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
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
associateResourceShare_clientToken :: Lens.Lens' AssociateResourceShare (Prelude.Maybe Prelude.Text)
associateResourceShare_clientToken = Lens.lens (\AssociateResourceShare' {clientToken} -> clientToken) (\s@AssociateResourceShare' {} a -> s {clientToken = a} :: AssociateResourceShare)

-- | Specifies a list of principals to whom you want to the resource share.
-- This can be @null@ if you want to add only resources.
--
-- What the principals can do with the resources in the share is determined
-- by the RAM permissions that you associate with the resource share. See
-- AssociateResourceSharePermission.
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
associateResourceShare_principals :: Lens.Lens' AssociateResourceShare (Prelude.Maybe [Prelude.Text])
associateResourceShare_principals = Lens.lens (\AssociateResourceShare' {principals} -> principals) (\s@AssociateResourceShare' {} a -> s {principals = a} :: AssociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resources that you want to share. This can be @null@ if you want
-- to add only principals.
associateResourceShare_resourceArns :: Lens.Lens' AssociateResourceShare (Prelude.Maybe [Prelude.Text])
associateResourceShare_resourceArns = Lens.lens (\AssociateResourceShare' {resourceArns} -> resourceArns) (\s@AssociateResourceShare' {} a -> s {resourceArns = a} :: AssociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to add principals or resources to.
associateResourceShare_resourceShareArn :: Lens.Lens' AssociateResourceShare Prelude.Text
associateResourceShare_resourceShareArn = Lens.lens (\AssociateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@AssociateResourceShare' {} a -> s {resourceShareArn = a} :: AssociateResourceShare)

instance Core.AWSRequest AssociateResourceShare where
  type
    AWSResponse AssociateResourceShare =
      AssociateResourceShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResourceShareResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> ( x Data..?> "resourceShareAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateResourceShare where
  hashWithSalt _salt AssociateResourceShare' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData AssociateResourceShare where
  rnf AssociateResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Data.ToHeaders AssociateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateResourceShare where
  toJSON AssociateResourceShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("principals" Data..=) Prelude.<$> principals,
            ("resourceArns" Data..=) Prelude.<$> resourceArns,
            Prelude.Just
              ("resourceShareArn" Data..= resourceShareArn)
          ]
      )

instance Data.ToPath AssociateResourceShare where
  toPath = Prelude.const "/associateresourceshare"

instance Data.ToQuery AssociateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResourceShareResponse' smart constructor.
data AssociateResourceShareResponse = AssociateResourceShareResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain information about the associations.
    resourceShareAssociations :: Prelude.Maybe [ResourceShareAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateResourceShareResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'resourceShareAssociations', 'associateResourceShareResponse_resourceShareAssociations' - An array of objects that contain information about the associations.
--
-- 'httpStatus', 'associateResourceShareResponse_httpStatus' - The response's http status code.
newAssociateResourceShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateResourceShareResponse
newAssociateResourceShareResponse pHttpStatus_ =
  AssociateResourceShareResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShareAssociations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
associateResourceShareResponse_clientToken :: Lens.Lens' AssociateResourceShareResponse (Prelude.Maybe Prelude.Text)
associateResourceShareResponse_clientToken = Lens.lens (\AssociateResourceShareResponse' {clientToken} -> clientToken) (\s@AssociateResourceShareResponse' {} a -> s {clientToken = a} :: AssociateResourceShareResponse)

-- | An array of objects that contain information about the associations.
associateResourceShareResponse_resourceShareAssociations :: Lens.Lens' AssociateResourceShareResponse (Prelude.Maybe [ResourceShareAssociation])
associateResourceShareResponse_resourceShareAssociations = Lens.lens (\AssociateResourceShareResponse' {resourceShareAssociations} -> resourceShareAssociations) (\s@AssociateResourceShareResponse' {} a -> s {resourceShareAssociations = a} :: AssociateResourceShareResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
associateResourceShareResponse_httpStatus :: Lens.Lens' AssociateResourceShareResponse Prelude.Int
associateResourceShareResponse_httpStatus = Lens.lens (\AssociateResourceShareResponse' {httpStatus} -> httpStatus) (\s@AssociateResourceShareResponse' {} a -> s {httpStatus = a} :: AssociateResourceShareResponse)

instance
  Prelude.NFData
    AssociateResourceShareResponse
  where
  rnf AssociateResourceShareResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareAssociations
      `Prelude.seq` Prelude.rnf httpStatus
