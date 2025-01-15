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
-- Module      : Amazonka.RAM.DisassociateResourceShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified principals or resources from the specified
-- resource share.
module Amazonka.RAM.DisassociateResourceShare
  ( -- * Creating a Request
    DisassociateResourceShare (..),
    newDisassociateResourceShare,

    -- * Request Lenses
    disassociateResourceShare_clientToken,
    disassociateResourceShare_principals,
    disassociateResourceShare_resourceArns,
    disassociateResourceShare_resourceShareArn,

    -- * Destructuring the Response
    DisassociateResourceShareResponse (..),
    newDisassociateResourceShareResponse,

    -- * Response Lenses
    disassociateResourceShareResponse_clientToken,
    disassociateResourceShareResponse_resourceShareAssociations,
    disassociateResourceShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateResourceShare' smart constructor.
data DisassociateResourceShare = DisassociateResourceShare'
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
    -- | Specifies a list of one or more principals that no longer are to have
    -- access to the resources in this resource share.
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
    -- for one or more resources that you want to remove from the resource
    -- share. After the operation runs, these resources are no longer shared
    -- with principals outside of the Amazon Web Services account that created
    -- the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share that you want to remove resources from.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateResourceShare_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'principals', 'disassociateResourceShare_principals' - Specifies a list of one or more principals that no longer are to have
-- access to the resources in this resource share.
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
-- 'resourceArns', 'disassociateResourceShare_resourceArns' - Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- for one or more resources that you want to remove from the resource
-- share. After the operation runs, these resources are no longer shared
-- with principals outside of the Amazon Web Services account that created
-- the resources.
--
-- 'resourceShareArn', 'disassociateResourceShare_resourceShareArn' - Specifies
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to remove resources from.
newDisassociateResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  DisassociateResourceShare
newDisassociateResourceShare pResourceShareArn_ =
  DisassociateResourceShare'
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
disassociateResourceShare_clientToken :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe Prelude.Text)
disassociateResourceShare_clientToken = Lens.lens (\DisassociateResourceShare' {clientToken} -> clientToken) (\s@DisassociateResourceShare' {} a -> s {clientToken = a} :: DisassociateResourceShare)

-- | Specifies a list of one or more principals that no longer are to have
-- access to the resources in this resource share.
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
disassociateResourceShare_principals :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe [Prelude.Text])
disassociateResourceShare_principals = Lens.lens (\DisassociateResourceShare' {principals} -> principals) (\s@DisassociateResourceShare' {} a -> s {principals = a} :: DisassociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- for one or more resources that you want to remove from the resource
-- share. After the operation runs, these resources are no longer shared
-- with principals outside of the Amazon Web Services account that created
-- the resources.
disassociateResourceShare_resourceArns :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe [Prelude.Text])
disassociateResourceShare_resourceArns = Lens.lens (\DisassociateResourceShare' {resourceArns} -> resourceArns) (\s@DisassociateResourceShare' {} a -> s {resourceArns = a} :: DisassociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | Specifies
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to remove resources from.
disassociateResourceShare_resourceShareArn :: Lens.Lens' DisassociateResourceShare Prelude.Text
disassociateResourceShare_resourceShareArn = Lens.lens (\DisassociateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@DisassociateResourceShare' {} a -> s {resourceShareArn = a} :: DisassociateResourceShare)

instance Core.AWSRequest DisassociateResourceShare where
  type
    AWSResponse DisassociateResourceShare =
      DisassociateResourceShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResourceShareResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> ( x
                            Data..?> "resourceShareAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateResourceShare where
  hashWithSalt _salt DisassociateResourceShare' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData DisassociateResourceShare where
  rnf DisassociateResourceShare' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf principals `Prelude.seq`
        Prelude.rnf resourceArns `Prelude.seq`
          Prelude.rnf resourceShareArn

instance Data.ToHeaders DisassociateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateResourceShare where
  toJSON DisassociateResourceShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("principals" Data..=) Prelude.<$> principals,
            ("resourceArns" Data..=) Prelude.<$> resourceArns,
            Prelude.Just
              ("resourceShareArn" Data..= resourceShareArn)
          ]
      )

instance Data.ToPath DisassociateResourceShare where
  toPath = Prelude.const "/disassociateresourceshare"

instance Data.ToQuery DisassociateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResourceShareResponse' smart constructor.
data DisassociateResourceShareResponse = DisassociateResourceShareResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain information about the updated
    -- associations for this resource share.
    resourceShareAssociations :: Prelude.Maybe [ResourceShareAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateResourceShareResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'resourceShareAssociations', 'disassociateResourceShareResponse_resourceShareAssociations' - An array of objects that contain information about the updated
-- associations for this resource share.
--
-- 'httpStatus', 'disassociateResourceShareResponse_httpStatus' - The response's http status code.
newDisassociateResourceShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResourceShareResponse
newDisassociateResourceShareResponse pHttpStatus_ =
  DisassociateResourceShareResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShareAssociations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
disassociateResourceShareResponse_clientToken :: Lens.Lens' DisassociateResourceShareResponse (Prelude.Maybe Prelude.Text)
disassociateResourceShareResponse_clientToken = Lens.lens (\DisassociateResourceShareResponse' {clientToken} -> clientToken) (\s@DisassociateResourceShareResponse' {} a -> s {clientToken = a} :: DisassociateResourceShareResponse)

-- | An array of objects that contain information about the updated
-- associations for this resource share.
disassociateResourceShareResponse_resourceShareAssociations :: Lens.Lens' DisassociateResourceShareResponse (Prelude.Maybe [ResourceShareAssociation])
disassociateResourceShareResponse_resourceShareAssociations = Lens.lens (\DisassociateResourceShareResponse' {resourceShareAssociations} -> resourceShareAssociations) (\s@DisassociateResourceShareResponse' {} a -> s {resourceShareAssociations = a} :: DisassociateResourceShareResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociateResourceShareResponse_httpStatus :: Lens.Lens' DisassociateResourceShareResponse Prelude.Int
disassociateResourceShareResponse_httpStatus = Lens.lens (\DisassociateResourceShareResponse' {httpStatus} -> httpStatus) (\s@DisassociateResourceShareResponse' {} a -> s {httpStatus = a} :: DisassociateResourceShareResponse)

instance
  Prelude.NFData
    DisassociateResourceShareResponse
  where
  rnf DisassociateResourceShareResponse' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf resourceShareAssociations `Prelude.seq`
        Prelude.rnf httpStatus
