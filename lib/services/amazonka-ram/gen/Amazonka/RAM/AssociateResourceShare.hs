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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified resource share with the specified principals
-- and resources.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateResourceShare' smart constructor.
data AssociateResourceShare = AssociateResourceShare'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
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
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the resource share.
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
-- 'clientToken', 'associateResourceShare_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'principals', 'associateResourceShare_principals' - The principals to associate with the resource share. The possible values
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
-- 'resourceArns', 'associateResourceShare_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'resourceShareArn', 'associateResourceShare_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateResourceShare_clientToken :: Lens.Lens' AssociateResourceShare (Prelude.Maybe Prelude.Text)
associateResourceShare_clientToken = Lens.lens (\AssociateResourceShare' {clientToken} -> clientToken) (\s@AssociateResourceShare' {} a -> s {clientToken = a} :: AssociateResourceShare)

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
associateResourceShare_principals :: Lens.Lens' AssociateResourceShare (Prelude.Maybe [Prelude.Text])
associateResourceShare_principals = Lens.lens (\AssociateResourceShare' {principals} -> principals) (\s@AssociateResourceShare' {} a -> s {principals = a} :: AssociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the resources.
associateResourceShare_resourceArns :: Lens.Lens' AssociateResourceShare (Prelude.Maybe [Prelude.Text])
associateResourceShare_resourceArns = Lens.lens (\AssociateResourceShare' {resourceArns} -> resourceArns) (\s@AssociateResourceShare' {} a -> s {resourceArns = a} :: AssociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource share.
associateResourceShare_resourceShareArn :: Lens.Lens' AssociateResourceShare Prelude.Text
associateResourceShare_resourceShareArn = Lens.lens (\AssociateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@AssociateResourceShare' {} a -> s {resourceShareArn = a} :: AssociateResourceShare)

instance Core.AWSRequest AssociateResourceShare where
  type
    AWSResponse AssociateResourceShare =
      AssociateResourceShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateResourceShareResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> ( x Core..?> "resourceShareAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateResourceShare where
  hashWithSalt salt' AssociateResourceShare' {..} =
    salt' `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData AssociateResourceShare where
  rnf AssociateResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf principals

instance Core.ToHeaders AssociateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateResourceShare where
  toJSON AssociateResourceShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("principals" Core..=) Prelude.<$> principals,
            ("resourceArns" Core..=) Prelude.<$> resourceArns,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn)
          ]
      )

instance Core.ToPath AssociateResourceShare where
  toPath = Prelude.const "/associateresourceshare"

instance Core.ToQuery AssociateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateResourceShareResponse' smart constructor.
data AssociateResourceShareResponse = AssociateResourceShareResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the associations.
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
-- 'clientToken', 'associateResourceShareResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareAssociations', 'associateResourceShareResponse_resourceShareAssociations' - Information about the associations.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
associateResourceShareResponse_clientToken :: Lens.Lens' AssociateResourceShareResponse (Prelude.Maybe Prelude.Text)
associateResourceShareResponse_clientToken = Lens.lens (\AssociateResourceShareResponse' {clientToken} -> clientToken) (\s@AssociateResourceShareResponse' {} a -> s {clientToken = a} :: AssociateResourceShareResponse)

-- | Information about the associations.
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
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceShareAssociations
