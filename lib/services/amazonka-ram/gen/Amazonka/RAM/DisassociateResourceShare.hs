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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateResourceShare' smart constructor.
data DisassociateResourceShare = DisassociateResourceShare'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The principals.
    principals :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the resource share.
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
-- 'clientToken', 'disassociateResourceShare_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'principals', 'disassociateResourceShare_principals' - The principals.
--
-- 'resourceArns', 'disassociateResourceShare_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'resourceShareArn', 'disassociateResourceShare_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
disassociateResourceShare_clientToken :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe Prelude.Text)
disassociateResourceShare_clientToken = Lens.lens (\DisassociateResourceShare' {clientToken} -> clientToken) (\s@DisassociateResourceShare' {} a -> s {clientToken = a} :: DisassociateResourceShare)

-- | The principals.
disassociateResourceShare_principals :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe [Prelude.Text])
disassociateResourceShare_principals = Lens.lens (\DisassociateResourceShare' {principals} -> principals) (\s@DisassociateResourceShare' {} a -> s {principals = a} :: DisassociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the resources.
disassociateResourceShare_resourceArns :: Lens.Lens' DisassociateResourceShare (Prelude.Maybe [Prelude.Text])
disassociateResourceShare_resourceArns = Lens.lens (\DisassociateResourceShare' {resourceArns} -> resourceArns) (\s@DisassociateResourceShare' {} a -> s {resourceArns = a} :: DisassociateResourceShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource share.
disassociateResourceShare_resourceShareArn :: Lens.Lens' DisassociateResourceShare Prelude.Text
disassociateResourceShare_resourceShareArn = Lens.lens (\DisassociateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@DisassociateResourceShare' {} a -> s {resourceShareArn = a} :: DisassociateResourceShare)

instance Core.AWSRequest DisassociateResourceShare where
  type
    AWSResponse DisassociateResourceShare =
      DisassociateResourceShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResourceShareResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> ( x Core..?> "resourceShareAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateResourceShare where
  hashWithSalt _salt DisassociateResourceShare' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData DisassociateResourceShare where
  rnf DisassociateResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Core.ToHeaders DisassociateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateResourceShare where
  toJSON DisassociateResourceShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("principals" Core..=) Prelude.<$> principals,
            ("resourceArns" Core..=) Prelude.<$> resourceArns,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn)
          ]
      )

instance Core.ToPath DisassociateResourceShare where
  toPath = Prelude.const "/disassociateresourceshare"

instance Core.ToQuery DisassociateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResourceShareResponse' smart constructor.
data DisassociateResourceShareResponse = DisassociateResourceShareResponse'
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
-- Create a value of 'DisassociateResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateResourceShareResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareAssociations', 'disassociateResourceShareResponse_resourceShareAssociations' - Information about the associations.
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

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
disassociateResourceShareResponse_clientToken :: Lens.Lens' DisassociateResourceShareResponse (Prelude.Maybe Prelude.Text)
disassociateResourceShareResponse_clientToken = Lens.lens (\DisassociateResourceShareResponse' {clientToken} -> clientToken) (\s@DisassociateResourceShareResponse' {} a -> s {clientToken = a} :: DisassociateResourceShareResponse)

-- | Information about the associations.
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
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareAssociations
      `Prelude.seq` Prelude.rnf httpStatus
