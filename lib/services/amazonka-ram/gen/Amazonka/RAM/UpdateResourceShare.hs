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
-- Module      : Amazonka.RAM.UpdateResourceShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified resource share that you own.
module Amazonka.RAM.UpdateResourceShare
  ( -- * Creating a Request
    UpdateResourceShare (..),
    newUpdateResourceShare,

    -- * Request Lenses
    updateResourceShare_clientToken,
    updateResourceShare_allowExternalPrincipals,
    updateResourceShare_name,
    updateResourceShare_resourceShareArn,

    -- * Destructuring the Response
    UpdateResourceShareResponse (..),
    newUpdateResourceShareResponse,

    -- * Response Lenses
    updateResourceShareResponse_clientToken,
    updateResourceShareResponse_resourceShare,
    updateResourceShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceShare' smart constructor.
data UpdateResourceShare = UpdateResourceShare'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether principals outside your organization in Organizations
    -- can be associated with a resource share.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource share.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateResourceShare_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'allowExternalPrincipals', 'updateResourceShare_allowExternalPrincipals' - Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
--
-- 'name', 'updateResourceShare_name' - The name of the resource share.
--
-- 'resourceShareArn', 'updateResourceShare_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
newUpdateResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  UpdateResourceShare
newUpdateResourceShare pResourceShareArn_ =
  UpdateResourceShare'
    { clientToken = Prelude.Nothing,
      allowExternalPrincipals = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateResourceShare_clientToken :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Text)
updateResourceShare_clientToken = Lens.lens (\UpdateResourceShare' {clientToken} -> clientToken) (\s@UpdateResourceShare' {} a -> s {clientToken = a} :: UpdateResourceShare)

-- | Indicates whether principals outside your organization in Organizations
-- can be associated with a resource share.
updateResourceShare_allowExternalPrincipals :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Bool)
updateResourceShare_allowExternalPrincipals = Lens.lens (\UpdateResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@UpdateResourceShare' {} a -> s {allowExternalPrincipals = a} :: UpdateResourceShare)

-- | The name of the resource share.
updateResourceShare_name :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Text)
updateResourceShare_name = Lens.lens (\UpdateResourceShare' {name} -> name) (\s@UpdateResourceShare' {} a -> s {name = a} :: UpdateResourceShare)

-- | The Amazon Resource Name (ARN) of the resource share.
updateResourceShare_resourceShareArn :: Lens.Lens' UpdateResourceShare Prelude.Text
updateResourceShare_resourceShareArn = Lens.lens (\UpdateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@UpdateResourceShare' {} a -> s {resourceShareArn = a} :: UpdateResourceShare)

instance Core.AWSRequest UpdateResourceShare where
  type
    AWSResponse UpdateResourceShare =
      UpdateResourceShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceShareResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "resourceShare")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceShare where
  hashWithSalt salt' UpdateResourceShare' {..} =
    salt' `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` allowExternalPrincipals
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateResourceShare where
  rnf UpdateResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf allowExternalPrincipals

instance Core.ToHeaders UpdateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResourceShare where
  toJSON UpdateResourceShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("allowExternalPrincipals" Core..=)
              Prelude.<$> allowExternalPrincipals,
            ("name" Core..=) Prelude.<$> name,
            Prelude.Just
              ("resourceShareArn" Core..= resourceShareArn)
          ]
      )

instance Core.ToPath UpdateResourceShare where
  toPath = Prelude.const "/updateresourceshare"

instance Core.ToQuery UpdateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceShareResponse' smart constructor.
data UpdateResourceShareResponse = UpdateResourceShareResponse'
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
-- Create a value of 'UpdateResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateResourceShareResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShare', 'updateResourceShareResponse_resourceShare' - Information about the resource share.
--
-- 'httpStatus', 'updateResourceShareResponse_httpStatus' - The response's http status code.
newUpdateResourceShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceShareResponse
newUpdateResourceShareResponse pHttpStatus_ =
  UpdateResourceShareResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShare = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
updateResourceShareResponse_clientToken :: Lens.Lens' UpdateResourceShareResponse (Prelude.Maybe Prelude.Text)
updateResourceShareResponse_clientToken = Lens.lens (\UpdateResourceShareResponse' {clientToken} -> clientToken) (\s@UpdateResourceShareResponse' {} a -> s {clientToken = a} :: UpdateResourceShareResponse)

-- | Information about the resource share.
updateResourceShareResponse_resourceShare :: Lens.Lens' UpdateResourceShareResponse (Prelude.Maybe ResourceShare)
updateResourceShareResponse_resourceShare = Lens.lens (\UpdateResourceShareResponse' {resourceShare} -> resourceShare) (\s@UpdateResourceShareResponse' {} a -> s {resourceShare = a} :: UpdateResourceShareResponse)

-- | The response's http status code.
updateResourceShareResponse_httpStatus :: Lens.Lens' UpdateResourceShareResponse Prelude.Int
updateResourceShareResponse_httpStatus = Lens.lens (\UpdateResourceShareResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceShareResponse' {} a -> s {httpStatus = a} :: UpdateResourceShareResponse)

instance Prelude.NFData UpdateResourceShareResponse where
  rnf UpdateResourceShareResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceShare
