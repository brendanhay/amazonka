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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies some of the properties of the specified resource share.
module Amazonka.RAM.UpdateResourceShare
  ( -- * Creating a Request
    UpdateResourceShare (..),
    newUpdateResourceShare,

    -- * Request Lenses
    updateResourceShare_name,
    updateResourceShare_clientToken,
    updateResourceShare_allowExternalPrincipals,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceShare' smart constructor.
data UpdateResourceShare = UpdateResourceShare'
  { -- | If specified, the new name that you want to attach to the resource
    -- share.
    name :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies whether principals outside your organization in Organizations
    -- can be associated with a resource share.
    allowExternalPrincipals :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share that you want to modify.
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
-- 'name', 'updateResourceShare_name' - If specified, the new name that you want to attach to the resource
-- share.
--
-- 'clientToken', 'updateResourceShare_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'allowExternalPrincipals', 'updateResourceShare_allowExternalPrincipals' - Specifies whether principals outside your organization in Organizations
-- can be associated with a resource share.
--
-- 'resourceShareArn', 'updateResourceShare_resourceShareArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to modify.
newUpdateResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  UpdateResourceShare
newUpdateResourceShare pResourceShareArn_ =
  UpdateResourceShare'
    { name = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      allowExternalPrincipals = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
    }

-- | If specified, the new name that you want to attach to the resource
-- share.
updateResourceShare_name :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Text)
updateResourceShare_name = Lens.lens (\UpdateResourceShare' {name} -> name) (\s@UpdateResourceShare' {} a -> s {name = a} :: UpdateResourceShare)

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
updateResourceShare_clientToken :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Text)
updateResourceShare_clientToken = Lens.lens (\UpdateResourceShare' {clientToken} -> clientToken) (\s@UpdateResourceShare' {} a -> s {clientToken = a} :: UpdateResourceShare)

-- | Specifies whether principals outside your organization in Organizations
-- can be associated with a resource share.
updateResourceShare_allowExternalPrincipals :: Lens.Lens' UpdateResourceShare (Prelude.Maybe Prelude.Bool)
updateResourceShare_allowExternalPrincipals = Lens.lens (\UpdateResourceShare' {allowExternalPrincipals} -> allowExternalPrincipals) (\s@UpdateResourceShare' {} a -> s {allowExternalPrincipals = a} :: UpdateResourceShare)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share that you want to modify.
updateResourceShare_resourceShareArn :: Lens.Lens' UpdateResourceShare Prelude.Text
updateResourceShare_resourceShareArn = Lens.lens (\UpdateResourceShare' {resourceShareArn} -> resourceShareArn) (\s@UpdateResourceShare' {} a -> s {resourceShareArn = a} :: UpdateResourceShare)

instance Core.AWSRequest UpdateResourceShare where
  type
    AWSResponse UpdateResourceShare =
      UpdateResourceShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceShareResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "resourceShare")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceShare where
  hashWithSalt _salt UpdateResourceShare' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` allowExternalPrincipals
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData UpdateResourceShare where
  rnf UpdateResourceShare' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf allowExternalPrincipals
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Data.ToHeaders UpdateResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResourceShare where
  toJSON UpdateResourceShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("allowExternalPrincipals" Data..=)
              Prelude.<$> allowExternalPrincipals,
            Prelude.Just
              ("resourceShareArn" Data..= resourceShareArn)
          ]
      )

instance Data.ToPath UpdateResourceShare where
  toPath = Prelude.const "/updateresourceshare"

instance Data.ToQuery UpdateResourceShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceShareResponse' smart constructor.
data UpdateResourceShareResponse = UpdateResourceShareResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
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
-- 'clientToken', 'updateResourceShareResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
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

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
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
      `Prelude.seq` Prelude.rnf resourceShare
      `Prelude.seq` Prelude.rnf httpStatus
