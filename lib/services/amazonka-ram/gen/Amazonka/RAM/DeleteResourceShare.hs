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
-- Module      : Amazonka.RAM.DeleteResourceShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource share.
module Amazonka.RAM.DeleteResourceShare
  ( -- * Creating a Request
    DeleteResourceShare (..),
    newDeleteResourceShare,

    -- * Request Lenses
    deleteResourceShare_clientToken,
    deleteResourceShare_resourceShareArn,

    -- * Destructuring the Response
    DeleteResourceShareResponse (..),
    newDeleteResourceShareResponse,

    -- * Response Lenses
    deleteResourceShareResponse_clientToken,
    deleteResourceShareResponse_returnValue,
    deleteResourceShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceShare' smart constructor.
data DeleteResourceShare = DeleteResourceShare'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource share.
    resourceShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteResourceShare_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'resourceShareArn', 'deleteResourceShare_resourceShareArn' - The Amazon Resource Name (ARN) of the resource share.
newDeleteResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  DeleteResourceShare
newDeleteResourceShare pResourceShareArn_ =
  DeleteResourceShare'
    { clientToken = Prelude.Nothing,
      resourceShareArn = pResourceShareArn_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
deleteResourceShare_clientToken :: Lens.Lens' DeleteResourceShare (Prelude.Maybe Prelude.Text)
deleteResourceShare_clientToken = Lens.lens (\DeleteResourceShare' {clientToken} -> clientToken) (\s@DeleteResourceShare' {} a -> s {clientToken = a} :: DeleteResourceShare)

-- | The Amazon Resource Name (ARN) of the resource share.
deleteResourceShare_resourceShareArn :: Lens.Lens' DeleteResourceShare Prelude.Text
deleteResourceShare_resourceShareArn = Lens.lens (\DeleteResourceShare' {resourceShareArn} -> resourceShareArn) (\s@DeleteResourceShare' {} a -> s {resourceShareArn = a} :: DeleteResourceShare)

instance Core.AWSRequest DeleteResourceShare where
  type
    AWSResponse DeleteResourceShare =
      DeleteResourceShareResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourceShareResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourceShare where
  hashWithSalt _salt DeleteResourceShare' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData DeleteResourceShare where
  rnf DeleteResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Core.ToHeaders DeleteResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteResourceShare where
  toPath = Prelude.const "/deleteresourceshare"

instance Core.ToQuery DeleteResourceShare where
  toQuery DeleteResourceShare' {..} =
    Prelude.mconcat
      [ "clientToken" Core.=: clientToken,
        "resourceShareArn" Core.=: resourceShareArn
      ]

-- | /See:/ 'newDeleteResourceShareResponse' smart constructor.
data DeleteResourceShareResponse = DeleteResourceShareResponse'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the request succeeded.
    returnValue :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteResourceShareResponse_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'returnValue', 'deleteResourceShareResponse_returnValue' - Indicates whether the request succeeded.
--
-- 'httpStatus', 'deleteResourceShareResponse_httpStatus' - The response's http status code.
newDeleteResourceShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourceShareResponse
newDeleteResourceShareResponse pHttpStatus_ =
  DeleteResourceShareResponse'
    { clientToken =
        Prelude.Nothing,
      returnValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
deleteResourceShareResponse_clientToken :: Lens.Lens' DeleteResourceShareResponse (Prelude.Maybe Prelude.Text)
deleteResourceShareResponse_clientToken = Lens.lens (\DeleteResourceShareResponse' {clientToken} -> clientToken) (\s@DeleteResourceShareResponse' {} a -> s {clientToken = a} :: DeleteResourceShareResponse)

-- | Indicates whether the request succeeded.
deleteResourceShareResponse_returnValue :: Lens.Lens' DeleteResourceShareResponse (Prelude.Maybe Prelude.Bool)
deleteResourceShareResponse_returnValue = Lens.lens (\DeleteResourceShareResponse' {returnValue} -> returnValue) (\s@DeleteResourceShareResponse' {} a -> s {returnValue = a} :: DeleteResourceShareResponse)

-- | The response's http status code.
deleteResourceShareResponse_httpStatus :: Lens.Lens' DeleteResourceShareResponse Prelude.Int
deleteResourceShareResponse_httpStatus = Lens.lens (\DeleteResourceShareResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceShareResponse' {} a -> s {httpStatus = a} :: DeleteResourceShareResponse)

instance Prelude.NFData DeleteResourceShareResponse where
  rnf DeleteResourceShareResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf returnValue
      `Prelude.seq` Prelude.rnf httpStatus
