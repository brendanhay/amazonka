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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource share. This doesn\'t delete any of the
-- resources that were associated with the resource share; it only stops
-- the sharing of those resources outside of the Amazon Web Services
-- account that created them.
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceShare' smart constructor.
data DeleteResourceShare = DeleteResourceShare'
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
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource share to delete.
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
-- 'clientToken', 'deleteResourceShare_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'resourceShareArn', 'deleteResourceShare_resourceShareArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share to delete.
newDeleteResourceShare ::
  -- | 'resourceShareArn'
  Prelude.Text ->
  DeleteResourceShare
newDeleteResourceShare pResourceShareArn_ =
  DeleteResourceShare'
    { clientToken = Prelude.Nothing,
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
deleteResourceShare_clientToken :: Lens.Lens' DeleteResourceShare (Prelude.Maybe Prelude.Text)
deleteResourceShare_clientToken = Lens.lens (\DeleteResourceShare' {clientToken} -> clientToken) (\s@DeleteResourceShare' {} a -> s {clientToken = a} :: DeleteResourceShare)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource share to delete.
deleteResourceShare_resourceShareArn :: Lens.Lens' DeleteResourceShare Prelude.Text
deleteResourceShare_resourceShareArn = Lens.lens (\DeleteResourceShare' {resourceShareArn} -> resourceShareArn) (\s@DeleteResourceShare' {} a -> s {resourceShareArn = a} :: DeleteResourceShare)

instance Core.AWSRequest DeleteResourceShare where
  type
    AWSResponse DeleteResourceShare =
      DeleteResourceShareResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourceShareResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "returnValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourceShare where
  hashWithSalt _salt DeleteResourceShare' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceShareArn

instance Prelude.NFData DeleteResourceShare where
  rnf DeleteResourceShare' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareArn

instance Data.ToHeaders DeleteResourceShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourceShare where
  toPath = Prelude.const "/deleteresourceshare"

instance Data.ToQuery DeleteResourceShare where
  toQuery DeleteResourceShare' {..} =
    Prelude.mconcat
      [ "clientToken" Data.=: clientToken,
        "resourceShareArn" Data.=: resourceShareArn
      ]

-- | /See:/ 'newDeleteResourceShareResponse' smart constructor.
data DeleteResourceShareResponse = DeleteResourceShareResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A return value of @true@ indicates that the request succeeded. A value
    -- of @false@ indicates that the request failed.
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
-- 'clientToken', 'deleteResourceShareResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'returnValue', 'deleteResourceShareResponse_returnValue' - A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
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

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
deleteResourceShareResponse_clientToken :: Lens.Lens' DeleteResourceShareResponse (Prelude.Maybe Prelude.Text)
deleteResourceShareResponse_clientToken = Lens.lens (\DeleteResourceShareResponse' {clientToken} -> clientToken) (\s@DeleteResourceShareResponse' {} a -> s {clientToken = a} :: DeleteResourceShareResponse)

-- | A return value of @true@ indicates that the request succeeded. A value
-- of @false@ indicates that the request failed.
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
