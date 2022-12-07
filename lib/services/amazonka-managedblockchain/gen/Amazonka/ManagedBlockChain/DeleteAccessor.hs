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
-- Module      : Amazonka.ManagedBlockChain.DeleteAccessor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The token based access feature is in preview release for Ethereum on
-- Amazon Managed Blockchain and is subject to change. We recommend that
-- you use this feature only with test scenarios, and not in production
-- environments.
--
-- Deletes an accessor that your Amazon Web Services account owns. An
-- accessor object is a container that has the information required for
-- token based access to your Ethereum nodes including, the
-- @BILLING_TOKEN@. After an accessor is deleted, the status of the
-- accessor changes from @AVAILABLE@ to @PENDING_DELETION@. An accessor in
-- the @PENDING_DELETION@ state can’t be used for new WebSocket requests or
-- HTTP requests. However, WebSocket connections that were initiated while
-- the accessor was in the @AVAILABLE@ state remain open until they expire
-- (up to 2 hours).
module Amazonka.ManagedBlockChain.DeleteAccessor
  ( -- * Creating a Request
    DeleteAccessor (..),
    newDeleteAccessor,

    -- * Request Lenses
    deleteAccessor_accessorId,

    -- * Destructuring the Response
    DeleteAccessorResponse (..),
    newDeleteAccessorResponse,

    -- * Response Lenses
    deleteAccessorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAccessor' smart constructor.
data DeleteAccessor = DeleteAccessor'
  { -- | The unique identifier of the accessor.
    accessorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessorId', 'deleteAccessor_accessorId' - The unique identifier of the accessor.
newDeleteAccessor ::
  -- | 'accessorId'
  Prelude.Text ->
  DeleteAccessor
newDeleteAccessor pAccessorId_ =
  DeleteAccessor' {accessorId = pAccessorId_}

-- | The unique identifier of the accessor.
deleteAccessor_accessorId :: Lens.Lens' DeleteAccessor Prelude.Text
deleteAccessor_accessorId = Lens.lens (\DeleteAccessor' {accessorId} -> accessorId) (\s@DeleteAccessor' {} a -> s {accessorId = a} :: DeleteAccessor)

instance Core.AWSRequest DeleteAccessor where
  type
    AWSResponse DeleteAccessor =
      DeleteAccessorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAccessor where
  hashWithSalt _salt DeleteAccessor' {..} =
    _salt `Prelude.hashWithSalt` accessorId

instance Prelude.NFData DeleteAccessor where
  rnf DeleteAccessor' {..} = Prelude.rnf accessorId

instance Data.ToHeaders DeleteAccessor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAccessor where
  toPath DeleteAccessor' {..} =
    Prelude.mconcat
      ["/accessors/", Data.toBS accessorId]

instance Data.ToQuery DeleteAccessor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAccessorResponse' smart constructor.
data DeleteAccessorResponse = DeleteAccessorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAccessorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAccessorResponse_httpStatus' - The response's http status code.
newDeleteAccessorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAccessorResponse
newDeleteAccessorResponse pHttpStatus_ =
  DeleteAccessorResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAccessorResponse_httpStatus :: Lens.Lens' DeleteAccessorResponse Prelude.Int
deleteAccessorResponse_httpStatus = Lens.lens (\DeleteAccessorResponse' {httpStatus} -> httpStatus) (\s@DeleteAccessorResponse' {} a -> s {httpStatus = a} :: DeleteAccessorResponse)

instance Prelude.NFData DeleteAccessorResponse where
  rnf DeleteAccessorResponse' {..} =
    Prelude.rnf httpStatus
