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
-- Module      : Amazonka.StorageGateway.DeleteTapePool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a custom tape pool. A custom tape pool can only be deleted if
-- there are no tapes in the pool and if there are no automatic tape
-- creation policies that reference the custom tape pool.
module Amazonka.StorageGateway.DeleteTapePool
  ( -- * Creating a Request
    DeleteTapePool (..),
    newDeleteTapePool,

    -- * Request Lenses
    deleteTapePool_poolARN,

    -- * Destructuring the Response
    DeleteTapePoolResponse (..),
    newDeleteTapePoolResponse,

    -- * Response Lenses
    deleteTapePoolResponse_poolARN,
    deleteTapePoolResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDeleteTapePool' smart constructor.
data DeleteTapePool = DeleteTapePool'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
    poolARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTapePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'deleteTapePool_poolARN' - The Amazon Resource Name (ARN) of the custom tape pool to delete.
newDeleteTapePool ::
  -- | 'poolARN'
  Prelude.Text ->
  DeleteTapePool
newDeleteTapePool pPoolARN_ =
  DeleteTapePool' {poolARN = pPoolARN_}

-- | The Amazon Resource Name (ARN) of the custom tape pool to delete.
deleteTapePool_poolARN :: Lens.Lens' DeleteTapePool Prelude.Text
deleteTapePool_poolARN = Lens.lens (\DeleteTapePool' {poolARN} -> poolARN) (\s@DeleteTapePool' {} a -> s {poolARN = a} :: DeleteTapePool)

instance Core.AWSRequest DeleteTapePool where
  type
    AWSResponse DeleteTapePool =
      DeleteTapePoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTapePoolResponse'
            Prelude.<$> (x Data..?> "PoolARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTapePool where
  hashWithSalt _salt DeleteTapePool' {..} =
    _salt `Prelude.hashWithSalt` poolARN

instance Prelude.NFData DeleteTapePool where
  rnf DeleteTapePool' {..} = Prelude.rnf poolARN

instance Data.ToHeaders DeleteTapePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DeleteTapePool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTapePool where
  toJSON DeleteTapePool' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PoolARN" Data..= poolARN)]
      )

instance Data.ToPath DeleteTapePool where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTapePool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTapePoolResponse' smart constructor.
data DeleteTapePoolResponse = DeleteTapePoolResponse'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
    poolARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTapePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'deleteTapePoolResponse_poolARN' - The Amazon Resource Name (ARN) of the custom tape pool being deleted.
--
-- 'httpStatus', 'deleteTapePoolResponse_httpStatus' - The response's http status code.
newDeleteTapePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTapePoolResponse
newDeleteTapePoolResponse pHttpStatus_ =
  DeleteTapePoolResponse'
    { poolARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool being deleted.
deleteTapePoolResponse_poolARN :: Lens.Lens' DeleteTapePoolResponse (Prelude.Maybe Prelude.Text)
deleteTapePoolResponse_poolARN = Lens.lens (\DeleteTapePoolResponse' {poolARN} -> poolARN) (\s@DeleteTapePoolResponse' {} a -> s {poolARN = a} :: DeleteTapePoolResponse)

-- | The response's http status code.
deleteTapePoolResponse_httpStatus :: Lens.Lens' DeleteTapePoolResponse Prelude.Int
deleteTapePoolResponse_httpStatus = Lens.lens (\DeleteTapePoolResponse' {httpStatus} -> httpStatus) (\s@DeleteTapePoolResponse' {} a -> s {httpStatus = a} :: DeleteTapePoolResponse)

instance Prelude.NFData DeleteTapePoolResponse where
  rnf DeleteTapePoolResponse' {..} =
    Prelude.rnf poolARN
      `Prelude.seq` Prelude.rnf httpStatus
