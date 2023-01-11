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
-- Module      : Amazonka.WorkSpacesWeb.DeleteTrustStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the trust store.
module Amazonka.WorkSpacesWeb.DeleteTrustStore
  ( -- * Creating a Request
    DeleteTrustStore (..),
    newDeleteTrustStore,

    -- * Request Lenses
    deleteTrustStore_trustStoreArn,

    -- * Destructuring the Response
    DeleteTrustStoreResponse (..),
    newDeleteTrustStoreResponse,

    -- * Response Lenses
    deleteTrustStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteTrustStore' smart constructor.
data DeleteTrustStore = DeleteTrustStore'
  { -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrustStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStoreArn', 'deleteTrustStore_trustStoreArn' - The ARN of the trust store.
newDeleteTrustStore ::
  -- | 'trustStoreArn'
  Prelude.Text ->
  DeleteTrustStore
newDeleteTrustStore pTrustStoreArn_ =
  DeleteTrustStore' {trustStoreArn = pTrustStoreArn_}

-- | The ARN of the trust store.
deleteTrustStore_trustStoreArn :: Lens.Lens' DeleteTrustStore Prelude.Text
deleteTrustStore_trustStoreArn = Lens.lens (\DeleteTrustStore' {trustStoreArn} -> trustStoreArn) (\s@DeleteTrustStore' {} a -> s {trustStoreArn = a} :: DeleteTrustStore)

instance Core.AWSRequest DeleteTrustStore where
  type
    AWSResponse DeleteTrustStore =
      DeleteTrustStoreResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrustStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrustStore where
  hashWithSalt _salt DeleteTrustStore' {..} =
    _salt `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData DeleteTrustStore where
  rnf DeleteTrustStore' {..} = Prelude.rnf trustStoreArn

instance Data.ToHeaders DeleteTrustStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTrustStore where
  toPath DeleteTrustStore' {..} =
    Prelude.mconcat
      ["/trustStores/", Data.toBS trustStoreArn]

instance Data.ToQuery DeleteTrustStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrustStoreResponse' smart constructor.
data DeleteTrustStoreResponse = DeleteTrustStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrustStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrustStoreResponse_httpStatus' - The response's http status code.
newDeleteTrustStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrustStoreResponse
newDeleteTrustStoreResponse pHttpStatus_ =
  DeleteTrustStoreResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTrustStoreResponse_httpStatus :: Lens.Lens' DeleteTrustStoreResponse Prelude.Int
deleteTrustStoreResponse_httpStatus = Lens.lens (\DeleteTrustStoreResponse' {httpStatus} -> httpStatus) (\s@DeleteTrustStoreResponse' {} a -> s {httpStatus = a} :: DeleteTrustStoreResponse)

instance Prelude.NFData DeleteTrustStoreResponse where
  rnf DeleteTrustStoreResponse' {..} =
    Prelude.rnf httpStatus
