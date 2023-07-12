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
-- Module      : Amazonka.WorkSpacesWeb.GetTrustStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the trust store.
module Amazonka.WorkSpacesWeb.GetTrustStore
  ( -- * Creating a Request
    GetTrustStore (..),
    newGetTrustStore,

    -- * Request Lenses
    getTrustStore_trustStoreArn,

    -- * Destructuring the Response
    GetTrustStoreResponse (..),
    newGetTrustStoreResponse,

    -- * Response Lenses
    getTrustStoreResponse_trustStore,
    getTrustStoreResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetTrustStore' smart constructor.
data GetTrustStore = GetTrustStore'
  { -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrustStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStoreArn', 'getTrustStore_trustStoreArn' - The ARN of the trust store.
newGetTrustStore ::
  -- | 'trustStoreArn'
  Prelude.Text ->
  GetTrustStore
newGetTrustStore pTrustStoreArn_ =
  GetTrustStore' {trustStoreArn = pTrustStoreArn_}

-- | The ARN of the trust store.
getTrustStore_trustStoreArn :: Lens.Lens' GetTrustStore Prelude.Text
getTrustStore_trustStoreArn = Lens.lens (\GetTrustStore' {trustStoreArn} -> trustStoreArn) (\s@GetTrustStore' {} a -> s {trustStoreArn = a} :: GetTrustStore)

instance Core.AWSRequest GetTrustStore where
  type
    AWSResponse GetTrustStore =
      GetTrustStoreResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrustStoreResponse'
            Prelude.<$> (x Data..?> "trustStore")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTrustStore where
  hashWithSalt _salt GetTrustStore' {..} =
    _salt `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData GetTrustStore where
  rnf GetTrustStore' {..} = Prelude.rnf trustStoreArn

instance Data.ToHeaders GetTrustStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTrustStore where
  toPath GetTrustStore' {..} =
    Prelude.mconcat
      ["/trustStores/", Data.toBS trustStoreArn]

instance Data.ToQuery GetTrustStore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTrustStoreResponse' smart constructor.
data GetTrustStoreResponse = GetTrustStoreResponse'
  { -- | The trust store.
    trustStore :: Prelude.Maybe TrustStore,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTrustStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStore', 'getTrustStoreResponse_trustStore' - The trust store.
--
-- 'httpStatus', 'getTrustStoreResponse_httpStatus' - The response's http status code.
newGetTrustStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTrustStoreResponse
newGetTrustStoreResponse pHttpStatus_ =
  GetTrustStoreResponse'
    { trustStore =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The trust store.
getTrustStoreResponse_trustStore :: Lens.Lens' GetTrustStoreResponse (Prelude.Maybe TrustStore)
getTrustStoreResponse_trustStore = Lens.lens (\GetTrustStoreResponse' {trustStore} -> trustStore) (\s@GetTrustStoreResponse' {} a -> s {trustStore = a} :: GetTrustStoreResponse)

-- | The response's http status code.
getTrustStoreResponse_httpStatus :: Lens.Lens' GetTrustStoreResponse Prelude.Int
getTrustStoreResponse_httpStatus = Lens.lens (\GetTrustStoreResponse' {httpStatus} -> httpStatus) (\s@GetTrustStoreResponse' {} a -> s {httpStatus = a} :: GetTrustStoreResponse)

instance Prelude.NFData GetTrustStoreResponse where
  rnf GetTrustStoreResponse' {..} =
    Prelude.rnf trustStore
      `Prelude.seq` Prelude.rnf httpStatus
