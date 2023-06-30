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
-- Module      : Amazonka.WorkSpacesWeb.AssociateTrustStore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a trust store with a web portal.
module Amazonka.WorkSpacesWeb.AssociateTrustStore
  ( -- * Creating a Request
    AssociateTrustStore (..),
    newAssociateTrustStore,

    -- * Request Lenses
    associateTrustStore_portalArn,
    associateTrustStore_trustStoreArn,

    -- * Destructuring the Response
    AssociateTrustStoreResponse (..),
    newAssociateTrustStoreResponse,

    -- * Response Lenses
    associateTrustStoreResponse_httpStatus,
    associateTrustStoreResponse_portalArn,
    associateTrustStoreResponse_trustStoreArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newAssociateTrustStore' smart constructor.
data AssociateTrustStore = AssociateTrustStore'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrustStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'associateTrustStore_portalArn' - The ARN of the web portal.
--
-- 'trustStoreArn', 'associateTrustStore_trustStoreArn' - The ARN of the trust store.
newAssociateTrustStore ::
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'trustStoreArn'
  Prelude.Text ->
  AssociateTrustStore
newAssociateTrustStore pPortalArn_ pTrustStoreArn_ =
  AssociateTrustStore'
    { portalArn = pPortalArn_,
      trustStoreArn = pTrustStoreArn_
    }

-- | The ARN of the web portal.
associateTrustStore_portalArn :: Lens.Lens' AssociateTrustStore Prelude.Text
associateTrustStore_portalArn = Lens.lens (\AssociateTrustStore' {portalArn} -> portalArn) (\s@AssociateTrustStore' {} a -> s {portalArn = a} :: AssociateTrustStore)

-- | The ARN of the trust store.
associateTrustStore_trustStoreArn :: Lens.Lens' AssociateTrustStore Prelude.Text
associateTrustStore_trustStoreArn = Lens.lens (\AssociateTrustStore' {trustStoreArn} -> trustStoreArn) (\s@AssociateTrustStore' {} a -> s {trustStoreArn = a} :: AssociateTrustStore)

instance Core.AWSRequest AssociateTrustStore where
  type
    AWSResponse AssociateTrustStore =
      AssociateTrustStoreResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateTrustStoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "portalArn")
            Prelude.<*> (x Data..:> "trustStoreArn")
      )

instance Prelude.Hashable AssociateTrustStore where
  hashWithSalt _salt AssociateTrustStore' {..} =
    _salt
      `Prelude.hashWithSalt` portalArn
      `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData AssociateTrustStore where
  rnf AssociateTrustStore' {..} =
    Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf trustStoreArn

instance Data.ToHeaders AssociateTrustStore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateTrustStore where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateTrustStore where
  toPath AssociateTrustStore' {..} =
    Prelude.mconcat
      ["/portals/", Data.toBS portalArn, "/trustStores"]

instance Data.ToQuery AssociateTrustStore where
  toQuery AssociateTrustStore' {..} =
    Prelude.mconcat
      ["trustStoreArn" Data.=: trustStoreArn]

-- | /See:/ 'newAssociateTrustStoreResponse' smart constructor.
data AssociateTrustStoreResponse = AssociateTrustStoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text,
    -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateTrustStoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateTrustStoreResponse_httpStatus' - The response's http status code.
--
-- 'portalArn', 'associateTrustStoreResponse_portalArn' - The ARN of the web portal.
--
-- 'trustStoreArn', 'associateTrustStoreResponse_trustStoreArn' - The ARN of the trust store.
newAssociateTrustStoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'portalArn'
  Prelude.Text ->
  -- | 'trustStoreArn'
  Prelude.Text ->
  AssociateTrustStoreResponse
newAssociateTrustStoreResponse
  pHttpStatus_
  pPortalArn_
  pTrustStoreArn_ =
    AssociateTrustStoreResponse'
      { httpStatus =
          pHttpStatus_,
        portalArn = pPortalArn_,
        trustStoreArn = pTrustStoreArn_
      }

-- | The response's http status code.
associateTrustStoreResponse_httpStatus :: Lens.Lens' AssociateTrustStoreResponse Prelude.Int
associateTrustStoreResponse_httpStatus = Lens.lens (\AssociateTrustStoreResponse' {httpStatus} -> httpStatus) (\s@AssociateTrustStoreResponse' {} a -> s {httpStatus = a} :: AssociateTrustStoreResponse)

-- | The ARN of the web portal.
associateTrustStoreResponse_portalArn :: Lens.Lens' AssociateTrustStoreResponse Prelude.Text
associateTrustStoreResponse_portalArn = Lens.lens (\AssociateTrustStoreResponse' {portalArn} -> portalArn) (\s@AssociateTrustStoreResponse' {} a -> s {portalArn = a} :: AssociateTrustStoreResponse)

-- | The ARN of the trust store.
associateTrustStoreResponse_trustStoreArn :: Lens.Lens' AssociateTrustStoreResponse Prelude.Text
associateTrustStoreResponse_trustStoreArn = Lens.lens (\AssociateTrustStoreResponse' {trustStoreArn} -> trustStoreArn) (\s@AssociateTrustStoreResponse' {} a -> s {trustStoreArn = a} :: AssociateTrustStoreResponse)

instance Prelude.NFData AssociateTrustStoreResponse where
  rnf AssociateTrustStoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf portalArn
      `Prelude.seq` Prelude.rnf trustStoreArn
