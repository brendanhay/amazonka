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
-- Module      : Amazonka.APIGateway.UpdateClientCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an ClientCertificate resource.
module Amazonka.APIGateway.UpdateClientCertificate
  ( -- * Creating a Request
    UpdateClientCertificate (..),
    newUpdateClientCertificate,

    -- * Request Lenses
    updateClientCertificate_patchOperations,
    updateClientCertificate_clientCertificateId,

    -- * Destructuring the Response
    ClientCertificate (..),
    newClientCertificate,

    -- * Response Lenses
    clientCertificate_clientCertificateId,
    clientCertificate_createdDate,
    clientCertificate_description,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to change information about an ClientCertificate resource.
--
-- /See:/ 'newUpdateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The identifier of the ClientCertificate resource to be updated.
    clientCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateClientCertificate_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'clientCertificateId', 'updateClientCertificate_clientCertificateId' - The identifier of the ClientCertificate resource to be updated.
newUpdateClientCertificate ::
  -- | 'clientCertificateId'
  Prelude.Text ->
  UpdateClientCertificate
newUpdateClientCertificate pClientCertificateId_ =
  UpdateClientCertificate'
    { patchOperations =
        Prelude.Nothing,
      clientCertificateId = pClientCertificateId_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateClientCertificate_patchOperations :: Lens.Lens' UpdateClientCertificate (Prelude.Maybe [PatchOperation])
updateClientCertificate_patchOperations = Lens.lens (\UpdateClientCertificate' {patchOperations} -> patchOperations) (\s@UpdateClientCertificate' {} a -> s {patchOperations = a} :: UpdateClientCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the ClientCertificate resource to be updated.
updateClientCertificate_clientCertificateId :: Lens.Lens' UpdateClientCertificate Prelude.Text
updateClientCertificate_clientCertificateId = Lens.lens (\UpdateClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@UpdateClientCertificate' {} a -> s {clientCertificateId = a} :: UpdateClientCertificate)

instance Core.AWSRequest UpdateClientCertificate where
  type
    AWSResponse UpdateClientCertificate =
      ClientCertificate
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateClientCertificate where
  hashWithSalt _salt UpdateClientCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` clientCertificateId

instance Prelude.NFData UpdateClientCertificate where
  rnf UpdateClientCertificate' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf clientCertificateId

instance Data.ToHeaders UpdateClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateClientCertificate where
  toJSON UpdateClientCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateClientCertificate where
  toPath UpdateClientCertificate' {..} =
    Prelude.mconcat
      [ "/clientcertificates/",
        Data.toBS clientCertificateId
      ]

instance Data.ToQuery UpdateClientCertificate where
  toQuery = Prelude.const Prelude.mempty
