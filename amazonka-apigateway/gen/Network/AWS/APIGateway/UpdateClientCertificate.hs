{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.UpdateClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an ClientCertificate resource.
module Network.AWS.APIGateway.UpdateClientCertificate
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
    clientCertificate_createdDate,
    clientCertificate_expirationDate,
    clientCertificate_pemEncodedCertificate,
    clientCertificate_tags,
    clientCertificate_clientCertificateId,
    clientCertificate_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about an ClientCertificate resource.
--
-- /See:/ 'newUpdateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The identifier of the ClientCertificate resource to be
    -- updated.
    clientCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateClientCertificate_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'clientCertificateId', 'updateClientCertificate_clientCertificateId' - [Required] The identifier of the ClientCertificate resource to be
-- updated.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateClientCertificate_patchOperations :: Lens.Lens' UpdateClientCertificate (Prelude.Maybe [PatchOperation])
updateClientCertificate_patchOperations = Lens.lens (\UpdateClientCertificate' {patchOperations} -> patchOperations) (\s@UpdateClientCertificate' {} a -> s {patchOperations = a} :: UpdateClientCertificate) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The identifier of the ClientCertificate resource to be
-- updated.
updateClientCertificate_clientCertificateId :: Lens.Lens' UpdateClientCertificate Prelude.Text
updateClientCertificate_clientCertificateId = Lens.lens (\UpdateClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@UpdateClientCertificate' {} a -> s {clientCertificateId = a} :: UpdateClientCertificate)

instance Prelude.AWSRequest UpdateClientCertificate where
  type Rs UpdateClientCertificate = ClientCertificate
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateClientCertificate

instance Prelude.NFData UpdateClientCertificate

instance Prelude.ToHeaders UpdateClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON UpdateClientCertificate where
  toJSON UpdateClientCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("patchOperations" Prelude..=)
              Prelude.<$> patchOperations
          ]
      )

instance Prelude.ToPath UpdateClientCertificate where
  toPath UpdateClientCertificate' {..} =
    Prelude.mconcat
      [ "/clientcertificates/",
        Prelude.toBS clientCertificateId
      ]

instance Prelude.ToQuery UpdateClientCertificate where
  toQuery = Prelude.const Prelude.mempty
