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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about an ClientCertificate resource.
--
-- /See:/ 'newUpdateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The identifier of the ClientCertificate resource to be
    -- updated.
    clientCertificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateClientCertificate
newUpdateClientCertificate pClientCertificateId_ =
  UpdateClientCertificate'
    { patchOperations =
        Core.Nothing,
      clientCertificateId = pClientCertificateId_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateClientCertificate_patchOperations :: Lens.Lens' UpdateClientCertificate (Core.Maybe [PatchOperation])
updateClientCertificate_patchOperations = Lens.lens (\UpdateClientCertificate' {patchOperations} -> patchOperations) (\s@UpdateClientCertificate' {} a -> s {patchOperations = a} :: UpdateClientCertificate) Core.. Lens.mapping Lens._Coerce

-- | [Required] The identifier of the ClientCertificate resource to be
-- updated.
updateClientCertificate_clientCertificateId :: Lens.Lens' UpdateClientCertificate Core.Text
updateClientCertificate_clientCertificateId = Lens.lens (\UpdateClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@UpdateClientCertificate' {} a -> s {clientCertificateId = a} :: UpdateClientCertificate)

instance Core.AWSRequest UpdateClientCertificate where
  type
    AWSResponse UpdateClientCertificate =
      ClientCertificate
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateClientCertificate

instance Core.NFData UpdateClientCertificate

instance Core.ToHeaders UpdateClientCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateClientCertificate where
  toJSON UpdateClientCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateClientCertificate where
  toPath UpdateClientCertificate' {..} =
    Core.mconcat
      [ "/clientcertificates/",
        Core.toBS clientCertificateId
      ]

instance Core.ToQuery UpdateClientCertificate where
  toQuery = Core.const Core.mempty
