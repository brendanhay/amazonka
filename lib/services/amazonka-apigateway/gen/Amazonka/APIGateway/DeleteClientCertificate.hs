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
-- Module      : Amazonka.APIGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ClientCertificate resource.
module Amazonka.APIGateway.DeleteClientCertificate
  ( -- * Creating a Request
    DeleteClientCertificate (..),
    newDeleteClientCertificate,

    -- * Request Lenses
    deleteClientCertificate_clientCertificateId,

    -- * Destructuring the Response
    DeleteClientCertificateResponse (..),
    newDeleteClientCertificateResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete the ClientCertificate resource.
--
-- /See:/ 'newDeleteClientCertificate' smart constructor.
data DeleteClientCertificate = DeleteClientCertificate'
  { -- | The identifier of the ClientCertificate resource to be deleted.
    clientCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertificateId', 'deleteClientCertificate_clientCertificateId' - The identifier of the ClientCertificate resource to be deleted.
newDeleteClientCertificate ::
  -- | 'clientCertificateId'
  Prelude.Text ->
  DeleteClientCertificate
newDeleteClientCertificate pClientCertificateId_ =
  DeleteClientCertificate'
    { clientCertificateId =
        pClientCertificateId_
    }

-- | The identifier of the ClientCertificate resource to be deleted.
deleteClientCertificate_clientCertificateId :: Lens.Lens' DeleteClientCertificate Prelude.Text
deleteClientCertificate_clientCertificateId = Lens.lens (\DeleteClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@DeleteClientCertificate' {} a -> s {clientCertificateId = a} :: DeleteClientCertificate)

instance Core.AWSRequest DeleteClientCertificate where
  type
    AWSResponse DeleteClientCertificate =
      DeleteClientCertificateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteClientCertificateResponse'

instance Prelude.Hashable DeleteClientCertificate where
  hashWithSalt _salt DeleteClientCertificate' {..} =
    _salt `Prelude.hashWithSalt` clientCertificateId

instance Prelude.NFData DeleteClientCertificate where
  rnf DeleteClientCertificate' {..} =
    Prelude.rnf clientCertificateId

instance Core.ToHeaders DeleteClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteClientCertificate where
  toPath DeleteClientCertificate' {..} =
    Prelude.mconcat
      [ "/clientcertificates/",
        Core.toBS clientCertificateId
      ]

instance Core.ToQuery DeleteClientCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse = DeleteClientCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteClientCertificateResponse ::
  DeleteClientCertificateResponse
newDeleteClientCertificateResponse =
  DeleteClientCertificateResponse'

instance
  Prelude.NFData
    DeleteClientCertificateResponse
  where
  rnf _ = ()
