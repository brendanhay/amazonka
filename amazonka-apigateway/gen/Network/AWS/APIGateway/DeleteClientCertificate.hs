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
-- Module      : Network.AWS.APIGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ClientCertificate resource.
module Network.AWS.APIGateway.DeleteClientCertificate
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the ClientCertificate resource.
--
-- /See:/ 'newDeleteClientCertificate' smart constructor.
data DeleteClientCertificate = DeleteClientCertificate'
  { -- | [Required] The identifier of the ClientCertificate resource to be
    -- deleted.
    clientCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertificateId', 'deleteClientCertificate_clientCertificateId' - [Required] The identifier of the ClientCertificate resource to be
-- deleted.
newDeleteClientCertificate ::
  -- | 'clientCertificateId'
  Prelude.Text ->
  DeleteClientCertificate
newDeleteClientCertificate pClientCertificateId_ =
  DeleteClientCertificate'
    { clientCertificateId =
        pClientCertificateId_
    }

-- | [Required] The identifier of the ClientCertificate resource to be
-- deleted.
deleteClientCertificate_clientCertificateId :: Lens.Lens' DeleteClientCertificate Prelude.Text
deleteClientCertificate_clientCertificateId = Lens.lens (\DeleteClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@DeleteClientCertificate' {} a -> s {clientCertificateId = a} :: DeleteClientCertificate)

instance Prelude.AWSRequest DeleteClientCertificate where
  type
    Rs DeleteClientCertificate =
      DeleteClientCertificateResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteClientCertificateResponse'

instance Prelude.Hashable DeleteClientCertificate

instance Prelude.NFData DeleteClientCertificate

instance Prelude.ToHeaders DeleteClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteClientCertificate where
  toPath DeleteClientCertificate' {..} =
    Prelude.mconcat
      [ "/clientcertificates/",
        Prelude.toBS clientCertificateId
      ]

instance Prelude.ToQuery DeleteClientCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse = DeleteClientCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
