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
-- Module      : Network.AWS.APIGateway.GetClientCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current ClientCertificate resource.
module Network.AWS.APIGateway.GetClientCertificate
  ( -- * Creating a Request
    GetClientCertificate (..),
    newGetClientCertificate,

    -- * Request Lenses
    getClientCertificate_clientCertificateId,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about the current ClientCertificate
-- resource.
--
-- /See:/ 'newGetClientCertificate' smart constructor.
data GetClientCertificate = GetClientCertificate'
  { -- | [Required] The identifier of the ClientCertificate resource to be
    -- described.
    clientCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClientCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientCertificateId', 'getClientCertificate_clientCertificateId' - [Required] The identifier of the ClientCertificate resource to be
-- described.
newGetClientCertificate ::
  -- | 'clientCertificateId'
  Prelude.Text ->
  GetClientCertificate
newGetClientCertificate pClientCertificateId_ =
  GetClientCertificate'
    { clientCertificateId =
        pClientCertificateId_
    }

-- | [Required] The identifier of the ClientCertificate resource to be
-- described.
getClientCertificate_clientCertificateId :: Lens.Lens' GetClientCertificate Prelude.Text
getClientCertificate_clientCertificateId = Lens.lens (\GetClientCertificate' {clientCertificateId} -> clientCertificateId) (\s@GetClientCertificate' {} a -> s {clientCertificateId = a} :: GetClientCertificate)

instance Core.AWSRequest GetClientCertificate where
  type
    AWSResponse GetClientCertificate =
      ClientCertificate
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetClientCertificate

instance Prelude.NFData GetClientCertificate

instance Core.ToHeaders GetClientCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetClientCertificate where
  toPath GetClientCertificate' {..} =
    Prelude.mconcat
      [ "/clientcertificates/",
        Core.toBS clientCertificateId
      ]

instance Core.ToQuery GetClientCertificate where
  toQuery = Prelude.const Prelude.mempty
