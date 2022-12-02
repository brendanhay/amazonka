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
-- Module      : Amazonka.IoTWireless.AssociateWirelessGatewayWithCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a wireless gateway with a certificate.
module Amazonka.IoTWireless.AssociateWirelessGatewayWithCertificate
  ( -- * Creating a Request
    AssociateWirelessGatewayWithCertificate (..),
    newAssociateWirelessGatewayWithCertificate,

    -- * Request Lenses
    associateWirelessGatewayWithCertificate_id,
    associateWirelessGatewayWithCertificate_iotCertificateId,

    -- * Destructuring the Response
    AssociateWirelessGatewayWithCertificateResponse (..),
    newAssociateWirelessGatewayWithCertificateResponse,

    -- * Response Lenses
    associateWirelessGatewayWithCertificateResponse_iotCertificateId,
    associateWirelessGatewayWithCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateWirelessGatewayWithCertificate' smart constructor.
data AssociateWirelessGatewayWithCertificate = AssociateWirelessGatewayWithCertificate'
  { -- | The ID of the resource to update.
    id :: Prelude.Text,
    -- | The ID of the certificate to associate with the wireless gateway.
    iotCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessGatewayWithCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateWirelessGatewayWithCertificate_id' - The ID of the resource to update.
--
-- 'iotCertificateId', 'associateWirelessGatewayWithCertificate_iotCertificateId' - The ID of the certificate to associate with the wireless gateway.
newAssociateWirelessGatewayWithCertificate ::
  -- | 'id'
  Prelude.Text ->
  -- | 'iotCertificateId'
  Prelude.Text ->
  AssociateWirelessGatewayWithCertificate
newAssociateWirelessGatewayWithCertificate
  pId_
  pIotCertificateId_ =
    AssociateWirelessGatewayWithCertificate'
      { id = pId_,
        iotCertificateId =
          pIotCertificateId_
      }

-- | The ID of the resource to update.
associateWirelessGatewayWithCertificate_id :: Lens.Lens' AssociateWirelessGatewayWithCertificate Prelude.Text
associateWirelessGatewayWithCertificate_id = Lens.lens (\AssociateWirelessGatewayWithCertificate' {id} -> id) (\s@AssociateWirelessGatewayWithCertificate' {} a -> s {id = a} :: AssociateWirelessGatewayWithCertificate)

-- | The ID of the certificate to associate with the wireless gateway.
associateWirelessGatewayWithCertificate_iotCertificateId :: Lens.Lens' AssociateWirelessGatewayWithCertificate Prelude.Text
associateWirelessGatewayWithCertificate_iotCertificateId = Lens.lens (\AssociateWirelessGatewayWithCertificate' {iotCertificateId} -> iotCertificateId) (\s@AssociateWirelessGatewayWithCertificate' {} a -> s {iotCertificateId = a} :: AssociateWirelessGatewayWithCertificate)

instance
  Core.AWSRequest
    AssociateWirelessGatewayWithCertificate
  where
  type
    AWSResponse
      AssociateWirelessGatewayWithCertificate =
      AssociateWirelessGatewayWithCertificateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateWirelessGatewayWithCertificateResponse'
            Prelude.<$> (x Data..?> "IotCertificateId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWirelessGatewayWithCertificate
  where
  hashWithSalt
    _salt
    AssociateWirelessGatewayWithCertificate' {..} =
      _salt `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` iotCertificateId

instance
  Prelude.NFData
    AssociateWirelessGatewayWithCertificate
  where
  rnf AssociateWirelessGatewayWithCertificate' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf iotCertificateId

instance
  Data.ToHeaders
    AssociateWirelessGatewayWithCertificate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociateWirelessGatewayWithCertificate
  where
  toJSON AssociateWirelessGatewayWithCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IotCertificateId" Data..= iotCertificateId)
          ]
      )

instance
  Data.ToPath
    AssociateWirelessGatewayWithCertificate
  where
  toPath AssociateWirelessGatewayWithCertificate' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Data.toBS id, "/certificate"]

instance
  Data.ToQuery
    AssociateWirelessGatewayWithCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWirelessGatewayWithCertificateResponse' smart constructor.
data AssociateWirelessGatewayWithCertificateResponse = AssociateWirelessGatewayWithCertificateResponse'
  { -- | The ID of the certificate associated with the wireless gateway.
    iotCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessGatewayWithCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotCertificateId', 'associateWirelessGatewayWithCertificateResponse_iotCertificateId' - The ID of the certificate associated with the wireless gateway.
--
-- 'httpStatus', 'associateWirelessGatewayWithCertificateResponse_httpStatus' - The response's http status code.
newAssociateWirelessGatewayWithCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWirelessGatewayWithCertificateResponse
newAssociateWirelessGatewayWithCertificateResponse
  pHttpStatus_ =
    AssociateWirelessGatewayWithCertificateResponse'
      { iotCertificateId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the certificate associated with the wireless gateway.
associateWirelessGatewayWithCertificateResponse_iotCertificateId :: Lens.Lens' AssociateWirelessGatewayWithCertificateResponse (Prelude.Maybe Prelude.Text)
associateWirelessGatewayWithCertificateResponse_iotCertificateId = Lens.lens (\AssociateWirelessGatewayWithCertificateResponse' {iotCertificateId} -> iotCertificateId) (\s@AssociateWirelessGatewayWithCertificateResponse' {} a -> s {iotCertificateId = a} :: AssociateWirelessGatewayWithCertificateResponse)

-- | The response's http status code.
associateWirelessGatewayWithCertificateResponse_httpStatus :: Lens.Lens' AssociateWirelessGatewayWithCertificateResponse Prelude.Int
associateWirelessGatewayWithCertificateResponse_httpStatus = Lens.lens (\AssociateWirelessGatewayWithCertificateResponse' {httpStatus} -> httpStatus) (\s@AssociateWirelessGatewayWithCertificateResponse' {} a -> s {httpStatus = a} :: AssociateWirelessGatewayWithCertificateResponse)

instance
  Prelude.NFData
    AssociateWirelessGatewayWithCertificateResponse
  where
  rnf
    AssociateWirelessGatewayWithCertificateResponse' {..} =
      Prelude.rnf iotCertificateId
        `Prelude.seq` Prelude.rnf httpStatus
