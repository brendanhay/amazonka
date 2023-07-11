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
-- Module      : Amazonka.IoTWireless.GetWirelessGatewayCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the ID of the certificate that is currently associated with a
-- wireless gateway.
module Amazonka.IoTWireless.GetWirelessGatewayCertificate
  ( -- * Creating a Request
    GetWirelessGatewayCertificate (..),
    newGetWirelessGatewayCertificate,

    -- * Request Lenses
    getWirelessGatewayCertificate_id,

    -- * Destructuring the Response
    GetWirelessGatewayCertificateResponse (..),
    newGetWirelessGatewayCertificateResponse,

    -- * Response Lenses
    getWirelessGatewayCertificateResponse_iotCertificateId,
    getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId,
    getWirelessGatewayCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWirelessGatewayCertificate' smart constructor.
data GetWirelessGatewayCertificate = GetWirelessGatewayCertificate'
  { -- | The ID of the resource to get.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWirelessGatewayCertificate_id' - The ID of the resource to get.
newGetWirelessGatewayCertificate ::
  -- | 'id'
  Prelude.Text ->
  GetWirelessGatewayCertificate
newGetWirelessGatewayCertificate pId_ =
  GetWirelessGatewayCertificate' {id = pId_}

-- | The ID of the resource to get.
getWirelessGatewayCertificate_id :: Lens.Lens' GetWirelessGatewayCertificate Prelude.Text
getWirelessGatewayCertificate_id = Lens.lens (\GetWirelessGatewayCertificate' {id} -> id) (\s@GetWirelessGatewayCertificate' {} a -> s {id = a} :: GetWirelessGatewayCertificate)

instance
  Core.AWSRequest
    GetWirelessGatewayCertificate
  where
  type
    AWSResponse GetWirelessGatewayCertificate =
      GetWirelessGatewayCertificateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessGatewayCertificateResponse'
            Prelude.<$> (x Data..?> "IotCertificateId")
            Prelude.<*> (x Data..?> "LoRaWANNetworkServerCertificateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetWirelessGatewayCertificate
  where
  hashWithSalt _salt GetWirelessGatewayCertificate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWirelessGatewayCertificate where
  rnf GetWirelessGatewayCertificate' {..} =
    Prelude.rnf id

instance Data.ToHeaders GetWirelessGatewayCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetWirelessGatewayCertificate where
  toPath GetWirelessGatewayCertificate' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Data.toBS id, "/certificate"]

instance Data.ToQuery GetWirelessGatewayCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWirelessGatewayCertificateResponse' smart constructor.
data GetWirelessGatewayCertificateResponse = GetWirelessGatewayCertificateResponse'
  { -- | The ID of the certificate associated with the wireless gateway.
    iotCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate that is associated with the wireless gateway
    -- and used for the LoRaWANNetworkServer endpoint.
    loRaWANNetworkServerCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotCertificateId', 'getWirelessGatewayCertificateResponse_iotCertificateId' - The ID of the certificate associated with the wireless gateway.
--
-- 'loRaWANNetworkServerCertificateId', 'getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId' - The ID of the certificate that is associated with the wireless gateway
-- and used for the LoRaWANNetworkServer endpoint.
--
-- 'httpStatus', 'getWirelessGatewayCertificateResponse_httpStatus' - The response's http status code.
newGetWirelessGatewayCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessGatewayCertificateResponse
newGetWirelessGatewayCertificateResponse pHttpStatus_ =
  GetWirelessGatewayCertificateResponse'
    { iotCertificateId =
        Prelude.Nothing,
      loRaWANNetworkServerCertificateId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the certificate associated with the wireless gateway.
getWirelessGatewayCertificateResponse_iotCertificateId :: Lens.Lens' GetWirelessGatewayCertificateResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayCertificateResponse_iotCertificateId = Lens.lens (\GetWirelessGatewayCertificateResponse' {iotCertificateId} -> iotCertificateId) (\s@GetWirelessGatewayCertificateResponse' {} a -> s {iotCertificateId = a} :: GetWirelessGatewayCertificateResponse)

-- | The ID of the certificate that is associated with the wireless gateway
-- and used for the LoRaWANNetworkServer endpoint.
getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId :: Lens.Lens' GetWirelessGatewayCertificateResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayCertificateResponse_loRaWANNetworkServerCertificateId = Lens.lens (\GetWirelessGatewayCertificateResponse' {loRaWANNetworkServerCertificateId} -> loRaWANNetworkServerCertificateId) (\s@GetWirelessGatewayCertificateResponse' {} a -> s {loRaWANNetworkServerCertificateId = a} :: GetWirelessGatewayCertificateResponse)

-- | The response's http status code.
getWirelessGatewayCertificateResponse_httpStatus :: Lens.Lens' GetWirelessGatewayCertificateResponse Prelude.Int
getWirelessGatewayCertificateResponse_httpStatus = Lens.lens (\GetWirelessGatewayCertificateResponse' {httpStatus} -> httpStatus) (\s@GetWirelessGatewayCertificateResponse' {} a -> s {httpStatus = a} :: GetWirelessGatewayCertificateResponse)

instance
  Prelude.NFData
    GetWirelessGatewayCertificateResponse
  where
  rnf GetWirelessGatewayCertificateResponse' {..} =
    Prelude.rnf iotCertificateId
      `Prelude.seq` Prelude.rnf loRaWANNetworkServerCertificateId
      `Prelude.seq` Prelude.rnf httpStatus
