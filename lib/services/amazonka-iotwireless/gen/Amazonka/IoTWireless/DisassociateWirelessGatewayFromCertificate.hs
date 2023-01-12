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
-- Module      : Amazonka.IoTWireless.DisassociateWirelessGatewayFromCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a wireless gateway from its currently associated
-- certificate.
module Amazonka.IoTWireless.DisassociateWirelessGatewayFromCertificate
  ( -- * Creating a Request
    DisassociateWirelessGatewayFromCertificate (..),
    newDisassociateWirelessGatewayFromCertificate,

    -- * Request Lenses
    disassociateWirelessGatewayFromCertificate_id,

    -- * Destructuring the Response
    DisassociateWirelessGatewayFromCertificateResponse (..),
    newDisassociateWirelessGatewayFromCertificateResponse,

    -- * Response Lenses
    disassociateWirelessGatewayFromCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateWirelessGatewayFromCertificate' smart constructor.
data DisassociateWirelessGatewayFromCertificate = DisassociateWirelessGatewayFromCertificate'
  { -- | The ID of the resource to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessGatewayFromCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateWirelessGatewayFromCertificate_id' - The ID of the resource to update.
newDisassociateWirelessGatewayFromCertificate ::
  -- | 'id'
  Prelude.Text ->
  DisassociateWirelessGatewayFromCertificate
newDisassociateWirelessGatewayFromCertificate pId_ =
  DisassociateWirelessGatewayFromCertificate'
    { id =
        pId_
    }

-- | The ID of the resource to update.
disassociateWirelessGatewayFromCertificate_id :: Lens.Lens' DisassociateWirelessGatewayFromCertificate Prelude.Text
disassociateWirelessGatewayFromCertificate_id = Lens.lens (\DisassociateWirelessGatewayFromCertificate' {id} -> id) (\s@DisassociateWirelessGatewayFromCertificate' {} a -> s {id = a} :: DisassociateWirelessGatewayFromCertificate)

instance
  Core.AWSRequest
    DisassociateWirelessGatewayFromCertificate
  where
  type
    AWSResponse
      DisassociateWirelessGatewayFromCertificate =
      DisassociateWirelessGatewayFromCertificateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWirelessGatewayFromCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWirelessGatewayFromCertificate
  where
  hashWithSalt
    _salt
    DisassociateWirelessGatewayFromCertificate' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DisassociateWirelessGatewayFromCertificate
  where
  rnf DisassociateWirelessGatewayFromCertificate' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    DisassociateWirelessGatewayFromCertificate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateWirelessGatewayFromCertificate
  where
  toPath
    DisassociateWirelessGatewayFromCertificate' {..} =
      Prelude.mconcat
        ["/wireless-gateways/", Data.toBS id, "/certificate"]

instance
  Data.ToQuery
    DisassociateWirelessGatewayFromCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWirelessGatewayFromCertificateResponse' smart constructor.
data DisassociateWirelessGatewayFromCertificateResponse = DisassociateWirelessGatewayFromCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessGatewayFromCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWirelessGatewayFromCertificateResponse_httpStatus' - The response's http status code.
newDisassociateWirelessGatewayFromCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWirelessGatewayFromCertificateResponse
newDisassociateWirelessGatewayFromCertificateResponse
  pHttpStatus_ =
    DisassociateWirelessGatewayFromCertificateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWirelessGatewayFromCertificateResponse_httpStatus :: Lens.Lens' DisassociateWirelessGatewayFromCertificateResponse Prelude.Int
disassociateWirelessGatewayFromCertificateResponse_httpStatus = Lens.lens (\DisassociateWirelessGatewayFromCertificateResponse' {httpStatus} -> httpStatus) (\s@DisassociateWirelessGatewayFromCertificateResponse' {} a -> s {httpStatus = a} :: DisassociateWirelessGatewayFromCertificateResponse)

instance
  Prelude.NFData
    DisassociateWirelessGatewayFromCertificateResponse
  where
  rnf
    DisassociateWirelessGatewayFromCertificateResponse' {..} =
      Prelude.rnf httpStatus
