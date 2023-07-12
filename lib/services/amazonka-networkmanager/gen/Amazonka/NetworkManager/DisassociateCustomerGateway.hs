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
-- Module      : Amazonka.NetworkManager.DisassociateCustomerGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a customer gateway from a device and a link.
module Amazonka.NetworkManager.DisassociateCustomerGateway
  ( -- * Creating a Request
    DisassociateCustomerGateway (..),
    newDisassociateCustomerGateway,

    -- * Request Lenses
    disassociateCustomerGateway_globalNetworkId,
    disassociateCustomerGateway_customerGatewayArn,

    -- * Destructuring the Response
    DisassociateCustomerGatewayResponse (..),
    newDisassociateCustomerGatewayResponse,

    -- * Response Lenses
    disassociateCustomerGatewayResponse_customerGatewayAssociation,
    disassociateCustomerGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateCustomerGateway' smart constructor.
data DisassociateCustomerGateway = DisassociateCustomerGateway'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the customer gateway.
    customerGatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCustomerGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'disassociateCustomerGateway_globalNetworkId' - The ID of the global network.
--
-- 'customerGatewayArn', 'disassociateCustomerGateway_customerGatewayArn' - The Amazon Resource Name (ARN) of the customer gateway.
newDisassociateCustomerGateway ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'customerGatewayArn'
  Prelude.Text ->
  DisassociateCustomerGateway
newDisassociateCustomerGateway
  pGlobalNetworkId_
  pCustomerGatewayArn_ =
    DisassociateCustomerGateway'
      { globalNetworkId =
          pGlobalNetworkId_,
        customerGatewayArn = pCustomerGatewayArn_
      }

-- | The ID of the global network.
disassociateCustomerGateway_globalNetworkId :: Lens.Lens' DisassociateCustomerGateway Prelude.Text
disassociateCustomerGateway_globalNetworkId = Lens.lens (\DisassociateCustomerGateway' {globalNetworkId} -> globalNetworkId) (\s@DisassociateCustomerGateway' {} a -> s {globalNetworkId = a} :: DisassociateCustomerGateway)

-- | The Amazon Resource Name (ARN) of the customer gateway.
disassociateCustomerGateway_customerGatewayArn :: Lens.Lens' DisassociateCustomerGateway Prelude.Text
disassociateCustomerGateway_customerGatewayArn = Lens.lens (\DisassociateCustomerGateway' {customerGatewayArn} -> customerGatewayArn) (\s@DisassociateCustomerGateway' {} a -> s {customerGatewayArn = a} :: DisassociateCustomerGateway)

instance Core.AWSRequest DisassociateCustomerGateway where
  type
    AWSResponse DisassociateCustomerGateway =
      DisassociateCustomerGatewayResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateCustomerGatewayResponse'
            Prelude.<$> (x Data..?> "CustomerGatewayAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateCustomerGateway where
  hashWithSalt _salt DisassociateCustomerGateway' {..} =
    _salt
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` customerGatewayArn

instance Prelude.NFData DisassociateCustomerGateway where
  rnf DisassociateCustomerGateway' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf customerGatewayArn

instance Data.ToHeaders DisassociateCustomerGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateCustomerGateway where
  toPath DisassociateCustomerGateway' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/customer-gateway-associations/",
        Data.toBS customerGatewayArn
      ]

instance Data.ToQuery DisassociateCustomerGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateCustomerGatewayResponse' smart constructor.
data DisassociateCustomerGatewayResponse = DisassociateCustomerGatewayResponse'
  { -- | Information about the customer gateway association.
    customerGatewayAssociation :: Prelude.Maybe CustomerGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateCustomerGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayAssociation', 'disassociateCustomerGatewayResponse_customerGatewayAssociation' - Information about the customer gateway association.
--
-- 'httpStatus', 'disassociateCustomerGatewayResponse_httpStatus' - The response's http status code.
newDisassociateCustomerGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateCustomerGatewayResponse
newDisassociateCustomerGatewayResponse pHttpStatus_ =
  DisassociateCustomerGatewayResponse'
    { customerGatewayAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the customer gateway association.
disassociateCustomerGatewayResponse_customerGatewayAssociation :: Lens.Lens' DisassociateCustomerGatewayResponse (Prelude.Maybe CustomerGatewayAssociation)
disassociateCustomerGatewayResponse_customerGatewayAssociation = Lens.lens (\DisassociateCustomerGatewayResponse' {customerGatewayAssociation} -> customerGatewayAssociation) (\s@DisassociateCustomerGatewayResponse' {} a -> s {customerGatewayAssociation = a} :: DisassociateCustomerGatewayResponse)

-- | The response's http status code.
disassociateCustomerGatewayResponse_httpStatus :: Lens.Lens' DisassociateCustomerGatewayResponse Prelude.Int
disassociateCustomerGatewayResponse_httpStatus = Lens.lens (\DisassociateCustomerGatewayResponse' {httpStatus} -> httpStatus) (\s@DisassociateCustomerGatewayResponse' {} a -> s {httpStatus = a} :: DisassociateCustomerGatewayResponse)

instance
  Prelude.NFData
    DisassociateCustomerGatewayResponse
  where
  rnf DisassociateCustomerGatewayResponse' {..} =
    Prelude.rnf customerGatewayAssociation
      `Prelude.seq` Prelude.rnf httpStatus
