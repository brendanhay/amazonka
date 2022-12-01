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
-- Module      : Amazonka.DirectConnect.CreateDirectConnectGateway
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Direct Connect gateway, which is an intermediate object that
-- enables you to connect a set of virtual interfaces and virtual private
-- gateways. A Direct Connect gateway is global and visible in any Amazon
-- Web Services Region after it is created. The virtual interfaces and
-- virtual private gateways that are connected through a Direct Connect
-- gateway can be in different Amazon Web Services Regions. This enables
-- you to connect to a VPC in any Region, regardless of the Region in which
-- the virtual interfaces are located, and pass traffic between them.
module Amazonka.DirectConnect.CreateDirectConnectGateway
  ( -- * Creating a Request
    CreateDirectConnectGateway (..),
    newCreateDirectConnectGateway,

    -- * Request Lenses
    createDirectConnectGateway_amazonSideAsn,
    createDirectConnectGateway_directConnectGatewayName,

    -- * Destructuring the Response
    CreateDirectConnectGatewayResponse (..),
    newCreateDirectConnectGatewayResponse,

    -- * Response Lenses
    createDirectConnectGatewayResponse_directConnectGateway,
    createDirectConnectGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDirectConnectGateway' smart constructor.
data CreateDirectConnectGateway = CreateDirectConnectGateway'
  { -- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to
    -- be configured on the Amazon side of the connection. The ASN must be in
    -- the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294.
    -- The default is 64512.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The name of the Direct Connect gateway.
    directConnectGatewayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonSideAsn', 'createDirectConnectGateway_amazonSideAsn' - The autonomous system number (ASN) for Border Gateway Protocol (BGP) to
-- be configured on the Amazon side of the connection. The ASN must be in
-- the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294.
-- The default is 64512.
--
-- 'directConnectGatewayName', 'createDirectConnectGateway_directConnectGatewayName' - The name of the Direct Connect gateway.
newCreateDirectConnectGateway ::
  -- | 'directConnectGatewayName'
  Prelude.Text ->
  CreateDirectConnectGateway
newCreateDirectConnectGateway
  pDirectConnectGatewayName_ =
    CreateDirectConnectGateway'
      { amazonSideAsn =
          Prelude.Nothing,
        directConnectGatewayName =
          pDirectConnectGatewayName_
      }

-- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to
-- be configured on the Amazon side of the connection. The ASN must be in
-- the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294.
-- The default is 64512.
createDirectConnectGateway_amazonSideAsn :: Lens.Lens' CreateDirectConnectGateway (Prelude.Maybe Prelude.Integer)
createDirectConnectGateway_amazonSideAsn = Lens.lens (\CreateDirectConnectGateway' {amazonSideAsn} -> amazonSideAsn) (\s@CreateDirectConnectGateway' {} a -> s {amazonSideAsn = a} :: CreateDirectConnectGateway)

-- | The name of the Direct Connect gateway.
createDirectConnectGateway_directConnectGatewayName :: Lens.Lens' CreateDirectConnectGateway Prelude.Text
createDirectConnectGateway_directConnectGatewayName = Lens.lens (\CreateDirectConnectGateway' {directConnectGatewayName} -> directConnectGatewayName) (\s@CreateDirectConnectGateway' {} a -> s {directConnectGatewayName = a} :: CreateDirectConnectGateway)

instance Core.AWSRequest CreateDirectConnectGateway where
  type
    AWSResponse CreateDirectConnectGateway =
      CreateDirectConnectGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayResponse'
            Prelude.<$> (x Core..?> "directConnectGateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDirectConnectGateway where
  hashWithSalt _salt CreateDirectConnectGateway' {..} =
    _salt `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` directConnectGatewayName

instance Prelude.NFData CreateDirectConnectGateway where
  rnf CreateDirectConnectGateway' {..} =
    Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf directConnectGatewayName

instance Core.ToHeaders CreateDirectConnectGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateDirectConnectGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDirectConnectGateway where
  toJSON CreateDirectConnectGateway' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("amazonSideAsn" Core..=) Prelude.<$> amazonSideAsn,
            Prelude.Just
              ( "directConnectGatewayName"
                  Core..= directConnectGatewayName
              )
          ]
      )

instance Core.ToPath CreateDirectConnectGateway where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDirectConnectGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectConnectGatewayResponse' smart constructor.
data CreateDirectConnectGatewayResponse = CreateDirectConnectGatewayResponse'
  { -- | The Direct Connect gateway.
    directConnectGateway :: Prelude.Maybe DirectConnectGateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGateway', 'createDirectConnectGatewayResponse_directConnectGateway' - The Direct Connect gateway.
--
-- 'httpStatus', 'createDirectConnectGatewayResponse_httpStatus' - The response's http status code.
newCreateDirectConnectGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectConnectGatewayResponse
newCreateDirectConnectGatewayResponse pHttpStatus_ =
  CreateDirectConnectGatewayResponse'
    { directConnectGateway =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Direct Connect gateway.
createDirectConnectGatewayResponse_directConnectGateway :: Lens.Lens' CreateDirectConnectGatewayResponse (Prelude.Maybe DirectConnectGateway)
createDirectConnectGatewayResponse_directConnectGateway = Lens.lens (\CreateDirectConnectGatewayResponse' {directConnectGateway} -> directConnectGateway) (\s@CreateDirectConnectGatewayResponse' {} a -> s {directConnectGateway = a} :: CreateDirectConnectGatewayResponse)

-- | The response's http status code.
createDirectConnectGatewayResponse_httpStatus :: Lens.Lens' CreateDirectConnectGatewayResponse Prelude.Int
createDirectConnectGatewayResponse_httpStatus = Lens.lens (\CreateDirectConnectGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateDirectConnectGatewayResponse' {} a -> s {httpStatus = a} :: CreateDirectConnectGatewayResponse)

instance
  Prelude.NFData
    CreateDirectConnectGatewayResponse
  where
  rnf CreateDirectConnectGatewayResponse' {..} =
    Prelude.rnf directConnectGateway
      `Prelude.seq` Prelude.rnf httpStatus
