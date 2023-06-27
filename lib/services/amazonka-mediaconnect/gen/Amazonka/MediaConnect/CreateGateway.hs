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
-- Module      : Amazonka.MediaConnect.CreateGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new gateway. The request must include at least one network (up
-- to 4).
module Amazonka.MediaConnect.CreateGateway
  ( -- * Creating a Request
    CreateGateway (..),
    newCreateGateway,

    -- * Request Lenses
    createGateway_networks,
    createGateway_egressCidrBlocks,
    createGateway_name,

    -- * Destructuring the Response
    CreateGatewayResponse (..),
    newCreateGatewayResponse,

    -- * Response Lenses
    createGatewayResponse_gateway,
    createGatewayResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new gateway. The request must include at least one network (up
-- to 4).
--
-- /See:/ 'newCreateGateway' smart constructor.
data CreateGateway = CreateGateway'
  { -- | The list of networks that you want to add.
    networks :: [GatewayNetwork],
    -- | The range of IP addresses that are allowed to contribute content or
    -- initiate output requests for flows communicating with this gateway.
    -- These IP addresses should be in the form of a Classless Inter-Domain
    -- Routing (CIDR) block; for example, 10.0.0.0\/16.
    egressCidrBlocks :: [Prelude.Text],
    -- | The name of the gateway. This name can not be modified after the gateway
    -- is created.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networks', 'createGateway_networks' - The list of networks that you want to add.
--
-- 'egressCidrBlocks', 'createGateway_egressCidrBlocks' - The range of IP addresses that are allowed to contribute content or
-- initiate output requests for flows communicating with this gateway.
-- These IP addresses should be in the form of a Classless Inter-Domain
-- Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'name', 'createGateway_name' - The name of the gateway. This name can not be modified after the gateway
-- is created.
newCreateGateway ::
  -- | 'name'
  Prelude.Text ->
  CreateGateway
newCreateGateway pName_ =
  CreateGateway'
    { networks = Prelude.mempty,
      egressCidrBlocks = Prelude.mempty,
      name = pName_
    }

-- | The list of networks that you want to add.
createGateway_networks :: Lens.Lens' CreateGateway [GatewayNetwork]
createGateway_networks = Lens.lens (\CreateGateway' {networks} -> networks) (\s@CreateGateway' {} a -> s {networks = a} :: CreateGateway) Prelude.. Lens.coerced

-- | The range of IP addresses that are allowed to contribute content or
-- initiate output requests for flows communicating with this gateway.
-- These IP addresses should be in the form of a Classless Inter-Domain
-- Routing (CIDR) block; for example, 10.0.0.0\/16.
createGateway_egressCidrBlocks :: Lens.Lens' CreateGateway [Prelude.Text]
createGateway_egressCidrBlocks = Lens.lens (\CreateGateway' {egressCidrBlocks} -> egressCidrBlocks) (\s@CreateGateway' {} a -> s {egressCidrBlocks = a} :: CreateGateway) Prelude.. Lens.coerced

-- | The name of the gateway. This name can not be modified after the gateway
-- is created.
createGateway_name :: Lens.Lens' CreateGateway Prelude.Text
createGateway_name = Lens.lens (\CreateGateway' {name} -> name) (\s@CreateGateway' {} a -> s {name = a} :: CreateGateway)

instance Core.AWSRequest CreateGateway where
  type
    AWSResponse CreateGateway =
      CreateGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayResponse'
            Prelude.<$> (x Data..?> "gateway")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGateway where
  hashWithSalt _salt CreateGateway' {..} =
    _salt
      `Prelude.hashWithSalt` networks
      `Prelude.hashWithSalt` egressCidrBlocks
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateGateway where
  rnf CreateGateway' {..} =
    Prelude.rnf networks
      `Prelude.seq` Prelude.rnf egressCidrBlocks
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGateway where
  toJSON CreateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("networks" Data..= networks),
            Prelude.Just
              ("egressCidrBlocks" Data..= egressCidrBlocks),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateGateway where
  toPath = Prelude.const "/v1/gateways"

instance Data.ToQuery CreateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGatewayResponse' smart constructor.
data CreateGatewayResponse = CreateGatewayResponse'
  { gateway :: Prelude.Maybe Gateway,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateway', 'createGatewayResponse_gateway' - Undocumented member.
--
-- 'httpStatus', 'createGatewayResponse_httpStatus' - The response's http status code.
newCreateGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGatewayResponse
newCreateGatewayResponse pHttpStatus_ =
  CreateGatewayResponse'
    { gateway = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGatewayResponse_gateway :: Lens.Lens' CreateGatewayResponse (Prelude.Maybe Gateway)
createGatewayResponse_gateway = Lens.lens (\CreateGatewayResponse' {gateway} -> gateway) (\s@CreateGatewayResponse' {} a -> s {gateway = a} :: CreateGatewayResponse)

-- | The response's http status code.
createGatewayResponse_httpStatus :: Lens.Lens' CreateGatewayResponse Prelude.Int
createGatewayResponse_httpStatus = Lens.lens (\CreateGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateGatewayResponse' {} a -> s {httpStatus = a} :: CreateGatewayResponse)

instance Prelude.NFData CreateGatewayResponse where
  rnf CreateGatewayResponse' {..} =
    Prelude.rnf gateway
      `Prelude.seq` Prelude.rnf httpStatus
