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
-- Module      : Amazonka.GlobalAccelerator.AddCustomRoutingEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a virtual private cloud (VPC) subnet endpoint with your custom
-- routing accelerator.
--
-- The listener port range must be large enough to support the number of IP
-- addresses that can be specified in your subnet. The number of ports
-- required is: subnet size times the number of ports per destination EC2
-- instances. For example, a subnet defined as \/24 requires a listener
-- port range of at least 255 ports.
--
-- Note: You must have enough remaining listener ports available to map to
-- the subnet ports, or the call will fail with a LimitExceededException.
--
-- By default, all destinations in a subnet in a custom routing accelerator
-- cannot receive traffic. To enable all destinations to receive traffic,
-- or to specify individual port mappings that can receive traffic, see the
-- <https://docs.aws.amazon.com/global-accelerator/latest/api/API_AllowCustomRoutingTraffic.html AllowCustomRoutingTraffic>
-- operation.
module Amazonka.GlobalAccelerator.AddCustomRoutingEndpoints
  ( -- * Creating a Request
    AddCustomRoutingEndpoints (..),
    newAddCustomRoutingEndpoints,

    -- * Request Lenses
    addCustomRoutingEndpoints_endpointConfigurations,
    addCustomRoutingEndpoints_endpointGroupArn,

    -- * Destructuring the Response
    AddCustomRoutingEndpointsResponse (..),
    newAddCustomRoutingEndpointsResponse,

    -- * Response Lenses
    addCustomRoutingEndpointsResponse_endpointDescriptions,
    addCustomRoutingEndpointsResponse_endpointGroupArn,
    addCustomRoutingEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddCustomRoutingEndpoints' smart constructor.
data AddCustomRoutingEndpoints = AddCustomRoutingEndpoints'
  { -- | The list of endpoint objects to add to a custom routing accelerator.
    endpointConfigurations :: Prelude.NonEmpty CustomRoutingEndpointConfiguration,
    -- | The Amazon Resource Name (ARN) of the endpoint group for the custom
    -- routing endpoint.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddCustomRoutingEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigurations', 'addCustomRoutingEndpoints_endpointConfigurations' - The list of endpoint objects to add to a custom routing accelerator.
--
-- 'endpointGroupArn', 'addCustomRoutingEndpoints_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
newAddCustomRoutingEndpoints ::
  -- | 'endpointConfigurations'
  Prelude.NonEmpty CustomRoutingEndpointConfiguration ->
  -- | 'endpointGroupArn'
  Prelude.Text ->
  AddCustomRoutingEndpoints
newAddCustomRoutingEndpoints
  pEndpointConfigurations_
  pEndpointGroupArn_ =
    AddCustomRoutingEndpoints'
      { endpointConfigurations =
          Lens.coerced Lens.# pEndpointConfigurations_,
        endpointGroupArn = pEndpointGroupArn_
      }

-- | The list of endpoint objects to add to a custom routing accelerator.
addCustomRoutingEndpoints_endpointConfigurations :: Lens.Lens' AddCustomRoutingEndpoints (Prelude.NonEmpty CustomRoutingEndpointConfiguration)
addCustomRoutingEndpoints_endpointConfigurations = Lens.lens (\AddCustomRoutingEndpoints' {endpointConfigurations} -> endpointConfigurations) (\s@AddCustomRoutingEndpoints' {} a -> s {endpointConfigurations = a} :: AddCustomRoutingEndpoints) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
addCustomRoutingEndpoints_endpointGroupArn :: Lens.Lens' AddCustomRoutingEndpoints Prelude.Text
addCustomRoutingEndpoints_endpointGroupArn = Lens.lens (\AddCustomRoutingEndpoints' {endpointGroupArn} -> endpointGroupArn) (\s@AddCustomRoutingEndpoints' {} a -> s {endpointGroupArn = a} :: AddCustomRoutingEndpoints)

instance Core.AWSRequest AddCustomRoutingEndpoints where
  type
    AWSResponse AddCustomRoutingEndpoints =
      AddCustomRoutingEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCustomRoutingEndpointsResponse'
            Prelude.<$> ( x
                            Data..?> "EndpointDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "EndpointGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCustomRoutingEndpoints where
  hashWithSalt _salt AddCustomRoutingEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData AddCustomRoutingEndpoints where
  rnf AddCustomRoutingEndpoints' {..} =
    Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf endpointGroupArn

instance Data.ToHeaders AddCustomRoutingEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.AddCustomRoutingEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddCustomRoutingEndpoints where
  toJSON AddCustomRoutingEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EndpointConfigurations"
                  Data..= endpointConfigurations
              ),
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath AddCustomRoutingEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery AddCustomRoutingEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddCustomRoutingEndpointsResponse' smart constructor.
data AddCustomRoutingEndpointsResponse = AddCustomRoutingEndpointsResponse'
  { -- | The endpoint objects added to the custom routing accelerator.
    endpointDescriptions :: Prelude.Maybe [CustomRoutingEndpointDescription],
    -- | The Amazon Resource Name (ARN) of the endpoint group for the custom
    -- routing endpoint.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddCustomRoutingEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointDescriptions', 'addCustomRoutingEndpointsResponse_endpointDescriptions' - The endpoint objects added to the custom routing accelerator.
--
-- 'endpointGroupArn', 'addCustomRoutingEndpointsResponse_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
--
-- 'httpStatus', 'addCustomRoutingEndpointsResponse_httpStatus' - The response's http status code.
newAddCustomRoutingEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddCustomRoutingEndpointsResponse
newAddCustomRoutingEndpointsResponse pHttpStatus_ =
  AddCustomRoutingEndpointsResponse'
    { endpointDescriptions =
        Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint objects added to the custom routing accelerator.
addCustomRoutingEndpointsResponse_endpointDescriptions :: Lens.Lens' AddCustomRoutingEndpointsResponse (Prelude.Maybe [CustomRoutingEndpointDescription])
addCustomRoutingEndpointsResponse_endpointDescriptions = Lens.lens (\AddCustomRoutingEndpointsResponse' {endpointDescriptions} -> endpointDescriptions) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {endpointDescriptions = a} :: AddCustomRoutingEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
addCustomRoutingEndpointsResponse_endpointGroupArn :: Lens.Lens' AddCustomRoutingEndpointsResponse (Prelude.Maybe Prelude.Text)
addCustomRoutingEndpointsResponse_endpointGroupArn = Lens.lens (\AddCustomRoutingEndpointsResponse' {endpointGroupArn} -> endpointGroupArn) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {endpointGroupArn = a} :: AddCustomRoutingEndpointsResponse)

-- | The response's http status code.
addCustomRoutingEndpointsResponse_httpStatus :: Lens.Lens' AddCustomRoutingEndpointsResponse Prelude.Int
addCustomRoutingEndpointsResponse_httpStatus = Lens.lens (\AddCustomRoutingEndpointsResponse' {httpStatus} -> httpStatus) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {httpStatus = a} :: AddCustomRoutingEndpointsResponse)

instance
  Prelude.NFData
    AddCustomRoutingEndpointsResponse
  where
  rnf AddCustomRoutingEndpointsResponse' {..} =
    Prelude.rnf endpointDescriptions
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
