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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    addCustomRoutingEndpointsResponse_endpointGroupArn,
    addCustomRoutingEndpointsResponse_endpointDescriptions,
    addCustomRoutingEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCustomRoutingEndpointsResponse'
            Prelude.<$> (x Core..?> "EndpointGroupArn")
            Prelude.<*> ( x Core..?> "EndpointDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCustomRoutingEndpoints

instance Prelude.NFData AddCustomRoutingEndpoints

instance Core.ToHeaders AddCustomRoutingEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.AddCustomRoutingEndpoints" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddCustomRoutingEndpoints where
  toJSON AddCustomRoutingEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EndpointConfigurations"
                  Core..= endpointConfigurations
              ),
            Prelude.Just
              ("EndpointGroupArn" Core..= endpointGroupArn)
          ]
      )

instance Core.ToPath AddCustomRoutingEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery AddCustomRoutingEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddCustomRoutingEndpointsResponse' smart constructor.
data AddCustomRoutingEndpointsResponse = AddCustomRoutingEndpointsResponse'
  { -- | The Amazon Resource Name (ARN) of the endpoint group for the custom
    -- routing endpoint.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The endpoint objects added to the custom routing accelerator.
    endpointDescriptions :: Prelude.Maybe [CustomRoutingEndpointDescription],
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
-- 'endpointGroupArn', 'addCustomRoutingEndpointsResponse_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
--
-- 'endpointDescriptions', 'addCustomRoutingEndpointsResponse_endpointDescriptions' - The endpoint objects added to the custom routing accelerator.
--
-- 'httpStatus', 'addCustomRoutingEndpointsResponse_httpStatus' - The response's http status code.
newAddCustomRoutingEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddCustomRoutingEndpointsResponse
newAddCustomRoutingEndpointsResponse pHttpStatus_ =
  AddCustomRoutingEndpointsResponse'
    { endpointGroupArn =
        Prelude.Nothing,
      endpointDescriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the endpoint group for the custom
-- routing endpoint.
addCustomRoutingEndpointsResponse_endpointGroupArn :: Lens.Lens' AddCustomRoutingEndpointsResponse (Prelude.Maybe Prelude.Text)
addCustomRoutingEndpointsResponse_endpointGroupArn = Lens.lens (\AddCustomRoutingEndpointsResponse' {endpointGroupArn} -> endpointGroupArn) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {endpointGroupArn = a} :: AddCustomRoutingEndpointsResponse)

-- | The endpoint objects added to the custom routing accelerator.
addCustomRoutingEndpointsResponse_endpointDescriptions :: Lens.Lens' AddCustomRoutingEndpointsResponse (Prelude.Maybe [CustomRoutingEndpointDescription])
addCustomRoutingEndpointsResponse_endpointDescriptions = Lens.lens (\AddCustomRoutingEndpointsResponse' {endpointDescriptions} -> endpointDescriptions) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {endpointDescriptions = a} :: AddCustomRoutingEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addCustomRoutingEndpointsResponse_httpStatus :: Lens.Lens' AddCustomRoutingEndpointsResponse Prelude.Int
addCustomRoutingEndpointsResponse_httpStatus = Lens.lens (\AddCustomRoutingEndpointsResponse' {httpStatus} -> httpStatus) (\s@AddCustomRoutingEndpointsResponse' {} a -> s {httpStatus = a} :: AddCustomRoutingEndpointsResponse)

instance
  Prelude.NFData
    AddCustomRoutingEndpointsResponse
