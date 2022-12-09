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
-- Module      : Amazonka.GlobalAccelerator.DenyCustomRoutingTraffic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the Amazon EC2 instance (destination) IP addresses and ports for
-- a VPC subnet endpoint that cannot receive traffic for a custom routing
-- accelerator. You can deny traffic to all destinations in the VPC
-- endpoint, or deny traffic to a specified list of destination IP
-- addresses and ports. Note that you cannot specify IP addresses or ports
-- outside of the range that you configured for the endpoint group.
--
-- After you make changes, you can verify that the updates are complete by
-- checking the status of your accelerator: the status changes from
-- IN_PROGRESS to DEPLOYED.
module Amazonka.GlobalAccelerator.DenyCustomRoutingTraffic
  ( -- * Creating a Request
    DenyCustomRoutingTraffic (..),
    newDenyCustomRoutingTraffic,

    -- * Request Lenses
    denyCustomRoutingTraffic_denyAllTrafficToEndpoint,
    denyCustomRoutingTraffic_destinationAddresses,
    denyCustomRoutingTraffic_destinationPorts,
    denyCustomRoutingTraffic_endpointGroupArn,
    denyCustomRoutingTraffic_endpointId,

    -- * Destructuring the Response
    DenyCustomRoutingTrafficResponse (..),
    newDenyCustomRoutingTrafficResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDenyCustomRoutingTraffic' smart constructor.
data DenyCustomRoutingTraffic = DenyCustomRoutingTraffic'
  { -- | Indicates whether all destination IP addresses and ports for a specified
    -- VPC subnet endpoint /cannot/ receive traffic from a custom routing
    -- accelerator. The value is TRUE or FALSE.
    --
    -- When set to TRUE, /no/ destinations in the custom routing VPC subnet can
    -- receive traffic. Note that you cannot specify destination IP addresses
    -- and ports when the value is set to TRUE.
    --
    -- When set to FALSE (or not specified), you /must/ specify a list of
    -- destination IP addresses that cannot receive traffic. A list of ports is
    -- optional. If you don\'t specify a list of ports, the ports that can
    -- accept traffic is the same as the ports configured for the endpoint
    -- group.
    --
    -- The default value is FALSE.
    denyAllTrafficToEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | A list of specific Amazon EC2 instance IP addresses (destination
    -- addresses) in a subnet that you want to prevent from receiving traffic.
    -- The IP addresses must be a subset of the IP addresses allowed for the
    -- VPC subnet associated with the endpoint group.
    destinationAddresses :: Prelude.Maybe [Prelude.Text],
    -- | A list of specific Amazon EC2 instance ports (destination ports) in a
    -- subnet endpoint that you want to prevent from receiving traffic.
    destinationPorts :: Prelude.Maybe [Prelude.Natural],
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Text,
    -- | An ID for the endpoint. For custom routing accelerators, this is the
    -- virtual private cloud (VPC) subnet ID.
    endpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DenyCustomRoutingTraffic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'denyAllTrafficToEndpoint', 'denyCustomRoutingTraffic_denyAllTrafficToEndpoint' - Indicates whether all destination IP addresses and ports for a specified
-- VPC subnet endpoint /cannot/ receive traffic from a custom routing
-- accelerator. The value is TRUE or FALSE.
--
-- When set to TRUE, /no/ destinations in the custom routing VPC subnet can
-- receive traffic. Note that you cannot specify destination IP addresses
-- and ports when the value is set to TRUE.
--
-- When set to FALSE (or not specified), you /must/ specify a list of
-- destination IP addresses that cannot receive traffic. A list of ports is
-- optional. If you don\'t specify a list of ports, the ports that can
-- accept traffic is the same as the ports configured for the endpoint
-- group.
--
-- The default value is FALSE.
--
-- 'destinationAddresses', 'denyCustomRoutingTraffic_destinationAddresses' - A list of specific Amazon EC2 instance IP addresses (destination
-- addresses) in a subnet that you want to prevent from receiving traffic.
-- The IP addresses must be a subset of the IP addresses allowed for the
-- VPC subnet associated with the endpoint group.
--
-- 'destinationPorts', 'denyCustomRoutingTraffic_destinationPorts' - A list of specific Amazon EC2 instance ports (destination ports) in a
-- subnet endpoint that you want to prevent from receiving traffic.
--
-- 'endpointGroupArn', 'denyCustomRoutingTraffic_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'endpointId', 'denyCustomRoutingTraffic_endpointId' - An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
newDenyCustomRoutingTraffic ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  DenyCustomRoutingTraffic
newDenyCustomRoutingTraffic
  pEndpointGroupArn_
  pEndpointId_ =
    DenyCustomRoutingTraffic'
      { denyAllTrafficToEndpoint =
          Prelude.Nothing,
        destinationAddresses = Prelude.Nothing,
        destinationPorts = Prelude.Nothing,
        endpointGroupArn = pEndpointGroupArn_,
        endpointId = pEndpointId_
      }

-- | Indicates whether all destination IP addresses and ports for a specified
-- VPC subnet endpoint /cannot/ receive traffic from a custom routing
-- accelerator. The value is TRUE or FALSE.
--
-- When set to TRUE, /no/ destinations in the custom routing VPC subnet can
-- receive traffic. Note that you cannot specify destination IP addresses
-- and ports when the value is set to TRUE.
--
-- When set to FALSE (or not specified), you /must/ specify a list of
-- destination IP addresses that cannot receive traffic. A list of ports is
-- optional. If you don\'t specify a list of ports, the ports that can
-- accept traffic is the same as the ports configured for the endpoint
-- group.
--
-- The default value is FALSE.
denyCustomRoutingTraffic_denyAllTrafficToEndpoint :: Lens.Lens' DenyCustomRoutingTraffic (Prelude.Maybe Prelude.Bool)
denyCustomRoutingTraffic_denyAllTrafficToEndpoint = Lens.lens (\DenyCustomRoutingTraffic' {denyAllTrafficToEndpoint} -> denyAllTrafficToEndpoint) (\s@DenyCustomRoutingTraffic' {} a -> s {denyAllTrafficToEndpoint = a} :: DenyCustomRoutingTraffic)

-- | A list of specific Amazon EC2 instance IP addresses (destination
-- addresses) in a subnet that you want to prevent from receiving traffic.
-- The IP addresses must be a subset of the IP addresses allowed for the
-- VPC subnet associated with the endpoint group.
denyCustomRoutingTraffic_destinationAddresses :: Lens.Lens' DenyCustomRoutingTraffic (Prelude.Maybe [Prelude.Text])
denyCustomRoutingTraffic_destinationAddresses = Lens.lens (\DenyCustomRoutingTraffic' {destinationAddresses} -> destinationAddresses) (\s@DenyCustomRoutingTraffic' {} a -> s {destinationAddresses = a} :: DenyCustomRoutingTraffic) Prelude.. Lens.mapping Lens.coerced

-- | A list of specific Amazon EC2 instance ports (destination ports) in a
-- subnet endpoint that you want to prevent from receiving traffic.
denyCustomRoutingTraffic_destinationPorts :: Lens.Lens' DenyCustomRoutingTraffic (Prelude.Maybe [Prelude.Natural])
denyCustomRoutingTraffic_destinationPorts = Lens.lens (\DenyCustomRoutingTraffic' {destinationPorts} -> destinationPorts) (\s@DenyCustomRoutingTraffic' {} a -> s {destinationPorts = a} :: DenyCustomRoutingTraffic) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
denyCustomRoutingTraffic_endpointGroupArn :: Lens.Lens' DenyCustomRoutingTraffic Prelude.Text
denyCustomRoutingTraffic_endpointGroupArn = Lens.lens (\DenyCustomRoutingTraffic' {endpointGroupArn} -> endpointGroupArn) (\s@DenyCustomRoutingTraffic' {} a -> s {endpointGroupArn = a} :: DenyCustomRoutingTraffic)

-- | An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
denyCustomRoutingTraffic_endpointId :: Lens.Lens' DenyCustomRoutingTraffic Prelude.Text
denyCustomRoutingTraffic_endpointId = Lens.lens (\DenyCustomRoutingTraffic' {endpointId} -> endpointId) (\s@DenyCustomRoutingTraffic' {} a -> s {endpointId = a} :: DenyCustomRoutingTraffic)

instance Core.AWSRequest DenyCustomRoutingTraffic where
  type
    AWSResponse DenyCustomRoutingTraffic =
      DenyCustomRoutingTrafficResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DenyCustomRoutingTrafficResponse'

instance Prelude.Hashable DenyCustomRoutingTraffic where
  hashWithSalt _salt DenyCustomRoutingTraffic' {..} =
    _salt
      `Prelude.hashWithSalt` denyAllTrafficToEndpoint
      `Prelude.hashWithSalt` destinationAddresses
      `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData DenyCustomRoutingTraffic where
  rnf DenyCustomRoutingTraffic' {..} =
    Prelude.rnf denyAllTrafficToEndpoint
      `Prelude.seq` Prelude.rnf destinationAddresses
      `Prelude.seq` Prelude.rnf destinationPorts
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf endpointId

instance Data.ToHeaders DenyCustomRoutingTraffic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DenyCustomRoutingTraffic" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DenyCustomRoutingTraffic where
  toJSON DenyCustomRoutingTraffic' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DenyAllTrafficToEndpoint" Data..=)
              Prelude.<$> denyAllTrafficToEndpoint,
            ("DestinationAddresses" Data..=)
              Prelude.<$> destinationAddresses,
            ("DestinationPorts" Data..=)
              Prelude.<$> destinationPorts,
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn),
            Prelude.Just ("EndpointId" Data..= endpointId)
          ]
      )

instance Data.ToPath DenyCustomRoutingTraffic where
  toPath = Prelude.const "/"

instance Data.ToQuery DenyCustomRoutingTraffic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDenyCustomRoutingTrafficResponse' smart constructor.
data DenyCustomRoutingTrafficResponse = DenyCustomRoutingTrafficResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DenyCustomRoutingTrafficResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDenyCustomRoutingTrafficResponse ::
  DenyCustomRoutingTrafficResponse
newDenyCustomRoutingTrafficResponse =
  DenyCustomRoutingTrafficResponse'

instance
  Prelude.NFData
    DenyCustomRoutingTrafficResponse
  where
  rnf _ = ()
