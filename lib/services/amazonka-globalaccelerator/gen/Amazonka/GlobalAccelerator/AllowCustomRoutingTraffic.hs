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
-- Module      : Amazonka.GlobalAccelerator.AllowCustomRoutingTraffic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the Amazon EC2 instance (destination) IP addresses and ports for
-- a VPC subnet endpoint that can receive traffic for a custom routing
-- accelerator. You can allow traffic to all destinations in the subnet
-- endpoint, or allow traffic to a specified list of destination IP
-- addresses and ports in the subnet. Note that you cannot specify IP
-- addresses or ports outside of the range that you configured for the
-- endpoint group.
--
-- After you make changes, you can verify that the updates are complete by
-- checking the status of your accelerator: the status changes from
-- IN_PROGRESS to DEPLOYED.
module Amazonka.GlobalAccelerator.AllowCustomRoutingTraffic
  ( -- * Creating a Request
    AllowCustomRoutingTraffic (..),
    newAllowCustomRoutingTraffic,

    -- * Request Lenses
    allowCustomRoutingTraffic_allowAllTrafficToEndpoint,
    allowCustomRoutingTraffic_destinationAddresses,
    allowCustomRoutingTraffic_destinationPorts,
    allowCustomRoutingTraffic_endpointGroupArn,
    allowCustomRoutingTraffic_endpointId,

    -- * Destructuring the Response
    AllowCustomRoutingTrafficResponse (..),
    newAllowCustomRoutingTrafficResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAllowCustomRoutingTraffic' smart constructor.
data AllowCustomRoutingTraffic = AllowCustomRoutingTraffic'
  { -- | Indicates whether all destination IP addresses and ports for a specified
    -- VPC subnet endpoint can receive traffic from a custom routing
    -- accelerator. The value is TRUE or FALSE.
    --
    -- When set to TRUE, /all/ destinations in the custom routing VPC subnet
    -- can receive traffic. Note that you cannot specify destination IP
    -- addresses and ports when the value is set to TRUE.
    --
    -- When set to FALSE (or not specified), you /must/ specify a list of
    -- destination IP addresses that are allowed to receive traffic. A list of
    -- ports is optional. If you don\'t specify a list of ports, the ports that
    -- can accept traffic is the same as the ports configured for the endpoint
    -- group.
    --
    -- The default value is FALSE.
    allowAllTrafficToEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | A list of specific Amazon EC2 instance IP addresses (destination
    -- addresses) in a subnet that you want to allow to receive traffic. The IP
    -- addresses must be a subset of the IP addresses that you specified for
    -- the endpoint group.
    --
    -- @DestinationAddresses@ is required if @AllowAllTrafficToEndpoint@ is
    -- @FALSE@ or is not specified.
    destinationAddresses :: Prelude.Maybe [Prelude.Text],
    -- | A list of specific Amazon EC2 instance ports (destination ports) that
    -- you want to allow to receive traffic.
    destinationPorts :: Prelude.Maybe [Prelude.Natural],
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Text,
    -- | An ID for the endpoint. For custom routing accelerators, this is the
    -- virtual private cloud (VPC) subnet ID.
    endpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowCustomRoutingTraffic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAllTrafficToEndpoint', 'allowCustomRoutingTraffic_allowAllTrafficToEndpoint' - Indicates whether all destination IP addresses and ports for a specified
-- VPC subnet endpoint can receive traffic from a custom routing
-- accelerator. The value is TRUE or FALSE.
--
-- When set to TRUE, /all/ destinations in the custom routing VPC subnet
-- can receive traffic. Note that you cannot specify destination IP
-- addresses and ports when the value is set to TRUE.
--
-- When set to FALSE (or not specified), you /must/ specify a list of
-- destination IP addresses that are allowed to receive traffic. A list of
-- ports is optional. If you don\'t specify a list of ports, the ports that
-- can accept traffic is the same as the ports configured for the endpoint
-- group.
--
-- The default value is FALSE.
--
-- 'destinationAddresses', 'allowCustomRoutingTraffic_destinationAddresses' - A list of specific Amazon EC2 instance IP addresses (destination
-- addresses) in a subnet that you want to allow to receive traffic. The IP
-- addresses must be a subset of the IP addresses that you specified for
-- the endpoint group.
--
-- @DestinationAddresses@ is required if @AllowAllTrafficToEndpoint@ is
-- @FALSE@ or is not specified.
--
-- 'destinationPorts', 'allowCustomRoutingTraffic_destinationPorts' - A list of specific Amazon EC2 instance ports (destination ports) that
-- you want to allow to receive traffic.
--
-- 'endpointGroupArn', 'allowCustomRoutingTraffic_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'endpointId', 'allowCustomRoutingTraffic_endpointId' - An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
newAllowCustomRoutingTraffic ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  AllowCustomRoutingTraffic
newAllowCustomRoutingTraffic
  pEndpointGroupArn_
  pEndpointId_ =
    AllowCustomRoutingTraffic'
      { allowAllTrafficToEndpoint =
          Prelude.Nothing,
        destinationAddresses = Prelude.Nothing,
        destinationPorts = Prelude.Nothing,
        endpointGroupArn = pEndpointGroupArn_,
        endpointId = pEndpointId_
      }

-- | Indicates whether all destination IP addresses and ports for a specified
-- VPC subnet endpoint can receive traffic from a custom routing
-- accelerator. The value is TRUE or FALSE.
--
-- When set to TRUE, /all/ destinations in the custom routing VPC subnet
-- can receive traffic. Note that you cannot specify destination IP
-- addresses and ports when the value is set to TRUE.
--
-- When set to FALSE (or not specified), you /must/ specify a list of
-- destination IP addresses that are allowed to receive traffic. A list of
-- ports is optional. If you don\'t specify a list of ports, the ports that
-- can accept traffic is the same as the ports configured for the endpoint
-- group.
--
-- The default value is FALSE.
allowCustomRoutingTraffic_allowAllTrafficToEndpoint :: Lens.Lens' AllowCustomRoutingTraffic (Prelude.Maybe Prelude.Bool)
allowCustomRoutingTraffic_allowAllTrafficToEndpoint = Lens.lens (\AllowCustomRoutingTraffic' {allowAllTrafficToEndpoint} -> allowAllTrafficToEndpoint) (\s@AllowCustomRoutingTraffic' {} a -> s {allowAllTrafficToEndpoint = a} :: AllowCustomRoutingTraffic)

-- | A list of specific Amazon EC2 instance IP addresses (destination
-- addresses) in a subnet that you want to allow to receive traffic. The IP
-- addresses must be a subset of the IP addresses that you specified for
-- the endpoint group.
--
-- @DestinationAddresses@ is required if @AllowAllTrafficToEndpoint@ is
-- @FALSE@ or is not specified.
allowCustomRoutingTraffic_destinationAddresses :: Lens.Lens' AllowCustomRoutingTraffic (Prelude.Maybe [Prelude.Text])
allowCustomRoutingTraffic_destinationAddresses = Lens.lens (\AllowCustomRoutingTraffic' {destinationAddresses} -> destinationAddresses) (\s@AllowCustomRoutingTraffic' {} a -> s {destinationAddresses = a} :: AllowCustomRoutingTraffic) Prelude.. Lens.mapping Lens.coerced

-- | A list of specific Amazon EC2 instance ports (destination ports) that
-- you want to allow to receive traffic.
allowCustomRoutingTraffic_destinationPorts :: Lens.Lens' AllowCustomRoutingTraffic (Prelude.Maybe [Prelude.Natural])
allowCustomRoutingTraffic_destinationPorts = Lens.lens (\AllowCustomRoutingTraffic' {destinationPorts} -> destinationPorts) (\s@AllowCustomRoutingTraffic' {} a -> s {destinationPorts = a} :: AllowCustomRoutingTraffic) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
allowCustomRoutingTraffic_endpointGroupArn :: Lens.Lens' AllowCustomRoutingTraffic Prelude.Text
allowCustomRoutingTraffic_endpointGroupArn = Lens.lens (\AllowCustomRoutingTraffic' {endpointGroupArn} -> endpointGroupArn) (\s@AllowCustomRoutingTraffic' {} a -> s {endpointGroupArn = a} :: AllowCustomRoutingTraffic)

-- | An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
allowCustomRoutingTraffic_endpointId :: Lens.Lens' AllowCustomRoutingTraffic Prelude.Text
allowCustomRoutingTraffic_endpointId = Lens.lens (\AllowCustomRoutingTraffic' {endpointId} -> endpointId) (\s@AllowCustomRoutingTraffic' {} a -> s {endpointId = a} :: AllowCustomRoutingTraffic)

instance Core.AWSRequest AllowCustomRoutingTraffic where
  type
    AWSResponse AllowCustomRoutingTraffic =
      AllowCustomRoutingTrafficResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      AllowCustomRoutingTrafficResponse'

instance Prelude.Hashable AllowCustomRoutingTraffic where
  hashWithSalt _salt AllowCustomRoutingTraffic' {..} =
    _salt
      `Prelude.hashWithSalt` allowAllTrafficToEndpoint
      `Prelude.hashWithSalt` destinationAddresses
      `Prelude.hashWithSalt` destinationPorts
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData AllowCustomRoutingTraffic where
  rnf AllowCustomRoutingTraffic' {..} =
    Prelude.rnf allowAllTrafficToEndpoint `Prelude.seq`
      Prelude.rnf destinationAddresses `Prelude.seq`
        Prelude.rnf destinationPorts `Prelude.seq`
          Prelude.rnf endpointGroupArn `Prelude.seq`
            Prelude.rnf endpointId

instance Data.ToHeaders AllowCustomRoutingTraffic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.AllowCustomRoutingTraffic" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AllowCustomRoutingTraffic where
  toJSON AllowCustomRoutingTraffic' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowAllTrafficToEndpoint" Data..=)
              Prelude.<$> allowAllTrafficToEndpoint,
            ("DestinationAddresses" Data..=)
              Prelude.<$> destinationAddresses,
            ("DestinationPorts" Data..=)
              Prelude.<$> destinationPorts,
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn),
            Prelude.Just ("EndpointId" Data..= endpointId)
          ]
      )

instance Data.ToPath AllowCustomRoutingTraffic where
  toPath = Prelude.const "/"

instance Data.ToQuery AllowCustomRoutingTraffic where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAllowCustomRoutingTrafficResponse' smart constructor.
data AllowCustomRoutingTrafficResponse = AllowCustomRoutingTrafficResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowCustomRoutingTrafficResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAllowCustomRoutingTrafficResponse ::
  AllowCustomRoutingTrafficResponse
newAllowCustomRoutingTrafficResponse =
  AllowCustomRoutingTrafficResponse'

instance
  Prelude.NFData
    AllowCustomRoutingTrafficResponse
  where
  rnf _ = ()
