{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.EndpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type for endpoints. A resource must be valid and active when
-- you add it as an endpoint.
--
-- /See:/ 'newEndpointConfiguration' smart constructor.
data EndpointConfiguration = EndpointConfiguration'
  { -- | The weight associated with the endpoint. When you add weights to
    -- endpoints, you configure AWS Global Accelerator to route traffic based
    -- on proportions that you specify. For example, you might specify endpoint
    -- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
    -- traffic, on average, is routed to the first endpoint, 5\/20 is routed
    -- both to the second and third endpoints, and 6\/20 is routed to the last
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint Weights>
    -- in the /AWS Global Accelerator Developer Guide/.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether client IP address preservation is enabled for an
    -- Application Load Balancer endpoint. The value is true or false. The
    -- default value is true for new accelerators.
    --
    -- If the value is set to true, the client\'s IP address is preserved in
    -- the @X-Forwarded-For@ request header as traffic travels to applications
    -- on the Application Load Balancer endpoint fronted by the accelerator.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve Client IP Addresses in AWS Global Accelerator>
    -- in the /AWS Global Accelerator Developer Guide/.
    clientIPPreservationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
    -- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
    -- resource. If the endpoint is an Elastic IP address, this is the Elastic
    -- IP address allocation ID. For Amazon EC2 instances, this is the EC2
    -- instance ID. A resource must be valid and active when you add it as an
    -- endpoint.
    --
    -- An Application Load Balancer can be either internal or internet-facing.
    endpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weight', 'endpointConfiguration_weight' - The weight associated with the endpoint. When you add weights to
-- endpoints, you configure AWS Global Accelerator to route traffic based
-- on proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint Weights>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'clientIPPreservationEnabled', 'endpointConfiguration_clientIPPreservationEnabled' - Indicates whether client IP address preservation is enabled for an
-- Application Load Balancer endpoint. The value is true or false. The
-- default value is true for new accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the Application Load Balancer endpoint fronted by the accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve Client IP Addresses in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'endpointId', 'endpointConfiguration_endpointId' - An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID. A resource must be valid and active when you add it as an
-- endpoint.
--
-- An Application Load Balancer can be either internal or internet-facing.
newEndpointConfiguration ::
  EndpointConfiguration
newEndpointConfiguration =
  EndpointConfiguration'
    { weight = Prelude.Nothing,
      clientIPPreservationEnabled = Prelude.Nothing,
      endpointId = Prelude.Nothing
    }

-- | The weight associated with the endpoint. When you add weights to
-- endpoints, you configure AWS Global Accelerator to route traffic based
-- on proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint Weights>
-- in the /AWS Global Accelerator Developer Guide/.
endpointConfiguration_weight :: Lens.Lens' EndpointConfiguration (Prelude.Maybe Prelude.Natural)
endpointConfiguration_weight = Lens.lens (\EndpointConfiguration' {weight} -> weight) (\s@EndpointConfiguration' {} a -> s {weight = a} :: EndpointConfiguration)

-- | Indicates whether client IP address preservation is enabled for an
-- Application Load Balancer endpoint. The value is true or false. The
-- default value is true for new accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the Application Load Balancer endpoint fronted by the accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve Client IP Addresses in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
endpointConfiguration_clientIPPreservationEnabled :: Lens.Lens' EndpointConfiguration (Prelude.Maybe Prelude.Bool)
endpointConfiguration_clientIPPreservationEnabled = Lens.lens (\EndpointConfiguration' {clientIPPreservationEnabled} -> clientIPPreservationEnabled) (\s@EndpointConfiguration' {} a -> s {clientIPPreservationEnabled = a} :: EndpointConfiguration)

-- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID. A resource must be valid and active when you add it as an
-- endpoint.
--
-- An Application Load Balancer can be either internal or internet-facing.
endpointConfiguration_endpointId :: Lens.Lens' EndpointConfiguration (Prelude.Maybe Prelude.Text)
endpointConfiguration_endpointId = Lens.lens (\EndpointConfiguration' {endpointId} -> endpointId) (\s@EndpointConfiguration' {} a -> s {endpointId = a} :: EndpointConfiguration)

instance Prelude.Hashable EndpointConfiguration

instance Prelude.NFData EndpointConfiguration

instance Core.ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Weight" Core..=) Prelude.<$> weight,
            ("ClientIPPreservationEnabled" Core..=)
              Prelude.<$> clientIPPreservationEnabled,
            ("EndpointId" Core..=) Prelude.<$> endpointId
          ]
      )
