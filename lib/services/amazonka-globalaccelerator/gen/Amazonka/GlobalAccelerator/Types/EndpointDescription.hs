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
-- Module      : Amazonka.GlobalAccelerator.Types.EndpointDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.EndpointDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types.HealthState
import qualified Amazonka.Prelude as Prelude

-- | A complex type for an endpoint. Each endpoint group can include one or
-- more endpoints, such as load balancers.
--
-- /See:/ 'newEndpointDescription' smart constructor.
data EndpointDescription = EndpointDescription'
  { -- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
    -- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
    -- resource. If the endpoint is an Elastic IP address, this is the Elastic
    -- IP address allocation ID. For Amazon EC2 instances, this is the EC2
    -- instance ID.
    --
    -- An Application Load Balancer can be either internal or internet-facing.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | Returns a null result.
    healthReason :: Prelude.Maybe Prelude.Text,
    -- | The weight associated with the endpoint. When you add weights to
    -- endpoints, you configure Global Accelerator to route traffic based on
    -- proportions that you specify. For example, you might specify endpoint
    -- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
    -- traffic, on average, is routed to the first endpoint, 5\/20 is routed
    -- both to the second and third endpoints, and 6\/20 is routed to the last
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint weights>
    -- in the /Global Accelerator Developer Guide/.
    weight :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether client IP address preservation is enabled for an
    -- endpoint. The value is true or false. The default value is true for new
    -- accelerators.
    --
    -- If the value is set to true, the client\'s IP address is preserved in
    -- the @X-Forwarded-For@ request header as traffic travels to applications
    -- on the endpoint fronted by the accelerator.
    --
    -- Client IP address preservation is supported, in specific Amazon Web
    -- Services Regions, for endpoints that are Application Load Balancers and
    -- Amazon EC2 instances.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve client IP addresses in Global Accelerator>
    -- in the /Global Accelerator Developer Guide/.
    clientIPPreservationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The health status of the endpoint.
    healthState :: Prelude.Maybe HealthState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointId', 'endpointDescription_endpointId' - An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
--
-- 'healthReason', 'endpointDescription_healthReason' - Returns a null result.
--
-- 'weight', 'endpointDescription_weight' - The weight associated with the endpoint. When you add weights to
-- endpoints, you configure Global Accelerator to route traffic based on
-- proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint weights>
-- in the /Global Accelerator Developer Guide/.
--
-- 'clientIPPreservationEnabled', 'endpointDescription_clientIPPreservationEnabled' - Indicates whether client IP address preservation is enabled for an
-- endpoint. The value is true or false. The default value is true for new
-- accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the endpoint fronted by the accelerator.
--
-- Client IP address preservation is supported, in specific Amazon Web
-- Services Regions, for endpoints that are Application Load Balancers and
-- Amazon EC2 instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve client IP addresses in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
--
-- 'healthState', 'endpointDescription_healthState' - The health status of the endpoint.
newEndpointDescription ::
  EndpointDescription
newEndpointDescription =
  EndpointDescription'
    { endpointId = Prelude.Nothing,
      healthReason = Prelude.Nothing,
      weight = Prelude.Nothing,
      clientIPPreservationEnabled = Prelude.Nothing,
      healthState = Prelude.Nothing
    }

-- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
endpointDescription_endpointId :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Text)
endpointDescription_endpointId = Lens.lens (\EndpointDescription' {endpointId} -> endpointId) (\s@EndpointDescription' {} a -> s {endpointId = a} :: EndpointDescription)

-- | Returns a null result.
endpointDescription_healthReason :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Text)
endpointDescription_healthReason = Lens.lens (\EndpointDescription' {healthReason} -> healthReason) (\s@EndpointDescription' {} a -> s {healthReason = a} :: EndpointDescription)

-- | The weight associated with the endpoint. When you add weights to
-- endpoints, you configure Global Accelerator to route traffic based on
-- proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint weights>
-- in the /Global Accelerator Developer Guide/.
endpointDescription_weight :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Natural)
endpointDescription_weight = Lens.lens (\EndpointDescription' {weight} -> weight) (\s@EndpointDescription' {} a -> s {weight = a} :: EndpointDescription)

-- | Indicates whether client IP address preservation is enabled for an
-- endpoint. The value is true or false. The default value is true for new
-- accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the endpoint fronted by the accelerator.
--
-- Client IP address preservation is supported, in specific Amazon Web
-- Services Regions, for endpoints that are Application Load Balancers and
-- Amazon EC2 instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/preserve-client-ip-address.html Preserve client IP addresses in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
endpointDescription_clientIPPreservationEnabled :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Bool)
endpointDescription_clientIPPreservationEnabled = Lens.lens (\EndpointDescription' {clientIPPreservationEnabled} -> clientIPPreservationEnabled) (\s@EndpointDescription' {} a -> s {clientIPPreservationEnabled = a} :: EndpointDescription)

-- | The health status of the endpoint.
endpointDescription_healthState :: Lens.Lens' EndpointDescription (Prelude.Maybe HealthState)
endpointDescription_healthState = Lens.lens (\EndpointDescription' {healthState} -> healthState) (\s@EndpointDescription' {} a -> s {healthState = a} :: EndpointDescription)

instance Core.FromJSON EndpointDescription where
  parseJSON =
    Core.withObject
      "EndpointDescription"
      ( \x ->
          EndpointDescription'
            Prelude.<$> (x Core..:? "EndpointId")
            Prelude.<*> (x Core..:? "HealthReason")
            Prelude.<*> (x Core..:? "Weight")
            Prelude.<*> (x Core..:? "ClientIPPreservationEnabled")
            Prelude.<*> (x Core..:? "HealthState")
      )

instance Prelude.Hashable EndpointDescription where
  hashWithSalt _salt EndpointDescription' {..} =
    _salt `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` healthReason
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` clientIPPreservationEnabled
      `Prelude.hashWithSalt` healthState

instance Prelude.NFData EndpointDescription where
  rnf EndpointDescription' {..} =
    Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf healthReason
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf clientIPPreservationEnabled
      `Prelude.seq` Prelude.rnf healthState
