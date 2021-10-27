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
-- Module      : Network.AWS.GlobalAccelerator.Types.EndpointDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GlobalAccelerator.Types.EndpointDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types.HealthState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type for an endpoint. Each endpoint group can include one or
-- more endpoints, such as load balancers.
--
-- /See:/ 'newEndpointDescription' smart constructor.
data EndpointDescription = EndpointDescription'
  { -- | Returns a null result.
    healthReason :: Prelude.Maybe Prelude.Text,
    -- | The weight associated with the endpoint. When you add weights to
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
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/introduction-how-it-works-client-ip.html Viewing Client IP Addresses in AWS Global Accelerator>
    -- in the /AWS Global Accelerator Developer Guide/.
    clientIPPreservationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The health status of the endpoint.
    healthState :: Prelude.Maybe HealthState,
    -- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
    -- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
    -- resource. If the endpoint is an Elastic IP address, this is the Elastic
    -- IP address allocation ID. For Amazon EC2 instances, this is the EC2
    -- instance ID.
    --
    -- An Application Load Balancer can be either internal or internet-facing.
    endpointId :: Prelude.Maybe Prelude.Text
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
-- 'healthReason', 'endpointDescription_healthReason' - Returns a null result.
--
-- 'weight', 'endpointDescription_weight' - The weight associated with the endpoint. When you add weights to
-- endpoints, you configure AWS Global Accelerator to route traffic based
-- on proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint Weights>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'clientIPPreservationEnabled', 'endpointDescription_clientIPPreservationEnabled' - Indicates whether client IP address preservation is enabled for an
-- Application Load Balancer endpoint. The value is true or false. The
-- default value is true for new accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the Application Load Balancer endpoint fronted by the accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/introduction-how-it-works-client-ip.html Viewing Client IP Addresses in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'healthState', 'endpointDescription_healthState' - The health status of the endpoint.
--
-- 'endpointId', 'endpointDescription_endpointId' - An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
newEndpointDescription ::
  EndpointDescription
newEndpointDescription =
  EndpointDescription'
    { healthReason =
        Prelude.Nothing,
      weight = Prelude.Nothing,
      clientIPPreservationEnabled = Prelude.Nothing,
      healthState = Prelude.Nothing,
      endpointId = Prelude.Nothing
    }

-- | Returns a null result.
endpointDescription_healthReason :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Text)
endpointDescription_healthReason = Lens.lens (\EndpointDescription' {healthReason} -> healthReason) (\s@EndpointDescription' {} a -> s {healthReason = a} :: EndpointDescription)

-- | The weight associated with the endpoint. When you add weights to
-- endpoints, you configure AWS Global Accelerator to route traffic based
-- on proportions that you specify. For example, you might specify endpoint
-- weights of 4, 5, 5, and 6 (sum=20). The result is that 4\/20 of your
-- traffic, on average, is routed to the first endpoint, 5\/20 is routed
-- both to the second and third endpoints, and 6\/20 is routed to the last
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoints-endpoint-weights.html Endpoint Weights>
-- in the /AWS Global Accelerator Developer Guide/.
endpointDescription_weight :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Natural)
endpointDescription_weight = Lens.lens (\EndpointDescription' {weight} -> weight) (\s@EndpointDescription' {} a -> s {weight = a} :: EndpointDescription)

-- | Indicates whether client IP address preservation is enabled for an
-- Application Load Balancer endpoint. The value is true or false. The
-- default value is true for new accelerators.
--
-- If the value is set to true, the client\'s IP address is preserved in
-- the @X-Forwarded-For@ request header as traffic travels to applications
-- on the Application Load Balancer endpoint fronted by the accelerator.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/introduction-how-it-works-client-ip.html Viewing Client IP Addresses in AWS Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
endpointDescription_clientIPPreservationEnabled :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Bool)
endpointDescription_clientIPPreservationEnabled = Lens.lens (\EndpointDescription' {clientIPPreservationEnabled} -> clientIPPreservationEnabled) (\s@EndpointDescription' {} a -> s {clientIPPreservationEnabled = a} :: EndpointDescription)

-- | The health status of the endpoint.
endpointDescription_healthState :: Lens.Lens' EndpointDescription (Prelude.Maybe HealthState)
endpointDescription_healthState = Lens.lens (\EndpointDescription' {healthState} -> healthState) (\s@EndpointDescription' {} a -> s {healthState = a} :: EndpointDescription)

-- | An ID for the endpoint. If the endpoint is a Network Load Balancer or
-- Application Load Balancer, this is the Amazon Resource Name (ARN) of the
-- resource. If the endpoint is an Elastic IP address, this is the Elastic
-- IP address allocation ID. For Amazon EC2 instances, this is the EC2
-- instance ID.
--
-- An Application Load Balancer can be either internal or internet-facing.
endpointDescription_endpointId :: Lens.Lens' EndpointDescription (Prelude.Maybe Prelude.Text)
endpointDescription_endpointId = Lens.lens (\EndpointDescription' {endpointId} -> endpointId) (\s@EndpointDescription' {} a -> s {endpointId = a} :: EndpointDescription)

instance Core.FromJSON EndpointDescription where
  parseJSON =
    Core.withObject
      "EndpointDescription"
      ( \x ->
          EndpointDescription'
            Prelude.<$> (x Core..:? "HealthReason")
            Prelude.<*> (x Core..:? "Weight")
            Prelude.<*> (x Core..:? "ClientIPPreservationEnabled")
            Prelude.<*> (x Core..:? "HealthState")
            Prelude.<*> (x Core..:? "EndpointId")
      )

instance Prelude.Hashable EndpointDescription

instance Prelude.NFData EndpointDescription
