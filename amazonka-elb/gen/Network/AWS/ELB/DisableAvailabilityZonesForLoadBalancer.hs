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
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Availability Zones from the set of Availability
-- Zones for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use
-- DetachLoadBalancerFromSubnets.
--
-- There must be at least one Availability Zone registered with a load
-- balancer at all times. After an Availability Zone is removed, all
-- instances registered with the load balancer that are in the removed
-- Availability Zone go into the @OutOfService@ state. Then, the load
-- balancer attempts to equally balance the traffic among its remaining
-- Availability Zones.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
  ( -- * Creating a Request
    DisableAvailabilityZonesForLoadBalancer (..),
    newDisableAvailabilityZonesForLoadBalancer,

    -- * Request Lenses
    disableAvailabilityZonesForLoadBalancer_loadBalancerName,
    disableAvailabilityZonesForLoadBalancer_availabilityZones,

    -- * Destructuring the Response
    DisableAvailabilityZonesForLoadBalancerResponse (..),
    newDisableAvailabilityZonesForLoadBalancerResponse,

    -- * Response Lenses
    disableAvailabilityZonesForLoadBalancerResponse_availabilityZones,
    disableAvailabilityZonesForLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newDisableAvailabilityZonesForLoadBalancer' smart constructor.
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The Availability Zones.
    availabilityZones :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableAvailabilityZonesForLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'disableAvailabilityZonesForLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'availabilityZones', 'disableAvailabilityZonesForLoadBalancer_availabilityZones' - The Availability Zones.
newDisableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  DisableAvailabilityZonesForLoadBalancer
newDisableAvailabilityZonesForLoadBalancer
  pLoadBalancerName_ =
    DisableAvailabilityZonesForLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        availabilityZones = Core.mempty
      }

-- | The name of the load balancer.
disableAvailabilityZonesForLoadBalancer_loadBalancerName :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer Core.Text
disableAvailabilityZonesForLoadBalancer_loadBalancerName = Lens.lens (\DisableAvailabilityZonesForLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DisableAvailabilityZonesForLoadBalancer' {} a -> s {loadBalancerName = a} :: DisableAvailabilityZonesForLoadBalancer)

-- | The Availability Zones.
disableAvailabilityZonesForLoadBalancer_availabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer [Core.Text]
disableAvailabilityZonesForLoadBalancer_availabilityZones = Lens.lens (\DisableAvailabilityZonesForLoadBalancer' {availabilityZones} -> availabilityZones) (\s@DisableAvailabilityZonesForLoadBalancer' {} a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancer) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DisableAvailabilityZonesForLoadBalancer
  where
  type
    AWSResponse
      DisableAvailabilityZonesForLoadBalancer =
      DisableAvailabilityZonesForLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          DisableAvailabilityZonesForLoadBalancerResponse'
            Core.<$> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisableAvailabilityZonesForLoadBalancer

instance
  Core.NFData
    DisableAvailabilityZonesForLoadBalancer

instance
  Core.ToHeaders
    DisableAvailabilityZonesForLoadBalancer
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DisableAvailabilityZonesForLoadBalancer
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisableAvailabilityZonesForLoadBalancer
  where
  toQuery DisableAvailabilityZonesForLoadBalancer' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DisableAvailabilityZonesForLoadBalancer" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "AvailabilityZones"
          Core.=: Core.toQueryList "member" availabilityZones
      ]

-- | Contains the output for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newDisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
  { -- | The remaining Availability Zones for the load balancer.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableAvailabilityZonesForLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'disableAvailabilityZonesForLoadBalancerResponse_availabilityZones' - The remaining Availability Zones for the load balancer.
--
-- 'httpStatus', 'disableAvailabilityZonesForLoadBalancerResponse_httpStatus' - The response's http status code.
newDisableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableAvailabilityZonesForLoadBalancerResponse
newDisableAvailabilityZonesForLoadBalancerResponse
  pHttpStatus_ =
    DisableAvailabilityZonesForLoadBalancerResponse'
      { availabilityZones =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The remaining Availability Zones for the load balancer.
disableAvailabilityZonesForLoadBalancerResponse_availabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse (Core.Maybe [Core.Text])
disableAvailabilityZonesForLoadBalancerResponse_availabilityZones = Lens.lens (\DisableAvailabilityZonesForLoadBalancerResponse' {availabilityZones} -> availabilityZones) (\s@DisableAvailabilityZonesForLoadBalancerResponse' {} a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
disableAvailabilityZonesForLoadBalancerResponse_httpStatus :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse Core.Int
disableAvailabilityZonesForLoadBalancerResponse_httpStatus = Lens.lens (\DisableAvailabilityZonesForLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DisableAvailabilityZonesForLoadBalancerResponse' {} a -> s {httpStatus = a} :: DisableAvailabilityZonesForLoadBalancerResponse)

instance
  Core.NFData
    DisableAvailabilityZonesForLoadBalancerResponse
