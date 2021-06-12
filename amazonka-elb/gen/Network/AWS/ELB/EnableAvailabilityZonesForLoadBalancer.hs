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
-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified Availability Zones to the set of Availability Zones
-- for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use
-- AttachLoadBalancerToSubnets.
--
-- The load balancer evenly distributes requests across all its registered
-- Availability Zones that contain instances. For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
  ( -- * Creating a Request
    EnableAvailabilityZonesForLoadBalancer (..),
    newEnableAvailabilityZonesForLoadBalancer,

    -- * Request Lenses
    enableAvailabilityZonesForLoadBalancer_loadBalancerName,
    enableAvailabilityZonesForLoadBalancer_availabilityZones,

    -- * Destructuring the Response
    EnableAvailabilityZonesForLoadBalancerResponse (..),
    newEnableAvailabilityZonesForLoadBalancerResponse,

    -- * Response Lenses
    enableAvailabilityZonesForLoadBalancerResponse_availabilityZones,
    enableAvailabilityZonesForLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newEnableAvailabilityZonesForLoadBalancer' smart constructor.
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The Availability Zones. These must be in the same region as the load
    -- balancer.
    availabilityZones :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAvailabilityZonesForLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'enableAvailabilityZonesForLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'availabilityZones', 'enableAvailabilityZonesForLoadBalancer_availabilityZones' - The Availability Zones. These must be in the same region as the load
-- balancer.
newEnableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  EnableAvailabilityZonesForLoadBalancer
newEnableAvailabilityZonesForLoadBalancer
  pLoadBalancerName_ =
    EnableAvailabilityZonesForLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        availabilityZones = Core.mempty
      }

-- | The name of the load balancer.
enableAvailabilityZonesForLoadBalancer_loadBalancerName :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer Core.Text
enableAvailabilityZonesForLoadBalancer_loadBalancerName = Lens.lens (\EnableAvailabilityZonesForLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@EnableAvailabilityZonesForLoadBalancer' {} a -> s {loadBalancerName = a} :: EnableAvailabilityZonesForLoadBalancer)

-- | The Availability Zones. These must be in the same region as the load
-- balancer.
enableAvailabilityZonesForLoadBalancer_availabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer [Core.Text]
enableAvailabilityZonesForLoadBalancer_availabilityZones = Lens.lens (\EnableAvailabilityZonesForLoadBalancer' {availabilityZones} -> availabilityZones) (\s@EnableAvailabilityZonesForLoadBalancer' {} a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancer) Core.. Lens._Coerce

instance
  Core.AWSRequest
    EnableAvailabilityZonesForLoadBalancer
  where
  type
    AWSResponse
      EnableAvailabilityZonesForLoadBalancer =
      EnableAvailabilityZonesForLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EnableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          EnableAvailabilityZonesForLoadBalancerResponse'
            Core.<$> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    EnableAvailabilityZonesForLoadBalancer

instance
  Core.NFData
    EnableAvailabilityZonesForLoadBalancer

instance
  Core.ToHeaders
    EnableAvailabilityZonesForLoadBalancer
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    EnableAvailabilityZonesForLoadBalancer
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    EnableAvailabilityZonesForLoadBalancer
  where
  toQuery EnableAvailabilityZonesForLoadBalancer' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "EnableAvailabilityZonesForLoadBalancer" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "AvailabilityZones"
          Core.=: Core.toQueryList "member" availabilityZones
      ]

-- | Contains the output of EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newEnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
  { -- | The updated list of Availability Zones for the load balancer.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableAvailabilityZonesForLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'enableAvailabilityZonesForLoadBalancerResponse_availabilityZones' - The updated list of Availability Zones for the load balancer.
--
-- 'httpStatus', 'enableAvailabilityZonesForLoadBalancerResponse_httpStatus' - The response's http status code.
newEnableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableAvailabilityZonesForLoadBalancerResponse
newEnableAvailabilityZonesForLoadBalancerResponse
  pHttpStatus_ =
    EnableAvailabilityZonesForLoadBalancerResponse'
      { availabilityZones =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated list of Availability Zones for the load balancer.
enableAvailabilityZonesForLoadBalancerResponse_availabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse (Core.Maybe [Core.Text])
enableAvailabilityZonesForLoadBalancerResponse_availabilityZones = Lens.lens (\EnableAvailabilityZonesForLoadBalancerResponse' {availabilityZones} -> availabilityZones) (\s@EnableAvailabilityZonesForLoadBalancerResponse' {} a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
enableAvailabilityZonesForLoadBalancerResponse_httpStatus :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse Core.Int
enableAvailabilityZonesForLoadBalancerResponse_httpStatus = Lens.lens (\EnableAvailabilityZonesForLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@EnableAvailabilityZonesForLoadBalancerResponse' {} a -> s {httpStatus = a} :: EnableAvailabilityZonesForLoadBalancerResponse)

instance
  Core.NFData
    EnableAvailabilityZonesForLoadBalancerResponse
