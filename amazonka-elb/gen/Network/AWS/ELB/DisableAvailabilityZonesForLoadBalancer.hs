{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newDisableAvailabilityZonesForLoadBalancer' smart constructor.
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The Availability Zones.
    availabilityZones :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisableAvailabilityZonesForLoadBalancer
newDisableAvailabilityZonesForLoadBalancer
  pLoadBalancerName_ =
    DisableAvailabilityZonesForLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        availabilityZones = Prelude.mempty
      }

-- | The name of the load balancer.
disableAvailabilityZonesForLoadBalancer_loadBalancerName :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer Prelude.Text
disableAvailabilityZonesForLoadBalancer_loadBalancerName = Lens.lens (\DisableAvailabilityZonesForLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DisableAvailabilityZonesForLoadBalancer' {} a -> s {loadBalancerName = a} :: DisableAvailabilityZonesForLoadBalancer)

-- | The Availability Zones.
disableAvailabilityZonesForLoadBalancer_availabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer [Prelude.Text]
disableAvailabilityZonesForLoadBalancer_availabilityZones = Lens.lens (\DisableAvailabilityZonesForLoadBalancer' {availabilityZones} -> availabilityZones) (\s@DisableAvailabilityZonesForLoadBalancer' {} a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancer) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    DisableAvailabilityZonesForLoadBalancer
  where
  type
    Rs DisableAvailabilityZonesForLoadBalancer =
      DisableAvailabilityZonesForLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          DisableAvailabilityZonesForLoadBalancerResponse'
            Prelude.<$> ( x Prelude..@? "AvailabilityZones"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisableAvailabilityZonesForLoadBalancer

instance
  Prelude.NFData
    DisableAvailabilityZonesForLoadBalancer

instance
  Prelude.ToHeaders
    DisableAvailabilityZonesForLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DisableAvailabilityZonesForLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisableAvailabilityZonesForLoadBalancer
  where
  toQuery DisableAvailabilityZonesForLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DisableAvailabilityZonesForLoadBalancer" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "AvailabilityZones"
          Prelude.=: Prelude.toQueryList "member" availabilityZones
      ]

-- | Contains the output for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newDisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
  { -- | The remaining Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisableAvailabilityZonesForLoadBalancerResponse
newDisableAvailabilityZonesForLoadBalancerResponse
  pHttpStatus_ =
    DisableAvailabilityZonesForLoadBalancerResponse'
      { availabilityZones =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The remaining Availability Zones for the load balancer.
disableAvailabilityZonesForLoadBalancerResponse_availabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse (Prelude.Maybe [Prelude.Text])
disableAvailabilityZonesForLoadBalancerResponse_availabilityZones = Lens.lens (\DisableAvailabilityZonesForLoadBalancerResponse' {availabilityZones} -> availabilityZones) (\s@DisableAvailabilityZonesForLoadBalancerResponse' {} a -> s {availabilityZones = a} :: DisableAvailabilityZonesForLoadBalancerResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
disableAvailabilityZonesForLoadBalancerResponse_httpStatus :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse Prelude.Int
disableAvailabilityZonesForLoadBalancerResponse_httpStatus = Lens.lens (\DisableAvailabilityZonesForLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DisableAvailabilityZonesForLoadBalancerResponse' {} a -> s {httpStatus = a} :: DisableAvailabilityZonesForLoadBalancerResponse)

instance
  Prelude.NFData
    DisableAvailabilityZonesForLoadBalancerResponse
