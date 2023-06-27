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
-- Module      : Amazonka.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ELB.EnableAvailabilityZonesForLoadBalancer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newEnableAvailabilityZonesForLoadBalancer' smart constructor.
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The Availability Zones. These must be in the same region as the load
    -- balancer.
    availabilityZones :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  EnableAvailabilityZonesForLoadBalancer
newEnableAvailabilityZonesForLoadBalancer
  pLoadBalancerName_ =
    EnableAvailabilityZonesForLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        availabilityZones = Prelude.mempty
      }

-- | The name of the load balancer.
enableAvailabilityZonesForLoadBalancer_loadBalancerName :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer Prelude.Text
enableAvailabilityZonesForLoadBalancer_loadBalancerName = Lens.lens (\EnableAvailabilityZonesForLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@EnableAvailabilityZonesForLoadBalancer' {} a -> s {loadBalancerName = a} :: EnableAvailabilityZonesForLoadBalancer)

-- | The Availability Zones. These must be in the same region as the load
-- balancer.
enableAvailabilityZonesForLoadBalancer_availabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer [Prelude.Text]
enableAvailabilityZonesForLoadBalancer_availabilityZones = Lens.lens (\EnableAvailabilityZonesForLoadBalancer' {availabilityZones} -> availabilityZones) (\s@EnableAvailabilityZonesForLoadBalancer' {} a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    EnableAvailabilityZonesForLoadBalancer
  where
  type
    AWSResponse
      EnableAvailabilityZonesForLoadBalancer =
      EnableAvailabilityZonesForLoadBalancerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "EnableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          EnableAvailabilityZonesForLoadBalancerResponse'
            Prelude.<$> ( x
                            Data..@? "AvailabilityZones"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    EnableAvailabilityZonesForLoadBalancer
  where
  hashWithSalt
    _salt
    EnableAvailabilityZonesForLoadBalancer' {..} =
      _salt
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` availabilityZones

instance
  Prelude.NFData
    EnableAvailabilityZonesForLoadBalancer
  where
  rnf EnableAvailabilityZonesForLoadBalancer' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf availabilityZones

instance
  Data.ToHeaders
    EnableAvailabilityZonesForLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    EnableAvailabilityZonesForLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    EnableAvailabilityZonesForLoadBalancer
  where
  toQuery EnableAvailabilityZonesForLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "EnableAvailabilityZonesForLoadBalancer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "AvailabilityZones"
          Data.=: Data.toQueryList "member" availabilityZones
      ]

-- | Contains the output of EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'newEnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
  { -- | The updated list of Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  EnableAvailabilityZonesForLoadBalancerResponse
newEnableAvailabilityZonesForLoadBalancerResponse
  pHttpStatus_ =
    EnableAvailabilityZonesForLoadBalancerResponse'
      { availabilityZones =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated list of Availability Zones for the load balancer.
enableAvailabilityZonesForLoadBalancerResponse_availabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse (Prelude.Maybe [Prelude.Text])
enableAvailabilityZonesForLoadBalancerResponse_availabilityZones = Lens.lens (\EnableAvailabilityZonesForLoadBalancerResponse' {availabilityZones} -> availabilityZones) (\s@EnableAvailabilityZonesForLoadBalancerResponse' {} a -> s {availabilityZones = a} :: EnableAvailabilityZonesForLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
enableAvailabilityZonesForLoadBalancerResponse_httpStatus :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse Prelude.Int
enableAvailabilityZonesForLoadBalancerResponse_httpStatus = Lens.lens (\EnableAvailabilityZonesForLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@EnableAvailabilityZonesForLoadBalancerResponse' {} a -> s {httpStatus = a} :: EnableAvailabilityZonesForLoadBalancerResponse)

instance
  Prelude.NFData
    EnableAvailabilityZonesForLoadBalancerResponse
  where
  rnf
    EnableAvailabilityZonesForLoadBalancerResponse' {..} =
      Prelude.rnf availabilityZones
        `Prelude.seq` Prelude.rnf httpStatus
