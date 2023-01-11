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
-- Module      : Amazonka.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more security groups with your load balancer in a
-- virtual private cloud (VPC). The specified security groups override the
-- previously associated security groups.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-groups.html#elb-vpc-security-groups Security Groups for Load Balancers in a VPC>
-- in the /Classic Load Balancers Guide/.
module Amazonka.ELB.ApplySecurityGroupsToLoadBalancer
  ( -- * Creating a Request
    ApplySecurityGroupsToLoadBalancer (..),
    newApplySecurityGroupsToLoadBalancer,

    -- * Request Lenses
    applySecurityGroupsToLoadBalancer_loadBalancerName,
    applySecurityGroupsToLoadBalancer_securityGroups,

    -- * Destructuring the Response
    ApplySecurityGroupsToLoadBalancerResponse (..),
    newApplySecurityGroupsToLoadBalancerResponse,

    -- * Response Lenses
    applySecurityGroupsToLoadBalancerResponse_securityGroups,
    applySecurityGroupsToLoadBalancerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'newApplySecurityGroupsToLoadBalancer' smart constructor.
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The IDs of the security groups to associate with the load balancer. Note
    -- that you cannot specify the name of the security group.
    securityGroups :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplySecurityGroupsToLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'applySecurityGroupsToLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'securityGroups', 'applySecurityGroupsToLoadBalancer_securityGroups' - The IDs of the security groups to associate with the load balancer. Note
-- that you cannot specify the name of the security group.
newApplySecurityGroupsToLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  ApplySecurityGroupsToLoadBalancer
newApplySecurityGroupsToLoadBalancer
  pLoadBalancerName_ =
    ApplySecurityGroupsToLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        securityGroups = Prelude.mempty
      }

-- | The name of the load balancer.
applySecurityGroupsToLoadBalancer_loadBalancerName :: Lens.Lens' ApplySecurityGroupsToLoadBalancer Prelude.Text
applySecurityGroupsToLoadBalancer_loadBalancerName = Lens.lens (\ApplySecurityGroupsToLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@ApplySecurityGroupsToLoadBalancer' {} a -> s {loadBalancerName = a} :: ApplySecurityGroupsToLoadBalancer)

-- | The IDs of the security groups to associate with the load balancer. Note
-- that you cannot specify the name of the security group.
applySecurityGroupsToLoadBalancer_securityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancer [Prelude.Text]
applySecurityGroupsToLoadBalancer_securityGroups = Lens.lens (\ApplySecurityGroupsToLoadBalancer' {securityGroups} -> securityGroups) (\s@ApplySecurityGroupsToLoadBalancer' {} a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    ApplySecurityGroupsToLoadBalancer
  where
  type
    AWSResponse ApplySecurityGroupsToLoadBalancer =
      ApplySecurityGroupsToLoadBalancerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ApplySecurityGroupsToLoadBalancerResult"
      ( \s h x ->
          ApplySecurityGroupsToLoadBalancerResponse'
            Prelude.<$> ( x Data..@? "SecurityGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ApplySecurityGroupsToLoadBalancer
  where
  hashWithSalt
    _salt
    ApplySecurityGroupsToLoadBalancer' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` securityGroups

instance
  Prelude.NFData
    ApplySecurityGroupsToLoadBalancer
  where
  rnf ApplySecurityGroupsToLoadBalancer' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf securityGroups

instance
  Data.ToHeaders
    ApplySecurityGroupsToLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ApplySecurityGroupsToLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ApplySecurityGroupsToLoadBalancer
  where
  toQuery ApplySecurityGroupsToLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ApplySecurityGroupsToLoadBalancer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "SecurityGroups"
          Data.=: Data.toQueryList "member" securityGroups
      ]

-- | Contains the output of ApplySecurityGroupsToLoadBalancer.
--
-- /See:/ 'newApplySecurityGroupsToLoadBalancerResponse' smart constructor.
data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse'
  { -- | The IDs of the security groups associated with the load balancer.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplySecurityGroupsToLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'applySecurityGroupsToLoadBalancerResponse_securityGroups' - The IDs of the security groups associated with the load balancer.
--
-- 'httpStatus', 'applySecurityGroupsToLoadBalancerResponse_httpStatus' - The response's http status code.
newApplySecurityGroupsToLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ApplySecurityGroupsToLoadBalancerResponse
newApplySecurityGroupsToLoadBalancerResponse
  pHttpStatus_ =
    ApplySecurityGroupsToLoadBalancerResponse'
      { securityGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The IDs of the security groups associated with the load balancer.
applySecurityGroupsToLoadBalancerResponse_securityGroups :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse (Prelude.Maybe [Prelude.Text])
applySecurityGroupsToLoadBalancerResponse_securityGroups = Lens.lens (\ApplySecurityGroupsToLoadBalancerResponse' {securityGroups} -> securityGroups) (\s@ApplySecurityGroupsToLoadBalancerResponse' {} a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
applySecurityGroupsToLoadBalancerResponse_httpStatus :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse Prelude.Int
applySecurityGroupsToLoadBalancerResponse_httpStatus = Lens.lens (\ApplySecurityGroupsToLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@ApplySecurityGroupsToLoadBalancerResponse' {} a -> s {httpStatus = a} :: ApplySecurityGroupsToLoadBalancerResponse)

instance
  Prelude.NFData
    ApplySecurityGroupsToLoadBalancerResponse
  where
  rnf ApplySecurityGroupsToLoadBalancerResponse' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf httpStatus
