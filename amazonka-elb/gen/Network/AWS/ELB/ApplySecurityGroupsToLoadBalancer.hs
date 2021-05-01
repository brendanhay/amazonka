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
-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
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

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
applySecurityGroupsToLoadBalancer_securityGroups = Lens.lens (\ApplySecurityGroupsToLoadBalancer' {securityGroups} -> securityGroups) (\s@ApplySecurityGroupsToLoadBalancer' {} a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancer) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    ApplySecurityGroupsToLoadBalancer
  where
  type
    Rs ApplySecurityGroupsToLoadBalancer =
      ApplySecurityGroupsToLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ApplySecurityGroupsToLoadBalancerResult"
      ( \s h x ->
          ApplySecurityGroupsToLoadBalancerResponse'
            Prelude.<$> ( x Prelude..@? "SecurityGroups"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ApplySecurityGroupsToLoadBalancer

instance
  Prelude.NFData
    ApplySecurityGroupsToLoadBalancer

instance
  Prelude.ToHeaders
    ApplySecurityGroupsToLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ApplySecurityGroupsToLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ApplySecurityGroupsToLoadBalancer
  where
  toQuery ApplySecurityGroupsToLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ApplySecurityGroupsToLoadBalancer" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "SecurityGroups"
          Prelude.=: Prelude.toQueryList "member" securityGroups
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
applySecurityGroupsToLoadBalancerResponse_securityGroups = Lens.lens (\ApplySecurityGroupsToLoadBalancerResponse' {securityGroups} -> securityGroups) (\s@ApplySecurityGroupsToLoadBalancerResponse' {} a -> s {securityGroups = a} :: ApplySecurityGroupsToLoadBalancerResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
applySecurityGroupsToLoadBalancerResponse_httpStatus :: Lens.Lens' ApplySecurityGroupsToLoadBalancerResponse Prelude.Int
applySecurityGroupsToLoadBalancerResponse_httpStatus = Lens.lens (\ApplySecurityGroupsToLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@ApplySecurityGroupsToLoadBalancerResponse' {} a -> s {httpStatus = a} :: ApplySecurityGroupsToLoadBalancerResponse)

instance
  Prelude.NFData
    ApplySecurityGroupsToLoadBalancerResponse
