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
-- Module      : Network.AWS.ELB.CreateLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Classic Load Balancer.
--
-- You can add listeners, security groups, subnets, and tags when you
-- create your load balancer, or you can add them later using
-- CreateLoadBalancerListeners, ApplySecurityGroupsToLoadBalancer,
-- AttachLoadBalancerToSubnets, and AddTags.
--
-- To describe your current load balancers, see DescribeLoadBalancers. When
-- you are finished with a load balancer, you can delete it using
-- DeleteLoadBalancer.
--
-- You can create up to 20 load balancers per region per account. You can
-- request an increase for the number of load balancers for your account.
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html Limits for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.CreateLoadBalancer
  ( -- * Creating a Request
    CreateLoadBalancer (..),
    newCreateLoadBalancer,

    -- * Request Lenses
    createLoadBalancer_availabilityZones,
    createLoadBalancer_scheme,
    createLoadBalancer_securityGroups,
    createLoadBalancer_tags,
    createLoadBalancer_subnets,
    createLoadBalancer_loadBalancerName,
    createLoadBalancer_listeners,

    -- * Destructuring the Response
    CreateLoadBalancerResponse (..),
    newCreateLoadBalancerResponse,

    -- * Response Lenses
    createLoadBalancerResponse_dNSName,
    createLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancer.
--
-- /See:/ 'newCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | One or more Availability Zones from the same region as the load
    -- balancer.
    --
    -- You must specify at least one Availability Zone.
    --
    -- You can add more Availability Zones after you create the load balancer
    -- using EnableAvailabilityZonesForLoadBalancer.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The type of a load balancer. Valid only for load balancers in a VPC.
    --
    -- By default, Elastic Load Balancing creates an Internet-facing load
    -- balancer with a DNS name that resolves to public IP addresses. For more
    -- information about Internet-facing and Internal load balancers, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme>
    -- in the /Elastic Load Balancing User Guide/.
    --
    -- Specify @internal@ to create a load balancer with a DNS name that
    -- resolves to private IP addresses.
    scheme :: Core.Maybe Core.Text,
    -- | The IDs of the security groups to assign to the load balancer.
    securityGroups :: Core.Maybe [Core.Text],
    -- | A list of tags to assign to the load balancer.
    --
    -- For more information about tagging your load balancer, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer>
    -- in the /Classic Load Balancers Guide/.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The IDs of the subnets in your VPC to attach to the load balancer.
    -- Specify one subnet per Availability Zone specified in
    -- @AvailabilityZones@.
    subnets :: Core.Maybe [Core.Text],
    -- | The name of the load balancer.
    --
    -- This name must be unique within your set of load balancers for the
    -- region, must have a maximum of 32 characters, must contain only
    -- alphanumeric characters or hyphens, and cannot begin or end with a
    -- hyphen.
    loadBalancerName :: Core.Text,
    -- | The listeners.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
    -- in the /Classic Load Balancers Guide/.
    listeners :: [Listener]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'createLoadBalancer_availabilityZones' - One or more Availability Zones from the same region as the load
-- balancer.
--
-- You must specify at least one Availability Zone.
--
-- You can add more Availability Zones after you create the load balancer
-- using EnableAvailabilityZonesForLoadBalancer.
--
-- 'scheme', 'createLoadBalancer_scheme' - The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load
-- balancer with a DNS name that resolves to public IP addresses. For more
-- information about Internet-facing and Internal load balancers, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme>
-- in the /Elastic Load Balancing User Guide/.
--
-- Specify @internal@ to create a load balancer with a DNS name that
-- resolves to private IP addresses.
--
-- 'securityGroups', 'createLoadBalancer_securityGroups' - The IDs of the security groups to assign to the load balancer.
--
-- 'tags', 'createLoadBalancer_tags' - A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
--
-- 'subnets', 'createLoadBalancer_subnets' - The IDs of the subnets in your VPC to attach to the load balancer.
-- Specify one subnet per Availability Zone specified in
-- @AvailabilityZones@.
--
-- 'loadBalancerName', 'createLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the
-- region, must have a maximum of 32 characters, must contain only
-- alphanumeric characters or hyphens, and cannot begin or end with a
-- hyphen.
--
-- 'listeners', 'createLoadBalancer_listeners' - The listeners.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
newCreateLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  CreateLoadBalancer
newCreateLoadBalancer pLoadBalancerName_ =
  CreateLoadBalancer'
    { availabilityZones =
        Core.Nothing,
      scheme = Core.Nothing,
      securityGroups = Core.Nothing,
      tags = Core.Nothing,
      subnets = Core.Nothing,
      loadBalancerName = pLoadBalancerName_,
      listeners = Core.mempty
    }

-- | One or more Availability Zones from the same region as the load
-- balancer.
--
-- You must specify at least one Availability Zone.
--
-- You can add more Availability Zones after you create the load balancer
-- using EnableAvailabilityZonesForLoadBalancer.
createLoadBalancer_availabilityZones :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Core.Text])
createLoadBalancer_availabilityZones = Lens.lens (\CreateLoadBalancer' {availabilityZones} -> availabilityZones) (\s@CreateLoadBalancer' {} a -> s {availabilityZones = a} :: CreateLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The type of a load balancer. Valid only for load balancers in a VPC.
--
-- By default, Elastic Load Balancing creates an Internet-facing load
-- balancer with a DNS name that resolves to public IP addresses. For more
-- information about Internet-facing and Internal load balancers, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/how-elastic-load-balancing-works.html#load-balancer-scheme Load Balancer Scheme>
-- in the /Elastic Load Balancing User Guide/.
--
-- Specify @internal@ to create a load balancer with a DNS name that
-- resolves to private IP addresses.
createLoadBalancer_scheme :: Lens.Lens' CreateLoadBalancer (Core.Maybe Core.Text)
createLoadBalancer_scheme = Lens.lens (\CreateLoadBalancer' {scheme} -> scheme) (\s@CreateLoadBalancer' {} a -> s {scheme = a} :: CreateLoadBalancer)

-- | The IDs of the security groups to assign to the load balancer.
createLoadBalancer_securityGroups :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Core.Text])
createLoadBalancer_securityGroups = Lens.lens (\CreateLoadBalancer' {securityGroups} -> securityGroups) (\s@CreateLoadBalancer' {} a -> s {securityGroups = a} :: CreateLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | A list of tags to assign to the load balancer.
--
-- For more information about tagging your load balancer, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
createLoadBalancer_tags :: Lens.Lens' CreateLoadBalancer (Core.Maybe (Core.NonEmpty Tag))
createLoadBalancer_tags = Lens.lens (\CreateLoadBalancer' {tags} -> tags) (\s@CreateLoadBalancer' {} a -> s {tags = a} :: CreateLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the subnets in your VPC to attach to the load balancer.
-- Specify one subnet per Availability Zone specified in
-- @AvailabilityZones@.
createLoadBalancer_subnets :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Core.Text])
createLoadBalancer_subnets = Lens.lens (\CreateLoadBalancer' {subnets} -> subnets) (\s@CreateLoadBalancer' {} a -> s {subnets = a} :: CreateLoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The name of the load balancer.
--
-- This name must be unique within your set of load balancers for the
-- region, must have a maximum of 32 characters, must contain only
-- alphanumeric characters or hyphens, and cannot begin or end with a
-- hyphen.
createLoadBalancer_loadBalancerName :: Lens.Lens' CreateLoadBalancer Core.Text
createLoadBalancer_loadBalancerName = Lens.lens (\CreateLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancer' {} a -> s {loadBalancerName = a} :: CreateLoadBalancer)

-- | The listeners.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
createLoadBalancer_listeners :: Lens.Lens' CreateLoadBalancer [Listener]
createLoadBalancer_listeners = Lens.lens (\CreateLoadBalancer' {listeners} -> listeners) (\s@CreateLoadBalancer' {} a -> s {listeners = a} :: CreateLoadBalancer) Core.. Lens._Coerce

instance Core.AWSRequest CreateLoadBalancer where
  type
    AWSResponse CreateLoadBalancer =
      CreateLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Core.<$> (x Core..@? "DNSName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLoadBalancer

instance Core.NFData CreateLoadBalancer

instance Core.ToHeaders CreateLoadBalancer where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateLoadBalancer" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> availabilityZones
            ),
        "Scheme" Core.=: scheme,
        "SecurityGroups"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> securityGroups),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> tags),
        "Subnets"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> subnets),
        "LoadBalancerName" Core.=: loadBalancerName,
        "Listeners"
          Core.=: Core.toQueryList "member" listeners
      ]

-- | Contains the output for CreateLoadBalancer.
--
-- /See:/ 'newCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { -- | The DNS name of the load balancer.
    dNSName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dNSName', 'createLoadBalancerResponse_dNSName' - The DNS name of the load balancer.
--
-- 'httpStatus', 'createLoadBalancerResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLoadBalancerResponse
newCreateLoadBalancerResponse pHttpStatus_ =
  CreateLoadBalancerResponse'
    { dNSName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The DNS name of the load balancer.
createLoadBalancerResponse_dNSName :: Lens.Lens' CreateLoadBalancerResponse (Core.Maybe Core.Text)
createLoadBalancerResponse_dNSName = Lens.lens (\CreateLoadBalancerResponse' {dNSName} -> dNSName) (\s@CreateLoadBalancerResponse' {} a -> s {dNSName = a} :: CreateLoadBalancerResponse)

-- | The response's http status code.
createLoadBalancerResponse_httpStatus :: Lens.Lens' CreateLoadBalancerResponse Core.Int
createLoadBalancerResponse_httpStatus = Lens.lens (\CreateLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerResponse)

instance Core.NFData CreateLoadBalancerResponse
