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
-- Module      : Network.AWS.ELBv2.SetSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified security groups with the specified Application
-- Load Balancer. The specified security groups override the previously
-- associated security groups.
--
-- You can\'t specify a security group for a Network Load Balancer or
-- Gateway Load Balancer.
module Network.AWS.ELBv2.SetSecurityGroups
  ( -- * Creating a Request
    SetSecurityGroups (..),
    newSetSecurityGroups,

    -- * Request Lenses
    setSecurityGroups_loadBalancerArn,
    setSecurityGroups_securityGroups,

    -- * Destructuring the Response
    SetSecurityGroupsResponse (..),
    newSetSecurityGroupsResponse,

    -- * Response Lenses
    setSecurityGroupsResponse_securityGroupIds,
    setSecurityGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Text,
    -- | The IDs of the security groups.
    securityGroups :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'setSecurityGroups_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'securityGroups', 'setSecurityGroups_securityGroups' - The IDs of the security groups.
newSetSecurityGroups ::
  -- | 'loadBalancerArn'
  Core.Text ->
  SetSecurityGroups
newSetSecurityGroups pLoadBalancerArn_ =
  SetSecurityGroups'
    { loadBalancerArn =
        pLoadBalancerArn_,
      securityGroups = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
setSecurityGroups_loadBalancerArn :: Lens.Lens' SetSecurityGroups Core.Text
setSecurityGroups_loadBalancerArn = Lens.lens (\SetSecurityGroups' {loadBalancerArn} -> loadBalancerArn) (\s@SetSecurityGroups' {} a -> s {loadBalancerArn = a} :: SetSecurityGroups)

-- | The IDs of the security groups.
setSecurityGroups_securityGroups :: Lens.Lens' SetSecurityGroups [Core.Text]
setSecurityGroups_securityGroups = Lens.lens (\SetSecurityGroups' {securityGroups} -> securityGroups) (\s@SetSecurityGroups' {} a -> s {securityGroups = a} :: SetSecurityGroups) Core.. Lens._Coerce

instance Core.AWSRequest SetSecurityGroups where
  type
    AWSResponse SetSecurityGroups =
      SetSecurityGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetSecurityGroupsResult"
      ( \s h x ->
          SetSecurityGroupsResponse'
            Core.<$> ( x Core..@? "SecurityGroupIds" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetSecurityGroups

instance Core.NFData SetSecurityGroups

instance Core.ToHeaders SetSecurityGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetSecurityGroups where
  toPath = Core.const "/"

instance Core.ToQuery SetSecurityGroups where
  toQuery SetSecurityGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetSecurityGroups" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "SecurityGroups"
          Core.=: Core.toQueryList "member" securityGroups
      ]

-- | /See:/ 'newSetSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { -- | The IDs of the security groups associated with the load balancer.
    securityGroupIds :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'setSecurityGroupsResponse_securityGroupIds' - The IDs of the security groups associated with the load balancer.
--
-- 'httpStatus', 'setSecurityGroupsResponse_httpStatus' - The response's http status code.
newSetSecurityGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetSecurityGroupsResponse
newSetSecurityGroupsResponse pHttpStatus_ =
  SetSecurityGroupsResponse'
    { securityGroupIds =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the security groups associated with the load balancer.
setSecurityGroupsResponse_securityGroupIds :: Lens.Lens' SetSecurityGroupsResponse (Core.Maybe [Core.Text])
setSecurityGroupsResponse_securityGroupIds = Lens.lens (\SetSecurityGroupsResponse' {securityGroupIds} -> securityGroupIds) (\s@SetSecurityGroupsResponse' {} a -> s {securityGroupIds = a} :: SetSecurityGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
setSecurityGroupsResponse_httpStatus :: Lens.Lens' SetSecurityGroupsResponse Core.Int
setSecurityGroupsResponse_httpStatus = Lens.lens (\SetSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@SetSecurityGroupsResponse' {} a -> s {httpStatus = a} :: SetSecurityGroupsResponse)

instance Core.NFData SetSecurityGroupsResponse
