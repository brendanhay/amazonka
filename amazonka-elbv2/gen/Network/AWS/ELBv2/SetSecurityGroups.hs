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

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text,
    -- | The IDs of the security groups.
    securityGroups :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SetSecurityGroups
newSetSecurityGroups pLoadBalancerArn_ =
  SetSecurityGroups'
    { loadBalancerArn =
        pLoadBalancerArn_,
      securityGroups = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
setSecurityGroups_loadBalancerArn :: Lens.Lens' SetSecurityGroups Prelude.Text
setSecurityGroups_loadBalancerArn = Lens.lens (\SetSecurityGroups' {loadBalancerArn} -> loadBalancerArn) (\s@SetSecurityGroups' {} a -> s {loadBalancerArn = a} :: SetSecurityGroups)

-- | The IDs of the security groups.
setSecurityGroups_securityGroups :: Lens.Lens' SetSecurityGroups [Prelude.Text]
setSecurityGroups_securityGroups = Lens.lens (\SetSecurityGroups' {securityGroups} -> securityGroups) (\s@SetSecurityGroups' {} a -> s {securityGroups = a} :: SetSecurityGroups) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest SetSecurityGroups where
  type Rs SetSecurityGroups = SetSecurityGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetSecurityGroupsResult"
      ( \s h x ->
          SetSecurityGroupsResponse'
            Prelude.<$> ( x Prelude..@? "SecurityGroupIds"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSecurityGroups

instance Prelude.NFData SetSecurityGroups

instance Prelude.ToHeaders SetSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetSecurityGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetSecurityGroups where
  toQuery SetSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetSecurityGroups" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Prelude.=: loadBalancerArn,
        "SecurityGroups"
          Prelude.=: Prelude.toQueryList "member" securityGroups
      ]

-- | /See:/ 'newSetSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { -- | The IDs of the security groups associated with the load balancer.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  SetSecurityGroupsResponse
newSetSecurityGroupsResponse pHttpStatus_ =
  SetSecurityGroupsResponse'
    { securityGroupIds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the security groups associated with the load balancer.
setSecurityGroupsResponse_securityGroupIds :: Lens.Lens' SetSecurityGroupsResponse (Prelude.Maybe [Prelude.Text])
setSecurityGroupsResponse_securityGroupIds = Lens.lens (\SetSecurityGroupsResponse' {securityGroupIds} -> securityGroupIds) (\s@SetSecurityGroupsResponse' {} a -> s {securityGroupIds = a} :: SetSecurityGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
setSecurityGroupsResponse_httpStatus :: Lens.Lens' SetSecurityGroupsResponse Prelude.Int
setSecurityGroupsResponse_httpStatus = Lens.lens (\SetSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@SetSecurityGroupsResponse' {} a -> s {httpStatus = a} :: SetSecurityGroupsResponse)

instance Prelude.NFData SetSecurityGroupsResponse
