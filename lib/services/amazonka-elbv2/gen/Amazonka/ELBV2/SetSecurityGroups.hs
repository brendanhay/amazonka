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
-- Module      : Amazonka.ELBV2.SetSecurityGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ELBV2.SetSecurityGroups
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text,
    -- | The IDs of the security groups.
    securityGroups :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
setSecurityGroups_securityGroups = Lens.lens (\SetSecurityGroups' {securityGroups} -> securityGroups) (\s@SetSecurityGroups' {} a -> s {securityGroups = a} :: SetSecurityGroups) Prelude.. Lens.coerced

instance Core.AWSRequest SetSecurityGroups where
  type
    AWSResponse SetSecurityGroups =
      SetSecurityGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetSecurityGroupsResult"
      ( \s h x ->
          SetSecurityGroupsResponse'
            Prelude.<$> ( x Data..@? "SecurityGroupIds"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSecurityGroups where
  hashWithSalt _salt SetSecurityGroups' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` securityGroups

instance Prelude.NFData SetSecurityGroups where
  rnf SetSecurityGroups' {..} =
    Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf securityGroups

instance Data.ToHeaders SetSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetSecurityGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery SetSecurityGroups where
  toQuery SetSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetSecurityGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Data.=: loadBalancerArn,
        "SecurityGroups"
          Data.=: Data.toQueryList "member" securityGroups
      ]

-- | /See:/ 'newSetSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { -- | The IDs of the security groups associated with the load balancer.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
setSecurityGroupsResponse_securityGroupIds = Lens.lens (\SetSecurityGroupsResponse' {securityGroupIds} -> securityGroupIds) (\s@SetSecurityGroupsResponse' {} a -> s {securityGroupIds = a} :: SetSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
setSecurityGroupsResponse_httpStatus :: Lens.Lens' SetSecurityGroupsResponse Prelude.Int
setSecurityGroupsResponse_httpStatus = Lens.lens (\SetSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@SetSecurityGroupsResponse' {} a -> s {httpStatus = a} :: SetSecurityGroupsResponse)

instance Prelude.NFData SetSecurityGroupsResponse where
  rnf SetSecurityGroupsResponse' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf httpStatus
