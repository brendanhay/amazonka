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
-- Module      : Network.AWS.ELBv2.SetIpAddressType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the type of IP addresses used by the subnets of the specified
-- Application Load Balancer or Network Load Balancer.
module Network.AWS.ELBv2.SetIpAddressType
  ( -- * Creating a Request
    SetIpAddressType (..),
    newSetIpAddressType,

    -- * Request Lenses
    setIpAddressType_loadBalancerArn,
    setIpAddressType_ipAddressType,

    -- * Destructuring the Response
    SetIpAddressTypeResponse (..),
    newSetIpAddressTypeResponse,

    -- * Response Lenses
    setIpAddressTypeResponse_ipAddressType,
    setIpAddressTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetIpAddressType' smart constructor.
data SetIpAddressType = SetIpAddressType'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Text,
    -- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses)
    -- and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers
    -- must use @ipv4@. You can’t specify @dualstack@ for a load balancer with
    -- a UDP or TCP_UDP listener.
    ipAddressType :: IpAddressType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetIpAddressType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'setIpAddressType_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'ipAddressType', 'setIpAddressType_ipAddressType' - The IP address type. The possible values are @ipv4@ (for IPv4 addresses)
-- and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers
-- must use @ipv4@. You can’t specify @dualstack@ for a load balancer with
-- a UDP or TCP_UDP listener.
newSetIpAddressType ::
  -- | 'loadBalancerArn'
  Core.Text ->
  -- | 'ipAddressType'
  IpAddressType ->
  SetIpAddressType
newSetIpAddressType pLoadBalancerArn_ pIpAddressType_ =
  SetIpAddressType'
    { loadBalancerArn =
        pLoadBalancerArn_,
      ipAddressType = pIpAddressType_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
setIpAddressType_loadBalancerArn :: Lens.Lens' SetIpAddressType Core.Text
setIpAddressType_loadBalancerArn = Lens.lens (\SetIpAddressType' {loadBalancerArn} -> loadBalancerArn) (\s@SetIpAddressType' {} a -> s {loadBalancerArn = a} :: SetIpAddressType)

-- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses)
-- and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers
-- must use @ipv4@. You can’t specify @dualstack@ for a load balancer with
-- a UDP or TCP_UDP listener.
setIpAddressType_ipAddressType :: Lens.Lens' SetIpAddressType IpAddressType
setIpAddressType_ipAddressType = Lens.lens (\SetIpAddressType' {ipAddressType} -> ipAddressType) (\s@SetIpAddressType' {} a -> s {ipAddressType = a} :: SetIpAddressType)

instance Core.AWSRequest SetIpAddressType where
  type
    AWSResponse SetIpAddressType =
      SetIpAddressTypeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetIpAddressTypeResult"
      ( \s h x ->
          SetIpAddressTypeResponse'
            Core.<$> (x Core..@? "IpAddressType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetIpAddressType

instance Core.NFData SetIpAddressType

instance Core.ToHeaders SetIpAddressType where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetIpAddressType where
  toPath = Core.const "/"

instance Core.ToQuery SetIpAddressType where
  toQuery SetIpAddressType' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("SetIpAddressType" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "IpAddressType" Core.=: ipAddressType
      ]

-- | /See:/ 'newSetIpAddressTypeResponse' smart constructor.
data SetIpAddressTypeResponse = SetIpAddressTypeResponse'
  { -- | The IP address type.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetIpAddressTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'setIpAddressTypeResponse_ipAddressType' - The IP address type.
--
-- 'httpStatus', 'setIpAddressTypeResponse_httpStatus' - The response's http status code.
newSetIpAddressTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetIpAddressTypeResponse
newSetIpAddressTypeResponse pHttpStatus_ =
  SetIpAddressTypeResponse'
    { ipAddressType =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP address type.
setIpAddressTypeResponse_ipAddressType :: Lens.Lens' SetIpAddressTypeResponse (Core.Maybe IpAddressType)
setIpAddressTypeResponse_ipAddressType = Lens.lens (\SetIpAddressTypeResponse' {ipAddressType} -> ipAddressType) (\s@SetIpAddressTypeResponse' {} a -> s {ipAddressType = a} :: SetIpAddressTypeResponse)

-- | The response's http status code.
setIpAddressTypeResponse_httpStatus :: Lens.Lens' SetIpAddressTypeResponse Core.Int
setIpAddressTypeResponse_httpStatus = Lens.lens (\SetIpAddressTypeResponse' {httpStatus} -> httpStatus) (\s@SetIpAddressTypeResponse' {} a -> s {httpStatus = a} :: SetIpAddressTypeResponse)

instance Core.NFData SetIpAddressTypeResponse
