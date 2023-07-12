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
-- Module      : Amazonka.ELBV2.SetIpAddressType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the type of IP addresses used by the subnets of the specified load
-- balancer.
module Amazonka.ELBV2.SetIpAddressType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetIpAddressType' smart constructor.
data SetIpAddressType = SetIpAddressType'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text,
    -- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses)
    -- and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify
    -- @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
    ipAddressType :: IpAddressType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify
-- @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
newSetIpAddressType ::
  -- | 'loadBalancerArn'
  Prelude.Text ->
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
setIpAddressType_loadBalancerArn :: Lens.Lens' SetIpAddressType Prelude.Text
setIpAddressType_loadBalancerArn = Lens.lens (\SetIpAddressType' {loadBalancerArn} -> loadBalancerArn) (\s@SetIpAddressType' {} a -> s {loadBalancerArn = a} :: SetIpAddressType)

-- | The IP address type. The possible values are @ipv4@ (for IPv4 addresses)
-- and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify
-- @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
setIpAddressType_ipAddressType :: Lens.Lens' SetIpAddressType IpAddressType
setIpAddressType_ipAddressType = Lens.lens (\SetIpAddressType' {ipAddressType} -> ipAddressType) (\s@SetIpAddressType' {} a -> s {ipAddressType = a} :: SetIpAddressType)

instance Core.AWSRequest SetIpAddressType where
  type
    AWSResponse SetIpAddressType =
      SetIpAddressTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetIpAddressTypeResult"
      ( \s h x ->
          SetIpAddressTypeResponse'
            Prelude.<$> (x Data..@? "IpAddressType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetIpAddressType where
  hashWithSalt _salt SetIpAddressType' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` ipAddressType

instance Prelude.NFData SetIpAddressType where
  rnf SetIpAddressType' {..} =
    Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf ipAddressType

instance Data.ToHeaders SetIpAddressType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetIpAddressType where
  toPath = Prelude.const "/"

instance Data.ToQuery SetIpAddressType where
  toQuery SetIpAddressType' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetIpAddressType" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Data.=: loadBalancerArn,
        "IpAddressType" Data.=: ipAddressType
      ]

-- | /See:/ 'newSetIpAddressTypeResponse' smart constructor.
data SetIpAddressTypeResponse = SetIpAddressTypeResponse'
  { -- | The IP address type.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SetIpAddressTypeResponse
newSetIpAddressTypeResponse pHttpStatus_ =
  SetIpAddressTypeResponse'
    { ipAddressType =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP address type.
setIpAddressTypeResponse_ipAddressType :: Lens.Lens' SetIpAddressTypeResponse (Prelude.Maybe IpAddressType)
setIpAddressTypeResponse_ipAddressType = Lens.lens (\SetIpAddressTypeResponse' {ipAddressType} -> ipAddressType) (\s@SetIpAddressTypeResponse' {} a -> s {ipAddressType = a} :: SetIpAddressTypeResponse)

-- | The response's http status code.
setIpAddressTypeResponse_httpStatus :: Lens.Lens' SetIpAddressTypeResponse Prelude.Int
setIpAddressTypeResponse_httpStatus = Lens.lens (\SetIpAddressTypeResponse' {httpStatus} -> httpStatus) (\s@SetIpAddressTypeResponse' {} a -> s {httpStatus = a} :: SetIpAddressTypeResponse)

instance Prelude.NFData SetIpAddressTypeResponse where
  rnf SetIpAddressTypeResponse' {..} =
    Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf httpStatus
