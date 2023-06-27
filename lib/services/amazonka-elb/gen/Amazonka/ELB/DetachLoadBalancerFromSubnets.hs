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
-- Module      : Amazonka.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified subnets from the set of configured subnets for the
-- load balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load
-- balancer in the removed subnet go into the @OutOfService@ state. Then,
-- the load balancer balances the traffic among the remaining routable
-- subnets.
module Amazonka.ELB.DetachLoadBalancerFromSubnets
  ( -- * Creating a Request
    DetachLoadBalancerFromSubnets (..),
    newDetachLoadBalancerFromSubnets,

    -- * Request Lenses
    detachLoadBalancerFromSubnets_loadBalancerName,
    detachLoadBalancerFromSubnets_subnets,

    -- * Destructuring the Response
    DetachLoadBalancerFromSubnetsResponse (..),
    newDetachLoadBalancerFromSubnetsResponse,

    -- * Response Lenses
    detachLoadBalancerFromSubnetsResponse_subnets,
    detachLoadBalancerFromSubnetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DetachLoadBalancerFromSubnets.
--
-- /See:/ 'newDetachLoadBalancerFromSubnets' smart constructor.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The IDs of the subnets.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachLoadBalancerFromSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'detachLoadBalancerFromSubnets_loadBalancerName' - The name of the load balancer.
--
-- 'subnets', 'detachLoadBalancerFromSubnets_subnets' - The IDs of the subnets.
newDetachLoadBalancerFromSubnets ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DetachLoadBalancerFromSubnets
newDetachLoadBalancerFromSubnets pLoadBalancerName_ =
  DetachLoadBalancerFromSubnets'
    { loadBalancerName =
        pLoadBalancerName_,
      subnets = Prelude.mempty
    }

-- | The name of the load balancer.
detachLoadBalancerFromSubnets_loadBalancerName :: Lens.Lens' DetachLoadBalancerFromSubnets Prelude.Text
detachLoadBalancerFromSubnets_loadBalancerName = Lens.lens (\DetachLoadBalancerFromSubnets' {loadBalancerName} -> loadBalancerName) (\s@DetachLoadBalancerFromSubnets' {} a -> s {loadBalancerName = a} :: DetachLoadBalancerFromSubnets)

-- | The IDs of the subnets.
detachLoadBalancerFromSubnets_subnets :: Lens.Lens' DetachLoadBalancerFromSubnets [Prelude.Text]
detachLoadBalancerFromSubnets_subnets = Lens.lens (\DetachLoadBalancerFromSubnets' {subnets} -> subnets) (\s@DetachLoadBalancerFromSubnets' {} a -> s {subnets = a} :: DetachLoadBalancerFromSubnets) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DetachLoadBalancerFromSubnets
  where
  type
    AWSResponse DetachLoadBalancerFromSubnets =
      DetachLoadBalancerFromSubnetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerFromSubnetsResult"
      ( \s h x ->
          DetachLoadBalancerFromSubnetsResponse'
            Prelude.<$> ( x
                            Data..@? "Subnets"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachLoadBalancerFromSubnets
  where
  hashWithSalt _salt DetachLoadBalancerFromSubnets' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData DetachLoadBalancerFromSubnets where
  rnf DetachLoadBalancerFromSubnets' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToHeaders DetachLoadBalancerFromSubnets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachLoadBalancerFromSubnets where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachLoadBalancerFromSubnets where
  toQuery DetachLoadBalancerFromSubnets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DetachLoadBalancerFromSubnets" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "Subnets" Data.=: Data.toQueryList "member" subnets
      ]

-- | Contains the output of DetachLoadBalancerFromSubnets.
--
-- /See:/ 'newDetachLoadBalancerFromSubnetsResponse' smart constructor.
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
  { -- | The IDs of the remaining subnets for the load balancer.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachLoadBalancerFromSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnets', 'detachLoadBalancerFromSubnetsResponse_subnets' - The IDs of the remaining subnets for the load balancer.
--
-- 'httpStatus', 'detachLoadBalancerFromSubnetsResponse_httpStatus' - The response's http status code.
newDetachLoadBalancerFromSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachLoadBalancerFromSubnetsResponse
newDetachLoadBalancerFromSubnetsResponse pHttpStatus_ =
  DetachLoadBalancerFromSubnetsResponse'
    { subnets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the remaining subnets for the load balancer.
detachLoadBalancerFromSubnetsResponse_subnets :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse (Prelude.Maybe [Prelude.Text])
detachLoadBalancerFromSubnetsResponse_subnets = Lens.lens (\DetachLoadBalancerFromSubnetsResponse' {subnets} -> subnets) (\s@DetachLoadBalancerFromSubnetsResponse' {} a -> s {subnets = a} :: DetachLoadBalancerFromSubnetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detachLoadBalancerFromSubnetsResponse_httpStatus :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse Prelude.Int
detachLoadBalancerFromSubnetsResponse_httpStatus = Lens.lens (\DetachLoadBalancerFromSubnetsResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancerFromSubnetsResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancerFromSubnetsResponse)

instance
  Prelude.NFData
    DetachLoadBalancerFromSubnetsResponse
  where
  rnf DetachLoadBalancerFromSubnetsResponse' {..} =
    Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf httpStatus
