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
-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more subnets to the set of configured subnets for the
-- specified load balancer.
--
-- The load balancer evenly distributes requests across all registered
-- subnets. For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-manage-subnets.html Add or Remove Subnets for Your Load Balancer in a VPC>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.AttachLoadBalancerToSubnets
  ( -- * Creating a Request
    AttachLoadBalancerToSubnets (..),
    newAttachLoadBalancerToSubnets,

    -- * Request Lenses
    attachLoadBalancerToSubnets_loadBalancerName,
    attachLoadBalancerToSubnets_subnets,

    -- * Destructuring the Response
    AttachLoadBalancerToSubnetsResponse (..),
    newAttachLoadBalancerToSubnetsResponse,

    -- * Response Lenses
    attachLoadBalancerToSubnetsResponse_subnets,
    attachLoadBalancerToSubnetsResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachLoaBalancerToSubnets.
--
-- /See:/ 'newAttachLoadBalancerToSubnets' smart constructor.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The IDs of the subnets to add. You can add only one subnet per
    -- Availability Zone.
    subnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerToSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'attachLoadBalancerToSubnets_loadBalancerName' - The name of the load balancer.
--
-- 'subnets', 'attachLoadBalancerToSubnets_subnets' - The IDs of the subnets to add. You can add only one subnet per
-- Availability Zone.
newAttachLoadBalancerToSubnets ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  AttachLoadBalancerToSubnets
newAttachLoadBalancerToSubnets pLoadBalancerName_ =
  AttachLoadBalancerToSubnets'
    { loadBalancerName =
        pLoadBalancerName_,
      subnets = Prelude.mempty
    }

-- | The name of the load balancer.
attachLoadBalancerToSubnets_loadBalancerName :: Lens.Lens' AttachLoadBalancerToSubnets Prelude.Text
attachLoadBalancerToSubnets_loadBalancerName = Lens.lens (\AttachLoadBalancerToSubnets' {loadBalancerName} -> loadBalancerName) (\s@AttachLoadBalancerToSubnets' {} a -> s {loadBalancerName = a} :: AttachLoadBalancerToSubnets)

-- | The IDs of the subnets to add. You can add only one subnet per
-- Availability Zone.
attachLoadBalancerToSubnets_subnets :: Lens.Lens' AttachLoadBalancerToSubnets [Prelude.Text]
attachLoadBalancerToSubnets_subnets = Lens.lens (\AttachLoadBalancerToSubnets' {subnets} -> subnets) (\s@AttachLoadBalancerToSubnets' {} a -> s {subnets = a} :: AttachLoadBalancerToSubnets) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AttachLoadBalancerToSubnets
  where
  type
    Rs AttachLoadBalancerToSubnets =
      AttachLoadBalancerToSubnetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancerToSubnetsResult"
      ( \s h x ->
          AttachLoadBalancerToSubnetsResponse'
            Prelude.<$> ( x Prelude..@? "Subnets" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachLoadBalancerToSubnets

instance Prelude.NFData AttachLoadBalancerToSubnets

instance
  Prelude.ToHeaders
    AttachLoadBalancerToSubnets
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AttachLoadBalancerToSubnets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachLoadBalancerToSubnets where
  toQuery AttachLoadBalancerToSubnets' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AttachLoadBalancerToSubnets" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "Subnets"
          Prelude.=: Prelude.toQueryList "member" subnets
      ]

-- | Contains the output of AttachLoadBalancerToSubnets.
--
-- /See:/ 'newAttachLoadBalancerToSubnetsResponse' smart constructor.
data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse'
  { -- | The IDs of the subnets attached to the load balancer.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerToSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnets', 'attachLoadBalancerToSubnetsResponse_subnets' - The IDs of the subnets attached to the load balancer.
--
-- 'httpStatus', 'attachLoadBalancerToSubnetsResponse_httpStatus' - The response's http status code.
newAttachLoadBalancerToSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachLoadBalancerToSubnetsResponse
newAttachLoadBalancerToSubnetsResponse pHttpStatus_ =
  AttachLoadBalancerToSubnetsResponse'
    { subnets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the subnets attached to the load balancer.
attachLoadBalancerToSubnetsResponse_subnets :: Lens.Lens' AttachLoadBalancerToSubnetsResponse (Prelude.Maybe [Prelude.Text])
attachLoadBalancerToSubnetsResponse_subnets = Lens.lens (\AttachLoadBalancerToSubnetsResponse' {subnets} -> subnets) (\s@AttachLoadBalancerToSubnetsResponse' {} a -> s {subnets = a} :: AttachLoadBalancerToSubnetsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
attachLoadBalancerToSubnetsResponse_httpStatus :: Lens.Lens' AttachLoadBalancerToSubnetsResponse Prelude.Int
attachLoadBalancerToSubnetsResponse_httpStatus = Lens.lens (\AttachLoadBalancerToSubnetsResponse' {httpStatus} -> httpStatus) (\s@AttachLoadBalancerToSubnetsResponse' {} a -> s {httpStatus = a} :: AttachLoadBalancerToSubnetsResponse)

instance
  Prelude.NFData
    AttachLoadBalancerToSubnetsResponse
