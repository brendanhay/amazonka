{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves valid VPC peering authorizations that are pending for the AWS account. This operation returns all VPC peering authorizations and requests for peering. This includes those initiated and received by this account.
--
--
--     * 'CreateVpcPeeringAuthorization'
--
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--
--     * 'DeleteVpcPeeringAuthorization'
--
--
--     * 'CreateVpcPeeringConnection'
--
--
--     * 'DescribeVpcPeeringConnections'
--
--
--     * 'DeleteVpcPeeringConnection'
module Network.AWS.GameLift.DescribeVPCPeeringAuthorizations
  ( -- * Creating a request
    DescribeVPCPeeringAuthorizations (..),
    mkDescribeVPCPeeringAuthorizations,

    -- * Destructuring the response
    DescribeVPCPeeringAuthorizationsResponse (..),
    mkDescribeVPCPeeringAuthorizationsResponse,

    -- ** Response lenses
    dvpcparsVPCPeeringAuthorizations,
    dvpcparsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCPeeringAuthorizations' smart constructor.
data DescribeVPCPeeringAuthorizations = DescribeVPCPeeringAuthorizations'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringAuthorizations' with the minimum fields required to make a request.
mkDescribeVPCPeeringAuthorizations ::
  DescribeVPCPeeringAuthorizations
mkDescribeVPCPeeringAuthorizations =
  DescribeVPCPeeringAuthorizations'

instance Lude.AWSRequest DescribeVPCPeeringAuthorizations where
  type
    Rs DescribeVPCPeeringAuthorizations =
      DescribeVPCPeeringAuthorizationsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVPCPeeringAuthorizationsResponse'
            Lude.<$> (x Lude..?> "VpcPeeringAuthorizations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCPeeringAuthorizations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeVpcPeeringAuthorizations" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVPCPeeringAuthorizations where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeVPCPeeringAuthorizations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCPeeringAuthorizations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeVPCPeeringAuthorizationsResponse' smart constructor.
data DescribeVPCPeeringAuthorizationsResponse = DescribeVPCPeeringAuthorizationsResponse'
  { vpcPeeringAuthorizations ::
      Lude.Maybe
        [VPCPeeringAuthorization],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringAuthorizationsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpcPeeringAuthorizations' - A collection of objects that describe all valid VPC peering operations for the current AWS account.
mkDescribeVPCPeeringAuthorizationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCPeeringAuthorizationsResponse
mkDescribeVPCPeeringAuthorizationsResponse pResponseStatus_ =
  DescribeVPCPeeringAuthorizationsResponse'
    { vpcPeeringAuthorizations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of objects that describe all valid VPC peering operations for the current AWS account.
--
-- /Note:/ Consider using 'vpcPeeringAuthorizations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcparsVPCPeeringAuthorizations :: Lens.Lens' DescribeVPCPeeringAuthorizationsResponse (Lude.Maybe [VPCPeeringAuthorization])
dvpcparsVPCPeeringAuthorizations = Lens.lens (vpcPeeringAuthorizations :: DescribeVPCPeeringAuthorizationsResponse -> Lude.Maybe [VPCPeeringAuthorization]) (\s a -> s {vpcPeeringAuthorizations = a} :: DescribeVPCPeeringAuthorizationsResponse)
{-# DEPRECATED dvpcparsVPCPeeringAuthorizations "Use generic-lens or generic-optics with 'vpcPeeringAuthorizations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcparsResponseStatus :: Lens.Lens' DescribeVPCPeeringAuthorizationsResponse Lude.Int
dvpcparsResponseStatus = Lens.lens (responseStatus :: DescribeVPCPeeringAuthorizationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCPeeringAuthorizationsResponse)
{-# DEPRECATED dvpcparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
