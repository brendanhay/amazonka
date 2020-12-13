{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual private gateways owned by the AWS account.
--
-- You can create one or more AWS Direct Connect private virtual interfaces linked to a virtual private gateway.
module Network.AWS.DirectConnect.DescribeVirtualGateways
  ( -- * Creating a request
    DescribeVirtualGateways (..),
    mkDescribeVirtualGateways,

    -- * Destructuring the response
    DescribeVirtualGatewaysResponse (..),
    mkDescribeVirtualGatewaysResponse,

    -- ** Response lenses
    dvgrsVirtualGateways,
    dvgrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVirtualGateways' smart constructor.
data DescribeVirtualGateways = DescribeVirtualGateways'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVirtualGateways' with the minimum fields required to make a request.
mkDescribeVirtualGateways ::
  DescribeVirtualGateways
mkDescribeVirtualGateways = DescribeVirtualGateways'

instance Lude.AWSRequest DescribeVirtualGateways where
  type Rs DescribeVirtualGateways = DescribeVirtualGatewaysResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVirtualGatewaysResponse'
            Lude.<$> (x Lude..?> "virtualGateways" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVirtualGateways where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeVirtualGateways" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVirtualGateways where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeVirtualGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVirtualGateways where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeVirtualGatewaysResponse' smart constructor.
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'
  { -- | The virtual private gateways.
    virtualGateways :: Lude.Maybe [VirtualGateway],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVirtualGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'virtualGateways' - The virtual private gateways.
-- * 'responseStatus' - The response status code.
mkDescribeVirtualGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVirtualGatewaysResponse
mkDescribeVirtualGatewaysResponse pResponseStatus_ =
  DescribeVirtualGatewaysResponse'
    { virtualGateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The virtual private gateways.
--
-- /Note:/ Consider using 'virtualGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrsVirtualGateways :: Lens.Lens' DescribeVirtualGatewaysResponse (Lude.Maybe [VirtualGateway])
dvgrsVirtualGateways = Lens.lens (virtualGateways :: DescribeVirtualGatewaysResponse -> Lude.Maybe [VirtualGateway]) (\s a -> s {virtualGateways = a} :: DescribeVirtualGatewaysResponse)
{-# DEPRECATED dvgrsVirtualGateways "Use generic-lens or generic-optics with 'virtualGateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgrsResponseStatus :: Lens.Lens' DescribeVirtualGatewaysResponse Lude.Int
dvgrsResponseStatus = Lens.lens (responseStatus :: DescribeVirtualGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVirtualGatewaysResponse)
{-# DEPRECATED dvgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
