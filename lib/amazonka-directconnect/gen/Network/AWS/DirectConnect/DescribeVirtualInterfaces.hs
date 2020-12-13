{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays all virtual interfaces for an AWS account. Virtual interfaces deleted fewer than 15 minutes before you make the request are also returned. If you specify a connection ID, only the virtual interfaces associated with the connection are returned. If you specify a virtual interface ID, then only a single virtual interface is returned.
--
-- A virtual interface (VLAN) transmits the traffic between the AWS Direct Connect location and the customer network.
module Network.AWS.DirectConnect.DescribeVirtualInterfaces
  ( -- * Creating a request
    DescribeVirtualInterfaces (..),
    mkDescribeVirtualInterfaces,

    -- ** Request lenses
    dviConnectionId,
    dviVirtualInterfaceId,

    -- * Destructuring the response
    DescribeVirtualInterfacesResponse (..),
    mkDescribeVirtualInterfacesResponse,

    -- ** Response lenses
    dvisrsVirtualInterfaces,
    dvisrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVirtualInterfaces' smart constructor.
data DescribeVirtualInterfaces = DescribeVirtualInterfaces'
  { -- | The ID of the connection.
    connectionId :: Lude.Maybe Lude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVirtualInterfaces' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkDescribeVirtualInterfaces ::
  DescribeVirtualInterfaces
mkDescribeVirtualInterfaces =
  DescribeVirtualInterfaces'
    { connectionId = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviConnectionId :: Lens.Lens' DescribeVirtualInterfaces (Lude.Maybe Lude.Text)
dviConnectionId = Lens.lens (connectionId :: DescribeVirtualInterfaces -> Lude.Maybe Lude.Text) (\s a -> s {connectionId = a} :: DescribeVirtualInterfaces)
{-# DEPRECATED dviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dviVirtualInterfaceId :: Lens.Lens' DescribeVirtualInterfaces (Lude.Maybe Lude.Text)
dviVirtualInterfaceId = Lens.lens (virtualInterfaceId :: DescribeVirtualInterfaces -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: DescribeVirtualInterfaces)
{-# DEPRECATED dviVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest DescribeVirtualInterfaces where
  type
    Rs DescribeVirtualInterfaces =
      DescribeVirtualInterfacesResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVirtualInterfacesResponse'
            Lude.<$> (x Lude..?> "virtualInterfaces" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVirtualInterfaces where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeVirtualInterfaces" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVirtualInterfaces where
  toJSON DescribeVirtualInterfaces' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("connectionId" Lude..=) Lude.<$> connectionId,
            ("virtualInterfaceId" Lude..=) Lude.<$> virtualInterfaceId
          ]
      )

instance Lude.ToPath DescribeVirtualInterfaces where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVirtualInterfaces where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeVirtualInterfacesResponse' smart constructor.
data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse'
  { -- | The virtual interfaces
    virtualInterfaces :: Lude.Maybe [VirtualInterface],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVirtualInterfacesResponse' with the minimum fields required to make a request.
--
-- * 'virtualInterfaces' - The virtual interfaces
-- * 'responseStatus' - The response status code.
mkDescribeVirtualInterfacesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVirtualInterfacesResponse
mkDescribeVirtualInterfacesResponse pResponseStatus_ =
  DescribeVirtualInterfacesResponse'
    { virtualInterfaces =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The virtual interfaces
--
-- /Note:/ Consider using 'virtualInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvisrsVirtualInterfaces :: Lens.Lens' DescribeVirtualInterfacesResponse (Lude.Maybe [VirtualInterface])
dvisrsVirtualInterfaces = Lens.lens (virtualInterfaces :: DescribeVirtualInterfacesResponse -> Lude.Maybe [VirtualInterface]) (\s a -> s {virtualInterfaces = a} :: DescribeVirtualInterfacesResponse)
{-# DEPRECATED dvisrsVirtualInterfaces "Use generic-lens or generic-optics with 'virtualInterfaces' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvisrsResponseStatus :: Lens.Lens' DescribeVirtualInterfacesResponse Lude.Int
dvisrsResponseStatus = Lens.lens (responseStatus :: DescribeVirtualInterfacesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVirtualInterfacesResponse)
{-# DEPRECATED dvisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
