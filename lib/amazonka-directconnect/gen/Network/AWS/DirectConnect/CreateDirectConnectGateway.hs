{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Direct Connect gateway, which is an intermediate object that enables you to connect a set of virtual interfaces and virtual private gateways. A Direct Connect gateway is global and visible in any AWS Region after it is created. The virtual interfaces and virtual private gateways that are connected through a Direct Connect gateway can be in different AWS Regions. This enables you to connect to a VPC in any Region, regardless of the Region in which the virtual interfaces are located, and pass traffic between them.
module Network.AWS.DirectConnect.CreateDirectConnectGateway
  ( -- * Creating a request
    CreateDirectConnectGateway (..),
    mkCreateDirectConnectGateway,

    -- ** Request lenses
    cdcgAmazonSideASN,
    cdcgDirectConnectGatewayName,

    -- * Destructuring the response
    CreateDirectConnectGatewayResponse (..),
    mkCreateDirectConnectGatewayResponse,

    -- ** Response lenses
    cdcgrsDirectConnectGateway,
    cdcgrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDirectConnectGateway' smart constructor.
data CreateDirectConnectGateway = CreateDirectConnectGateway'
  { -- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
    amazonSideASN :: Lude.Maybe Lude.Integer,
    -- | The name of the Direct Connect gateway.
    directConnectGatewayName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectConnectGateway' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
-- * 'directConnectGatewayName' - The name of the Direct Connect gateway.
mkCreateDirectConnectGateway ::
  -- | 'directConnectGatewayName'
  Lude.Text ->
  CreateDirectConnectGateway
mkCreateDirectConnectGateway pDirectConnectGatewayName_ =
  CreateDirectConnectGateway'
    { amazonSideASN = Lude.Nothing,
      directConnectGatewayName = pDirectConnectGatewayName_
    }

-- | The autonomous system number (ASN) for Border Gateway Protocol (BGP) to be configured on the Amazon side of the connection. The ASN must be in the private range of 64,512 to 65,534 or 4,200,000,000 to 4,294,967,294. The default is 64512.
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgAmazonSideASN :: Lens.Lens' CreateDirectConnectGateway (Lude.Maybe Lude.Integer)
cdcgAmazonSideASN = Lens.lens (amazonSideASN :: CreateDirectConnectGateway -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: CreateDirectConnectGateway)
{-# DEPRECATED cdcgAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | The name of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgDirectConnectGatewayName :: Lens.Lens' CreateDirectConnectGateway Lude.Text
cdcgDirectConnectGatewayName = Lens.lens (directConnectGatewayName :: CreateDirectConnectGateway -> Lude.Text) (\s a -> s {directConnectGatewayName = a} :: CreateDirectConnectGateway)
{-# DEPRECATED cdcgDirectConnectGatewayName "Use generic-lens or generic-optics with 'directConnectGatewayName' instead." #-}

instance Lude.AWSRequest CreateDirectConnectGateway where
  type
    Rs CreateDirectConnectGateway =
      CreateDirectConnectGatewayResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayResponse'
            Lude.<$> (x Lude..?> "directConnectGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDirectConnectGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.CreateDirectConnectGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDirectConnectGateway where
  toJSON CreateDirectConnectGateway' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("amazonSideAsn" Lude..=) Lude.<$> amazonSideASN,
            Lude.Just
              ("directConnectGatewayName" Lude..= directConnectGatewayName)
          ]
      )

instance Lude.ToPath CreateDirectConnectGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDirectConnectGateway where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDirectConnectGatewayResponse' smart constructor.
data CreateDirectConnectGatewayResponse = CreateDirectConnectGatewayResponse'
  { -- | The Direct Connect gateway.
    directConnectGateway :: Lude.Maybe DirectConnectGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectConnectGatewayResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGateway' - The Direct Connect gateway.
-- * 'responseStatus' - The response status code.
mkCreateDirectConnectGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDirectConnectGatewayResponse
mkCreateDirectConnectGatewayResponse pResponseStatus_ =
  CreateDirectConnectGatewayResponse'
    { directConnectGateway =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgrsDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayResponse (Lude.Maybe DirectConnectGateway)
cdcgrsDirectConnectGateway = Lens.lens (directConnectGateway :: CreateDirectConnectGatewayResponse -> Lude.Maybe DirectConnectGateway) (\s a -> s {directConnectGateway = a} :: CreateDirectConnectGatewayResponse)
{-# DEPRECATED cdcgrsDirectConnectGateway "Use generic-lens or generic-optics with 'directConnectGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgrsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayResponse Lude.Int
cdcgrsResponseStatus = Lens.lens (responseStatus :: CreateDirectConnectGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDirectConnectGatewayResponse)
{-# DEPRECATED cdcgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
