{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an internet gateway for use with a VPC. After creating the internet gateway, you attach it to a VPC using 'AttachInternetGateway' .
--
-- For more information about your VPC and internet gateway, see the <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide> .
module Network.AWS.EC2.CreateInternetGateway
  ( -- * Creating a request
    CreateInternetGateway (..),
    mkCreateInternetGateway,

    -- ** Request lenses
    cigTagSpecifications,
    cigDryRun,

    -- * Destructuring the response
    CreateInternetGatewayResponse (..),
    mkCreateInternetGatewayResponse,

    -- ** Response lenses
    cigrsInternetGateway,
    cigrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInternetGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to assign to the internet gateway.
mkCreateInternetGateway ::
  CreateInternetGateway
mkCreateInternetGateway =
  CreateInternetGateway'
    { tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The tags to assign to the internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigTagSpecifications :: Lens.Lens' CreateInternetGateway (Lude.Maybe [TagSpecification])
cigTagSpecifications = Lens.lens (tagSpecifications :: CreateInternetGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateInternetGateway)
{-# DEPRECATED cigTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigDryRun :: Lens.Lens' CreateInternetGateway (Lude.Maybe Lude.Bool)
cigDryRun = Lens.lens (dryRun :: CreateInternetGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateInternetGateway)
{-# DEPRECATED cigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateInternetGateway where
  type Rs CreateInternetGateway = CreateInternetGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateInternetGatewayResponse'
            Lude.<$> (x Lude..@? "internetGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInternetGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateInternetGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInternetGateway where
  toQuery CreateInternetGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateInternetGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { internetGateway ::
      Lude.Maybe InternetGateway,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInternetGatewayResponse' with the minimum fields required to make a request.
--
-- * 'internetGateway' - Information about the internet gateway.
-- * 'responseStatus' - The response status code.
mkCreateInternetGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInternetGatewayResponse
mkCreateInternetGatewayResponse pResponseStatus_ =
  CreateInternetGatewayResponse'
    { internetGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the internet gateway.
--
-- /Note:/ Consider using 'internetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrsInternetGateway :: Lens.Lens' CreateInternetGatewayResponse (Lude.Maybe InternetGateway)
cigrsInternetGateway = Lens.lens (internetGateway :: CreateInternetGatewayResponse -> Lude.Maybe InternetGateway) (\s a -> s {internetGateway = a} :: CreateInternetGatewayResponse)
{-# DEPRECATED cigrsInternetGateway "Use generic-lens or generic-optics with 'internetGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cigrsResponseStatus :: Lens.Lens' CreateInternetGatewayResponse Lude.Int
cigrsResponseStatus = Lens.lens (responseStatus :: CreateInternetGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInternetGatewayResponse)
{-# DEPRECATED cigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
