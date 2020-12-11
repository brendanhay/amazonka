{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateDefaultVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a default VPC with a size @/16@ IPv4 CIDR block and a default subnet in each Availability Zone. For more information about the components of a default VPC, see <https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html Default VPC and Default Subnets> in the /Amazon Virtual Private Cloud User Guide/ . You cannot specify the components of the default VPC yourself.
--
-- If you deleted your previous default VPC, you can create a default VPC. You cannot have more than one default VPC per Region.
-- If your account supports EC2-Classic, you cannot use this action to create a default VPC in a Region that supports EC2-Classic. If you want a default VPC in a Region that supports EC2-Classic, see "I really want a default VPC for my existing EC2 account. Is that possible?" in the <http://aws.amazon.com/vpc/faqs/#Default_VPCs Default VPCs FAQ> .
module Network.AWS.EC2.CreateDefaultVPC
  ( -- * Creating a request
    CreateDefaultVPC (..),
    mkCreateDefaultVPC,

    -- ** Request lenses
    cdvDryRun,

    -- * Destructuring the response
    CreateDefaultVPCResponse (..),
    mkCreateDefaultVPCResponse,

    -- ** Response lenses
    cdvrsVPC,
    cdvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDefaultVPC' smart constructor.
newtype CreateDefaultVPC = CreateDefaultVPC'
  { dryRun ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDefaultVPC' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateDefaultVPC ::
  CreateDefaultVPC
mkCreateDefaultVPC = CreateDefaultVPC' {dryRun = Lude.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDryRun :: Lens.Lens' CreateDefaultVPC (Lude.Maybe Lude.Bool)
cdvDryRun = Lens.lens (dryRun :: CreateDefaultVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateDefaultVPC)
{-# DEPRECATED cdvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateDefaultVPC where
  type Rs CreateDefaultVPC = CreateDefaultVPCResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateDefaultVPCResponse'
            Lude.<$> (x Lude..@? "vpc") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDefaultVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDefaultVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDefaultVPC where
  toQuery CreateDefaultVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDefaultVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateDefaultVPCResponse' smart constructor.
data CreateDefaultVPCResponse = CreateDefaultVPCResponse'
  { vpc ::
      Lude.Maybe VPC,
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

-- | Creates a value of 'CreateDefaultVPCResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpc' - Information about the VPC.
mkCreateDefaultVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDefaultVPCResponse
mkCreateDefaultVPCResponse pResponseStatus_ =
  CreateDefaultVPCResponse'
    { vpc = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvrsVPC :: Lens.Lens' CreateDefaultVPCResponse (Lude.Maybe VPC)
cdvrsVPC = Lens.lens (vpc :: CreateDefaultVPCResponse -> Lude.Maybe VPC) (\s a -> s {vpc = a} :: CreateDefaultVPCResponse)
{-# DEPRECATED cdvrsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvrsResponseStatus :: Lens.Lens' CreateDefaultVPCResponse Lude.Int
cdvrsResponseStatus = Lens.lens (responseStatus :: CreateDefaultVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDefaultVPCResponse)
{-# DEPRECATED cdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
