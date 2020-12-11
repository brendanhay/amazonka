{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachClassicLinkVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the instance has been unlinked, the VPC security groups are no longer associated with it. An instance is automatically unlinked from a VPC when it's stopped.
module Network.AWS.EC2.DetachClassicLinkVPC
  ( -- * Creating a request
    DetachClassicLinkVPC (..),
    mkDetachClassicLinkVPC,

    -- ** Request lenses
    dclvDryRun,
    dclvInstanceId,
    dclvVPCId,

    -- * Destructuring the response
    DetachClassicLinkVPCResponse (..),
    mkDetachClassicLinkVPCResponse,

    -- ** Response lenses
    dclvrsReturn,
    dclvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachClassicLinkVPC' smart constructor.
data DetachClassicLinkVPC = DetachClassicLinkVPC'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachClassicLinkVPC' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance to unlink from the VPC.
-- * 'vpcId' - The ID of the VPC to which the instance is linked.
mkDetachClassicLinkVPC ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  DetachClassicLinkVPC
mkDetachClassicLinkVPC pInstanceId_ pVPCId_ =
  DetachClassicLinkVPC'
    { dryRun = Lude.Nothing,
      instanceId = pInstanceId_,
      vpcId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvDryRun :: Lens.Lens' DetachClassicLinkVPC (Lude.Maybe Lude.Bool)
dclvDryRun = Lens.lens (dryRun :: DetachClassicLinkVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DetachClassicLinkVPC)
{-# DEPRECATED dclvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance to unlink from the VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvInstanceId :: Lens.Lens' DetachClassicLinkVPC Lude.Text
dclvInstanceId = Lens.lens (instanceId :: DetachClassicLinkVPC -> Lude.Text) (\s a -> s {instanceId = a} :: DetachClassicLinkVPC)
{-# DEPRECATED dclvInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the VPC to which the instance is linked.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvVPCId :: Lens.Lens' DetachClassicLinkVPC Lude.Text
dclvVPCId = Lens.lens (vpcId :: DetachClassicLinkVPC -> Lude.Text) (\s a -> s {vpcId = a} :: DetachClassicLinkVPC)
{-# DEPRECATED dclvVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest DetachClassicLinkVPC where
  type Rs DetachClassicLinkVPC = DetachClassicLinkVPCResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DetachClassicLinkVPCResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachClassicLinkVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachClassicLinkVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachClassicLinkVPC where
  toQuery DetachClassicLinkVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachClassicLinkVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkDetachClassicLinkVPCResponse' smart constructor.
data DetachClassicLinkVPCResponse = DetachClassicLinkVPCResponse'
  { return ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DetachClassicLinkVPCResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkDetachClassicLinkVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachClassicLinkVPCResponse
mkDetachClassicLinkVPCResponse pResponseStatus_ =
  DetachClassicLinkVPCResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvrsReturn :: Lens.Lens' DetachClassicLinkVPCResponse (Lude.Maybe Lude.Bool)
dclvrsReturn = Lens.lens (return :: DetachClassicLinkVPCResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DetachClassicLinkVPCResponse)
{-# DEPRECATED dclvrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclvrsResponseStatus :: Lens.Lens' DetachClassicLinkVPCResponse Lude.Int
dclvrsResponseStatus = Lens.lens (responseStatus :: DetachClassicLinkVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachClassicLinkVPCResponse)
{-# DEPRECATED dclvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
