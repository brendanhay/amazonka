{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyVPCTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the instance tenancy attribute of the specified VPC. You can change the instance tenancy attribute of a VPC to @default@ only. You cannot change the instance tenancy attribute to @dedicated@ .
--
-- After you modify the tenancy of the VPC, any new instances that you launch into the VPC have a tenancy of @default@ , unless you specify otherwise during launch. The tenancy of any existing instances in the VPC is not affected.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyVPCTenancy
  ( -- * Creating a request
    ModifyVPCTenancy (..),
    mkModifyVPCTenancy,

    -- ** Request lenses
    mvtDryRun,
    mvtVPCId,
    mvtInstanceTenancy,

    -- * Destructuring the response
    ModifyVPCTenancyResponse (..),
    mkModifyVPCTenancyResponse,

    -- ** Response lenses
    mvtrsReturnValue,
    mvtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyVPCTenancy' smart constructor.
data ModifyVPCTenancy = ModifyVPCTenancy'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    vpcId :: Lude.Text,
    instanceTenancy :: VPCTenancy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyVPCTenancy' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceTenancy' - The instance tenancy attribute for the VPC.
-- * 'vpcId' - The ID of the VPC.
mkModifyVPCTenancy ::
  -- | 'vpcId'
  Lude.Text ->
  -- | 'instanceTenancy'
  VPCTenancy ->
  ModifyVPCTenancy
mkModifyVPCTenancy pVPCId_ pInstanceTenancy_ =
  ModifyVPCTenancy'
    { dryRun = Lude.Nothing,
      vpcId = pVPCId_,
      instanceTenancy = pInstanceTenancy_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtDryRun :: Lens.Lens' ModifyVPCTenancy (Lude.Maybe Lude.Bool)
mvtDryRun = Lens.lens (dryRun :: ModifyVPCTenancy -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyVPCTenancy)
{-# DEPRECATED mvtDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtVPCId :: Lens.Lens' ModifyVPCTenancy Lude.Text
mvtVPCId = Lens.lens (vpcId :: ModifyVPCTenancy -> Lude.Text) (\s a -> s {vpcId = a} :: ModifyVPCTenancy)
{-# DEPRECATED mvtVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The instance tenancy attribute for the VPC.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtInstanceTenancy :: Lens.Lens' ModifyVPCTenancy VPCTenancy
mvtInstanceTenancy = Lens.lens (instanceTenancy :: ModifyVPCTenancy -> VPCTenancy) (\s a -> s {instanceTenancy = a} :: ModifyVPCTenancy)
{-# DEPRECATED mvtInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

instance Lude.AWSRequest ModifyVPCTenancy where
  type Rs ModifyVPCTenancy = ModifyVPCTenancyResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyVPCTenancyResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyVPCTenancy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyVPCTenancy where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyVPCTenancy where
  toQuery ModifyVPCTenancy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyVpcTenancy" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VpcId" Lude.=: vpcId,
        "InstanceTenancy" Lude.=: instanceTenancy
      ]

-- | /See:/ 'mkModifyVPCTenancyResponse' smart constructor.
data ModifyVPCTenancyResponse = ModifyVPCTenancyResponse'
  { returnValue ::
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

-- | Creates a value of 'ModifyVPCTenancyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'returnValue' - Returns @true@ if the request succeeds; otherwise, returns an error.
mkModifyVPCTenancyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyVPCTenancyResponse
mkModifyVPCTenancyResponse pResponseStatus_ =
  ModifyVPCTenancyResponse'
    { returnValue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtrsReturnValue :: Lens.Lens' ModifyVPCTenancyResponse (Lude.Maybe Lude.Bool)
mvtrsReturnValue = Lens.lens (returnValue :: ModifyVPCTenancyResponse -> Lude.Maybe Lude.Bool) (\s a -> s {returnValue = a} :: ModifyVPCTenancyResponse)
{-# DEPRECATED mvtrsReturnValue "Use generic-lens or generic-optics with 'returnValue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvtrsResponseStatus :: Lens.Lens' ModifyVPCTenancyResponse Lude.Int
mvtrsResponseStatus = Lens.lens (responseStatus :: ModifyVPCTenancyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyVPCTenancyResponse)
{-# DEPRECATED mvtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
